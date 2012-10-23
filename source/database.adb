
--
-- Database -- Factoid and quote database manipulation task package for Allegra info-bot
--


--
-- Standard packages
with Ada.Characters.Handling;
use  Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Numerics.Float_Random;
with Ada.Strings.Unbounded;


--
-- Local library packages
with Strings;
use  Strings;


--
-- Application packages
with Auth;
with Config;
with CommandQ;
with DB;
use  DB;
with IRC;
with Log;
with OutputQ;


--
-- Request-queue package
with DatabaseQ;


package body Database is

------------------------------------------------------------------------------
--
-- Package constants
--
------------------------------------------------------------------------------

   -- This task's name, for logging purposes
   Database_Name : constant string := "Database";

   -- Length of a single message, used to break up very long responses.  Yes,
   -- this is rather shorter than IRC supports, but we sometimes add stuff to
   -- the lines, and who cares if they're max length, anyway?
   Message_Limit : constant := 256;

   -- Names of the database tables we use
   ActQuips_Tbl  : constant string := "actquips";
   Factoid_Tbl   : constant string := "factoids";
   Factstats_Tbl : constant string := "factstats";
   Quips_Tbl     : constant string := "quips";
   QuitMsg_Tbl   : constant string := "quitmsgs";
   Quotes_Tbl    : constant string := "quotes";
   UserLvl_Tbl   : constant string := "usrlevels";

------------------------------------------------------------------------------
--
-- Package exceptions
--
------------------------------------------------------------------------------

   Connect_Error : exception;

------------------------------------------------------------------------------
--
-- Package variables
--
------------------------------------------------------------------------------

   -- Random number generator, for random quote/quip selection and such
   Randoms : Ada.Numerics.Float_Random.Generator;

   -- The database request we're processing at the moment
   Request : DatabaseQ.Request_Rec;

------------------------------------------------------------------------------
--
-- Package subroutines
--
------------------------------------------------------------------------------

   -- Return true if Who matches Creator (meaning this requestor is the
   -- creator of the factoid) or if the user who sent the request has bot
   -- operator access.
   function Is_Owner_Operator (Who     : in string;
                               Creator : in string) return boolean is
   begin  -- Is_Owner_Operator
      if Creator /= To_Lower (Who) then
         if Auth.Level (Request.Requestor) < Auth.Bot_Operator_Level then
            OutputQ.Say ("I'm sorry " & Who & ", that factoid was created by " &
                         Creator & " and I can't let you change it.  Check with " &
                         Creator & " or my operator to get it changed.", Request.Destination);
            return false;
         end if;
      end if;
      return true;
   end Is_Owner_Operator;

   ---------------------------------------------------------------------------

   procedure Safe_Connect (Handle : out DB_Handle) is
   begin  -- Safe_Connect
      Connect
         (Handle   => Handle,
          Host     => Config.DB_Hostname,
          DB       => Config.DB_Name,
          Login    => Config.DB_Login,
          Password => Config.DB_Password);
   exception
      when others =>
         OutputQ.Say ("Can't connect to the factoid database.  Release the hounds!  (And tell the bot operator, please.)",
                      Request.Destination);
         raise Connect_Error;
   end Safe_Connect;

   ---------------------------------------------------------------------------

   -- Select a random entry from given database table and return it; used for
   -- quips and quit messages.  Requires the table to have a "num" column
   -- which contains the row number.
   function Random_Select (Table : in string) return string is

      use Ada.Numerics.Float_Random;

      Count  : natural;
      Data   : DB_Result;
      Handle : DB_Handle;
      Index  : positive;

   begin  -- Random_Select

      -- Connect and fetch the count of rows in the table
      Safe_Connect (Handle);
      Fetch (Handle, "count(msg)", Table, "", Data);

      -- Get the count into a local variable; return a null string if we
      -- couldn't fetch the count
      if Rows (Data) > 0 then
         Count := Get_Value (Data, 1, "count");
      else
         Count := 0;
      end if;
      if Count < 1 then
         return "";
      end if;

      -- Calculate which row we want as a random number 1..Count
      Index := (integer (float (Count) * Random (Randoms)) mod Count) + 1;

      -- Fetch the row we've randomly selected, and disconnect
      Fetch (Handle, "msg", Table, "where num=" & positive'Image (Index), Data);
      Disconnect (Handle);

      -- If we got data, return it; if not, return a null string
      if Rows (Data) > 0 then
         return Get_Value (Data, 1, "msg");
      else
         return "";
      end if;

   exception
      when Connect_Error =>
         return "";  -- message already sent

      when E : others =>
         OutputQ.Say ("*burp*  Sorry, that gave me gas:  " & Ada.Exceptions.Exception_Information (E), Request.Destination);
         Disconnect (Handle);
         return "";
   end Random_Select;

   ---------------------------------------------------------------------------

   -- Select a random quote from quotes database table and return it.  Similar
   -- to Random_Select, but returns both a quote and its attribution.
   procedure Random_Quote (Quote, Attrib : out UString) is

      use Ada.Numerics.Float_Random;

      Count  : natural;
      Data   : DB_Result;
      Handle : DB_Handle;
      Index  : positive;

   begin  -- Random_Quote

      -- Connect and fetch the count of rows in the table
      Safe_Connect (Handle);
      Fetch (Handle, "count(quote)", Quotes_Tbl, "", Data);

      -- See if we got the count back
      if Rows (Data) > 0 then
         Count := Get_Value (Data, 1, "count");
      else
         Count := 0;
      end if;

      -- If we have a count, try to fetch a quote
      if Count > 0 then

         -- Calculate which row we want as a random number 1..Count
         Index := (integer (float (Count) * Random (Randoms)) mod Count) + 1;

         -- Fetch the row we've randomly selected
         Fetch (Handle, "quote,attr", Quotes_Tbl, "where num=" & positive'Image (Index), Data);

         -- If we got a quote, disconnect from the db, and return the data values
         if Rows (Data) > 0 then
            Disconnect (Handle);
            Quote  := US (Source => Get_Value (Data, 1, "quote"));
            Attrib := US (Source => Get_Value (Data, 1, "attr"));
            return;
         end if;
      end if;

      -- If we fall through here, we either didn't get the row count, or
      -- didn't get the row we wanted.  In either case, return null strings
      -- for both the quote and attribution.
      Disconnect (Handle);
      Quote  := Null_UString;
      Attrib := Null_UString;

   exception
      when Connect_Error =>
         null;  -- message already sent

      when E : others =>
         OutputQ.Say ("Here's a quote for you:  " & Ada.Exceptions.Exception_Information (E), Request.Destination);
         Disconnect (Handle);
   end Random_Quote;

   ---------------------------------------------------------------------------

   -- Put a factoid/definition pair in the factoid database table
   procedure Set_Factoid is

      Data   : DB_Result;
      Fact   : string := S (Request.Key);
      Handle : DB_Handle;
      Hits   : natural;
      Msg    : UString;

   begin  -- Set_Factoid

      -- Connect and try to fetch the factoid, to see if it's already there
      Safe_Connect (Handle);
      Fetch (Handle, "name", Factoid_Tbl, "where name=" & Escape (To_Lower (Fact)), Data);

      -- If we got no hits on that factoid name, insert it into the table as a
      -- new entry
      Hits := Rows (Data);
      if Hits = 0 then
         Statement (Handle, "insert into " & Factoid_Tbl & " (name,value) values (" &
                    To_Lower (Escape (Fact)) & "," & Escape (S (Request.Data)) & ")");
         Statement (Handle, "insert into " & Factstats_Tbl & " (name,created,creator) values (" &
                    To_Lower (Escape (Fact)) & ",'now'," & Escape (To_Lower (S (Request.Origin))) & ")");
         OutputQ.Say ("""" & Fact & """ has been added!", Request.Destination);

      -- Factoid is already there, so tell the user that we can't do the set
      else
         if Hits = 1 then
            Msg := US ("I'm sorry, there is already a definition");
         else
            Msg := US ("I'm sorry, there are already" & natural'Image (Hits) & " definitions");
         end if;
         Msg := Msg & " for """ & Fact &
           """.  Do a reset (""no, factoid is definition"") if you want to replace ";
         if Hits = 1 then
            Msg := Msg & "it, or an add (""factoid is also definition"") to add a second definition.";
         else
            Msg := Msg & "them all, or an add (""factoid is also definition"") to add another.";
         end if;
         OutputQ.Say (Msg, Request.Destination);
      end if;

      -- Done with the db now
      Disconnect (Handle);

   exception
      when Connect_Error =>
         null;  -- message already sent

      when E : others =>
         OutputQ.Say ("That didn't set too well with me:  " & Ada.Exceptions.Exception_Information (E), Request.Destination);
         Disconnect (Handle);
   end Set_Factoid;

   ---------------------------------------------------------------------------

   -- Add another definition to an existing factoid.  Will also create a new
   -- factoid if it's not already there, since that seems a reasonable respose
   -- to that situation.
   procedure Add_Factoid is

      Data   : DB_Result;
      Fact   : string := S (Request.Key);
      Handle : DB_Handle;
      Hits   : natural;

   begin  -- Add_Factoid

      -- Connect and try to fetch the factoid, to see if it's already there
      Safe_Connect (Handle);
      Fetch (Handle, "name", Factoid_Tbl, "where name=" & Escape (To_Lower (Fact)), Data);

      -- If it's not already there, treat it as a "set"
      Hits := Rows (Data);
      if Hits = 0 then
         OutputQ.Say ("I don't know a factoid """ & """ yet, but what the heck.", Request.Destination);
         Disconnect (Handle);
         Set_Factoid;
         return;
      end if;

      -- It's already there, so add the new definition and disconnect
      Statement (Handle, "insert into " & Factoid_Tbl & " (name,value) values (" &
                 To_Lower (Escape (Fact)) & "," & Escape (S (Request.Data)) & ")");
      OutputQ.Say ("Another definition for """ & Fact & """ has been added!", Request.Destination);
      Disconnect (Handle);

   exception
      when Connect_Error =>
         null;  -- message already sent

      when E : others =>
         OutputQ.Say ("I forgot how to add ... " & Ada.Exceptions.Exception_Information (E), Request.Destination);
         Disconnect (Handle);
   end Add_Factoid;

   ---------------------------------------------------------------------------

   -- Print statistics about a single factoid
   procedure Factoid_Stats (Name : in string) is

      Data   : DB_Result;
      Handle : DB_Handle;
      Msg    : UString;

      Line_Pause : constant Duration := Duration'Value (Config.Get_Value (Config.Item_Line_Pause));

   begin  -- Factoid_Stats

      -- Connect, fetch the factoid's stats, and disconnect
      Safe_Connect (Handle);
      Fetch (Handle, "*", Factstats_Tbl, "where name=" & Escape (To_Lower (Name)), Data);
      Disconnect (Handle);

      -- If we got the stats, print them out in a nice human-readable format
      if Rows (Data) > 0 then
         OutputQ.Say ("The factoid named """ & Name & """ was created by " & Get_Value (Data, 1, "creator") &
                      " on " & Get_Value (Data, 1, "created") & ".", Request.Destination);
         delay Line_Pause;
         declare
            Count : string := Get_Value (Data, 1, "acc_count");
         begin
            if Count = "0" then
               OutputQ.Say ("It has never been accessed.", Request.Destination);
            else
               if Count = "1" then
                  Msg := US ("It has been accessed once");
               else
                  Msg := US ("It has been accessed " & Count & " times");
               end if;
               Msg := Msg & ", last by " & Get_Value (Data, 1, "acc_by") &
                 " on " & Get_Value (Data, 1, "acc_last") & ".";
               OutputQ.Say (Msg, Request.Destination);
            end if;
         end;

      -- Couldn't find that factoid, too bad
      else
         OutputQ.Say ("I can't seem to locate a factoid named """ & Name & """, sorry.", Request.Destination);
      end if;

   exception
      when Connect_Error =>
         null;  -- message already sent

      when E : others =>
         OutputQ.Say ("Statistics always confused me:  " & Ada.Exceptions.Exception_Information (E), Request.Destination);
         Disconnect (Handle);
   end Factoid_Stats;

   ---------------------------------------------------------------------------

   -- Forget a factoid by deleting its entries from the factoid table, after
   -- verifying that the requesting user is allowed to do that.
   procedure Forget_Factoid is

      Data   : DB_Result;
      Fact   : string := S (Request.Key);
      Handle : DB_Handle;
      Count  : natural;
      Msg    : UString;

   begin  -- Forget_Factoid

      -- Connect and fetch the ID of the user who created this factoid
      -- originally
      Safe_Connect (Handle);
      Fetch (Handle, "creator", Factstats_Tbl, "where name=" & Escape (To_Lower (Fact)), Data);

      -- If we didn't get anything, assume that the factoid doesn't exist;
      -- complain and return
      if Rows (Data) = 0 then
         OutputQ.Say ("I don't seem to know a factoid """ & Fact & """, so nothing was forgotten.",
                      Request.Destination);
         return;
      end if;

      -- Only process the request if made by the original creator, or by the
      -- bot operator
      if Is_Owner_Operator (S (Request.Origin), Get_Value (Data, 1, "creator")) then

         -- Find out how many definitions we're deleting, so we can report
         -- that number back to the user
         Fetch (Handle, "count(name)", Factoid_Tbl, "where name=" & Escape (To_Lower (Fact)), Data);
         if Rows (Data) > 0 then
            Count := Get_Value (Data, 1, "count");
         else
            Count := 1;  -- shouldn't happen, but if it does, 1 is a safe assumption
         end if;

         -- Delete this factoid's entries from both the factoid table and the
         -- factoid statistics table
         Statement (Handle, "delete from " & Factoid_Tbl   & " where name=" & Escape (To_Lower (Fact)));
         Statement (Handle, "delete from " & Factstats_Tbl & " where name=" & Escape (To_Lower (Fact)));

         -- Tell the user what we did
         Msg := US ("I've forgotten ");
         if Count > 1 then
            Msg := Msg & "all" & natural'Image (Count) & " definitions of";
         end if;
         OutputQ.Say (Msg & "factoid """ & Fact & """!", Request.Destination);
      end if;

      -- Done with the db now
      Disconnect (Handle);

   exception
      when Connect_Error =>
         null;  -- message already sent

      when E : others =>
         OutputQ.Say ("I forgot how to bot:  " & Ada.Exceptions.Exception_Information (E), Request.Destination);
         Disconnect (Handle);
   end Forget_Factoid;

   ---------------------------------------------------------------------------

   -- List all factoids whose names match the given regexp
   procedure List_Factoids (Pat : in string) is

      use Ada.Strings.Unbounded;

      Data   : DB_Result;
      Handle : DB_Handle;
      Hits   : natural;
      Msg    : UString;
      First  : boolean;

      Line_Pause : constant Duration := Duration'Value (Config.Get_Value (Config.Item_Line_Pause));

   begin  -- List_Factoids

      -- Connect and fetch the names of factoids matching the regexp.  We use
      -- "distinct" here, because we don't care whether a factoid has multiple
      -- definitions or not--we're only interested in the name.
      Safe_Connect (Handle);
      Fetch (Handle, "distinct name", Factoid_Tbl, "where name ~* " & Escape (To_Lower (Pat)) & " order by name", Data);
      Disconnect (Handle);

      -- See if we got any matches
      Hits := Rows (Data);
      if Hits > 0 then

         -- Flag determining whether to add a comma separator between names in
         -- the output line
         First := true;

         -- Step through all the names we got.  They're unique because we used
         -- "distinct" in our db query.
         for Row in 1 .. Hits loop

            -- Print a title on the first line
            if Row = 1 then

               -- Look for the magic "all factoids" pattern used by the
               -- command task, and use it to select a more appropriate title.
               -- If the user specifies that pattern explicitly, then they'll
               -- get the special title too, oh boy!
               if Pat = "." then
                  Msg := US ("All factoids:  ");
               else
                  Msg := US ("Factoids matching """ & Pat & """:  ");
               end if;
            end if;

            -- Get the next factoid name and tack it onto the end of the line
            -- we're building, which the first time through is just the title,
            -- and subsequent times is the title plus the preceding names
            declare
               Name : string := Get_Value (Data, Row, "name");
            begin

               -- If the line is full (over our max length), print it and
               -- start a new one
               if Length (Msg) + Name'Length > Message_Limit then
                  if Length (Msg) > 0 then
                     OutputQ.Say (Msg, Request.Destination);
                  end if;

                  -- The "title" for second and subsequent lines is just a
                  -- continuation marker.  Also, reset the insert-comma flag
                  Msg := US (" - and:  ");
                  delay Line_Pause;
                  First := true;
               end if;

               -- See if we need to insert a comma, which we do for all but
               -- the first name on a line
               if First then
                  Append (Msg, Name);
                  First := false;
               else
                  Append (Msg, ", " & Name);
               end if;
            end;
         end loop;

         -- Print out last line if there is one
         if Length (Msg) > 0 then
            OutputQ.Say (Msg, Request.Destination);
         end if;

      -- No matches, no factoid
      else
         OutputQ.Say ("No factoids match """ & Pat & """", Request.Destination);
      end if;

   exception
      when Connect_Error =>
         null;  -- message already sent

      when E : others =>
         OutputQ.Say ("Instead of a list, I got this:  " & Ada.Exceptions.Exception_Information (E), Request.Destination);
         Disconnect (Handle);
   end List_Factoids;

   ---------------------------------------------------------------------------

   -- Fetch one or more factoid definitions from the database and print them,
   -- and update that factoid's usage statistics.  Op is "=" for exact match,
   -- "~*" for regexp match.  From is non-null if this is a "tell" request.
   procedure Fetch_Factoid (Op : in string;   From : in UString := Null_UString) is

      Data   : DB_Result;
      AccCnt : DB_Result;
      Handle : DB_Handle;
      Hits   : natural;

      Line_Pause : constant Duration := Duration'Value (Config.Get_Value (Config.Item_Line_Pause));

   begin  -- Fetch_Factoid

      -- Not sure when this (null factoid name) might happen, but if it does,
      -- treat it as a quip request and bail out
      if Ada.Strings.Unbounded.Length (Request.Data) < 1 then
         OutputQ.Say (Random_Select (Quips_Tbl), Request.Destination);
         return;
      end if;

      -- Have a factoid name, so connect and try to fetch it and its
      -- definition.  May fetch several rows, either multiple defs for a
      -- single factoid name, or multiple names matching a regexp.
      Safe_Connect (Handle);
      Fetch (Handle, "name,value", Factoid_Tbl, "where name" & Op & Escape (To_Lower (S (Request.Data))), Data);

      -- See how many hits we got from our query, and proceed differently if
      -- it's zero versus nonzero.
      Hits := Rows (Data);
      if Hits > 0 then

         -- If this is a tell, see if it's "tell me" or "tell somebody-else"
         -- by comparing the origin with the destination, and print the
         -- appropriate message for each.
         if not Equal (From, Null_UString) then
            if Equal (From, Request.Destination) then
               OutputQ.Say ("You wanted to know:", Request.Destination);
            else
               OutputQ.Say (From & " wanted me to tell you:", Request.Destination);
            end if;
         end if;

         -- Get the factoid name for some testing
         declare
            Key : string := Get_Value (Data, 1, "name");
         begin

            -- Scan the name fields of the rows we got, for any that differ
            -- from the first.  If we find such, then we had a regexp that
            -- matched more than one factoid name; turn this into a "list"
            -- request and bail out.
            for Row in 2 .. Hits loop
               if Get_Value (Data, Row, "name") /= Key then
                  OutputQ.Say ("""" & Request.Data & """ matches more than one factoid ...", Request.Destination);
                  List_Factoids (S (Request.Data));
                  Disconnect (Handle);
                  return;
               end if;
            end loop;

            -- All fetched rows are for the same factoid name, so update that
            -- factoid's fetch stats
            Fetch (Handle, "acc_count", Factstats_Tbl, "where name=" & Escape (Key), AccCnt);
            Statement (Handle, "update " & Factstats_Tbl &
                       " set acc_count=" & natural'Image (Get_Value (AccCnt, 1, "acc_count") + 1) &
                       ",acc_last='now',acc_by=" & Escape (To_Lower (S (Request.Origin))) &
                       " where name='" & Key & "'");
            Disconnect (Handle);
         end;

         -- Print the fetched definition(s) in a human-friendly format
         for Row in 1 .. Hits loop

            -- Only print the factoid name on the first (or only) line
            if Row = 1 then
               OutputQ.Say (Get_Value (Data, Row, "name") & ":  " & Get_Value (Data, Row, "value"), Request.Destination);
            else
               OutputQ.Say ("or: " & Get_Value (Data, Row, "value"), Request.Destination);
            end if;

            -- If this is the second or subsequent line of a multi-definition
            -- factoid, print the definition separator
            if Hits > 1 and then Row /= Hits then
               delay Line_Pause;
--               OutputQ.Say (" - or -", Request.Destination);
--               delay Line_Pause;
            end if;
         end loop;

         -- If this was a "tell" operation, and wasn't a "tell me", report
         -- back to the original requestor what we did
         if (not Equal (From, Null_UString)) and then (not Equal (From, Request.Destination)) then
            OutputQ.Say ("I told " & Request.Destination & natural'Image (Hits) & " line(s).", Request.Origin);
         end if;

      -- Zero hits means no match; tailor the failure message according to
      -- whether it's a "tell me", a "tell somebody-else", or a simple fetch
      else
         Disconnect (Handle);
         if not Equal (From, Null_UString) then
            if Equal (From, Request.Destination) then
               OutputQ.Say ("You wanted to know about """ & Request.Data & """, but I couldn't find it.", From);
            else
               OutputQ.Say (From & " asked me to tell you about """ & Request.Data & """, but I couldn't find it.",
                            Request.Destination);
               OutputQ.Say ("Regrettably, I couldn't find """ & Request.Data & """ to tell " & S (Request.Destination),
                            From);
            end if;
         else
            OutputQ.Say ("Sorry, I couldn't find anything that matches """ & Request.Data & """", Request.Destination);
         end if;
      end if;

   exception
      when Connect_Error =>
         null;  -- message already sent

      when E : others =>
         OutputQ.Say ("I seem to have fetched up a hairball:  " & Ada.Exceptions.Exception_Information (E), Request.Destination);
         Disconnect (Handle);
   end Fetch_Factoid;

   ---------------------------------------------------------------------------

   -- Change a factoid's name
   procedure Rename_Factoid is

      Data    : DB_Result;
      OldName : string := S (Request.Key);
      NewName : string := S (Request.Data);
      Handle  : DB_Handle;

   begin  -- Rename_Factoid

      -- Connect and fetch the ID of the user who created this factoid originally
      Safe_Connect (Handle);
      Fetch (Handle, "creator", Factstats_Tbl, "where name=" & Escape (To_Lower (OldName)), Data);

      -- If we didn't get anything, assume that the factoid doesn't exist;
      -- complain and return
      if Rows (Data) = 0 then
         OutputQ.Say ("I don't seem to know a factoid """ & OldName & """, so there's nothing to rename.",
                      Request.Destination);
         return;
      end if;

      -- Only process the request if made by the original creator, or by the
      -- bot operator
      if Is_Owner_Operator (S (Request.Origin), Get_Value (Data, 1, "creator")) then

         -- Change the name in both the factoid and statistics databases, and
         -- report that to the user
         Statement (Handle, "update " & Factoid_Tbl & " set name=" & Escape (To_Lower (NewName)) &
                    " where name=" & Escape (To_Lower (OldName)));
         Statement (Handle, "update " & Factstats_Tbl & " set name=" & Escape (To_Lower (NewName)) &
                    " where name=" & Escape (To_Lower (OldName)));
         OutputQ.Say ("I've renamed factoid """ & OldName & """ to """ & NewName & """!", Request.Destination);
      end if;

      -- Done with the db now
      Disconnect (Handle);

   exception
      when Connect_Error =>
         null;  -- message already sent

      when E : others =>
         OutputQ.Say ("What's in a name:  " & Ada.Exceptions.Exception_Information (E), Request.Destination);
         Disconnect (Handle);
   end Rename_Factoid;

   ---------------------------------------------------------------------------

   -- Reset (change) a factoid's definition.  This could replace a multi-line
   -- factoid with just one short one, thus losing information, so it should
   -- be used with care.  We should probably think about adding some
   -- safeguards for that case.
   procedure Reset_Factoid is

      Data   : DB_Result;
      Fact   : string := S (Request.Key);
      Handle : DB_Handle;

   begin  -- Reset_Factoid

      -- Connect and fetch the ID of the user who created this factoid originally
      Safe_Connect (Handle);
      Fetch (Handle, "creator", Factstats_Tbl, "where name=" & Escape (To_Lower (Fact)), Data);

      -- If we didn't get anything, assume that the factoid doesn't exist;
      -- complain and return
      if Rows (Data) = 0 then
         OutputQ.Say ("I don't seem to know a factoid """ & Fact &
                      """ ... if you want to set it, just do ""factoid is definition"".",
                      Request.Destination);
         return;
      end if;

      -- Only process the request if made by the original creator, or by the
      -- bot operator
      if Is_Owner_Operator (S (Request.Origin), Get_Value (Data, 1, "creator")) then

         -- Process the request by deleting the old definition(s) and
         -- inserting the new one.  We don't update the statistics here, but
         -- maybe we should; presumably it would require adding a "reset"
         -- field to the stats table, or something like that.
         Statement (Handle, "delete from " & Factoid_Tbl & " where name=" & Escape (To_Lower (Fact)));
         Statement (Handle, "insert into " & Factoid_Tbl & " (name,value) values (" &
                    Escape (To_Lower (Fact)) & "," & Escape (S (Request.Data)) & ")");

         -- Tell the user that the deed is done
         OutputQ.Say ("I've changed factoid """ & Fact & """!", Request.Destination);
      end if;

      -- Done with the db now
      Disconnect (Handle);

   exception
      when Connect_Error =>
         null;  -- message already sent

      when E : others =>
         OutputQ.Say ("I think I need to reset my brain:  " & Ada.Exceptions.Exception_Information (E), Request.Destination);
         Disconnect (Handle);
   end Reset_Factoid;

   ---------------------------------------------------------------------------

   -- Set or update the access level for a usermask
   procedure Set_Access is

      Data   : DB_Result;
      Handle : DB_Handle;
      Hits   : natural;

   begin  -- Set_Access

      -- Connect and try to fetch the usermask from the user auth table
      Safe_Connect (Handle);
      Fetch (Handle, "name", UserLvl_Tbl, "where name = " & Escape (To_Lower (S (Request.Key))), Data);

      -- If we got the usermask, then this is an update; if not, it's an insert
      Hits := Rows (Data);
      if Hits > 0 then
         Statement (Handle, "update " & UserLvl_Tbl & " set level=" & S (Request.Data) &
                    "where name=" & Escape (To_Lower (S (Request.Key))));
         OutputQ.Say ("Access level for " & To_Lower (S (Request.Key)) & " updated to " & S (Request.Data), Request.Origin);
      else
         Statement (Handle, "insert into " & UserLvl_Tbl & " (name,level) values (" &
                    Escape (To_Lower (S (Request.Key))) & "," & S (Request.Data) & ")");
         OutputQ.Say ("Usermask " & To_Lower (S (Request.Key)) & " added with level " & S (Request.Data), Request.Origin);
      end if;

      -- Done with the db now
      Disconnect (Handle);

      -- Re-read the user auth cache
      Auth.Init;

   exception
      when Connect_Error =>
         null;  -- message already sent

      when E : others =>
         OutputQ.Say ("I seem to have accessed a bug:  " & Ada.Exceptions.Exception_Information (E), Request.Destination);
         Disconnect (Handle);
   end Set_Access;

   ---------------------------------------------------------------------------

   -- Hook for future implementation of the "<factoid> is action <action>" command
   procedure Set_Action is
   begin  -- Set_Action
      OutputQ.Say ("The set-action command is not yet implemented.", Request.Destination);
   end Set_Action;

   ---------------------------------------------------------------------------

   -- Hook for future implementation of the "<factoid> is reply <reply>" command
   procedure Set_Reply is
   begin  -- Set_Reply
      OutputQ.Say ("The set-reply command is not yet implemented.", Request.Destination);
   end Set_Reply;

   ---------------------------------------------------------------------------

   -- Show the factoid database stats as part of the "stats" command's overall
   -- summary output
   procedure Show_Stats is

      Data   : DB_Result;
      Handle : DB_Handle;
      FCount : natural := 0;
      QCount : natural := 0;

   begin  -- Show_Stats

      -- Connect and fetch the count of unique factoid names in the factoid table
      Safe_Connect (Handle);
      Fetch (Handle, "count(distinct name)", Factoid_Tbl, "", Data);

      -- If we got data, extract the value; otherwise it stays 0
      if Rows (Data) > 0 then
         FCount := Get_Value (Data, 1, "count");
      end if;

      -- Fetch the count of items in the quotes table
      Fetch (Handle, "count(quote)", Quotes_Tbl, "", Data);

      -- If we got data, extract the value; otherwise it stays 0
      if Rows (Data) > 0 then
         QCount := Get_Value (Data, 1, "count");
      end if;

      -- Done with the db now
      Disconnect (Handle);

      -- Report our results
      OutputQ.Say ("I currently know" & natural'Image (FCount) & " factoids and" & natural'Image (QCount) & " quotes.",
                   Request.Destination);

   exception
      when Connect_Error =>
         null;  -- message already sent

      when E : others =>
         OutputQ.Say ("Statistics show ... a bug:  " & Ada.Exceptions.Exception_Information (E), Request.Destination);
         Disconnect (Handle);
   end Show_Stats;

------------------------------------------------------------------------------
--
-- Public task
--
------------------------------------------------------------------------------

   task body Database_Task is

      use DatabaseQ;

   begin  -- Database_Task

      -- Initialize the random number generator based on the time of day (by
      -- omitting an explicit initiator value)
      Ada.Numerics.Float_Random.Reset (Randoms);

      -- Main task request loop
      loop

         -- Fetch next request from our request queue and handle it
         Requests.Dequeue (Request);
         case Request.Operation is

            when Access_Operation =>
               Set_Access;

            when AddFactoid_Operation =>
               Add_Factoid;

            when FactoidStats_Operation =>
               Factoid_Stats (S (Request.Key));

            when Fetch_Operation =>
               Fetch_Factoid ("=");

            when Forget_Operation =>
               Forget_Factoid;

            when List_Operation =>
               List_Factoids (S (Request.Data));

            when Quip_Operation =>
               -- The probability of actually writing a quip is configurable
               if Ada.Numerics.Float_Random.Random (Randoms) <= float'Value (Config.Get_Value (Config.Item_Quips)) then
                  -- If we do decide to make a quip, spread it over the two
                  -- forms (message and action)
                  if Ada.Numerics.Float_Random.Random (Randoms) <= Config.Act_Vs_Msg then
                     OutputQ.Say (Random_Select (Quips_Tbl), Request.Destination);
                  else
                     OutputQ.Say (IRC.CTCP_Marker & "ACTION " & Random_Select (ActQuips_Tbl) & IRC.CTCP_Marker,
                                  Request.Destination);
                  end if;
               end if;

            when Quote_Operation =>
               declare
                  Quote  : UString;
                  Attrib : UString;
               begin
                  Random_Quote (Quote, Attrib);
                  OutputQ.Say (Quote, Request.Destination);
                  delay Duration'Value (Config.Get_Value (Config.Item_Line_Pause));
                  OutputQ.Say ("  -- " & Attrib, Request.Destination);
               end;

            when RE_Fetch_Operation =>
               Fetch_Factoid ("~*");

            when RE_Tell_Operation =>
               Fetch_Factoid ("~*", Request.Origin);

            when Rename_Operation =>
               Rename_Factoid;

            when ResetFactoid_Operation =>
               Reset_Factoid;

            when SetAction_Operation =>
               Set_Action;

            when SetFactoid_Operation =>
               Set_Factoid;

            when SetReply_Operation =>
               Set_Reply;

            when Shutdown_Operation =>
               -- Arrange to issue a random quit message before we go
               declare
                  Output_Request : OutputQ.Request_Rec;
               begin
                  Output_Request.Operation := OutputQ.Shutdown_Operation;
                  Output_Request.Data := US (Random_Select (QuitMsg_Tbl));
                  OutputQ.Requests.Enqueue (Output_Request);
                  exit;  -- exit the main loop, thus shutting down the task
               end;

            when Snack_Operation =>
               -- Right now, the botsnack behavior is just a way to force a
               -- quip, without the probability of skipping it.  A different
               -- action could be done here, though, like fetching from a
               -- different table, if we ever get the energy to create one.
               -- As with a regular quip, spread it over the two forms
               -- (message and action)
               if Ada.Numerics.Float_Random.Random (Randoms) <= Config.Act_Vs_Msg then
                  OutputQ.Say (Random_Select (Quips_Tbl), Request.Destination);
               else
                  OutputQ.Say (IRC.CTCP_Marker & "ACTION " & Random_Select (ActQuips_Tbl) & IRC.CTCP_Marker, Request.Destination);
               end if;

            when Stats_Operation =>
               Show_Stats;

            when Tell_Operation =>
               Fetch_Factoid ("=", Request.Origin);

         end case;
      end loop;

   exception
      -- Log unexpected exceptions in this task's code and send a crash
      -- request to the command task, which shuts down the bot.
      when E : others =>
         Log.Err (Database_Name, "Exception:  " & Ada.Exceptions.Exception_Information (E));
         CommandQ.Crash (Database_Name);
   end Database_Task;

   ---------------------------------------------------------------------------

end Database;
