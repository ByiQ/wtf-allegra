with
  Ada.Characters.Handling,
  Ada.Exceptions,
  Ada.Numerics.Float_Random,
  Ada.Strings.Unbounded,
  Auth,
  Config,
  Command,
  DB,
  Log,
  Output;

use
  Ada.Characters.Handling,
  Ada.Strings.Unbounded,
  Log;

package body Database is

   task body Database_Task is
      function S (Source : in Ada.Strings.Unbounded.Unbounded_String) return string
        renames Ada.Strings.Unbounded.To_String;

      function US (Source : in string) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      Message_Limit    : constant := 256;

      Quip_Probability : constant := 1.0 / 2.0;  -- need to put this in the db instead

      Factoid_Tbl   : constant string := "factoids";
      Factstats_Tbl : constant string := "factstats";
      Quips_Tbl     : constant string := "quips";
      QuitMsg_Tbl   : constant string := "quitmsgs";
      Quotes_Tbl    : constant string := "quotes";
      UserLvl_Tbl   : constant string := "usrlevels";

      Attrib         : Unbounded_String;
      Crash_Request  : Command.Request_Rec;
      Quote          : Unbounded_String;
      Randoms        : Ada.Numerics.Float_Random.Generator;
      Request        : Request_Rec;
      Output_Request : Output.Request_Rec;


      procedure Say (Msg : in string;  To : in Unbounded_String) is
      begin  -- Say
         Output_Request.Operation := Output.Message_Operation;
         Output_Request.Destination := To;
         Output_Request.Data := US (Msg);
         Output.Requests.Enqueue (Output_Request);
      end Say;

      function Is_Owner_Operator (Who     : in string;
                                  Creator : in string) return boolean is
      begin  -- Is_Owner_Operator
         if Creator /= To_Lower (Who) then
            if Auth.Level (Request.Requestor) < Auth.Bot_Operator_Level then
               Say ("I'm sorry " & Who & ", that factoid was created by " &
                    Creator & " and I can't let you change it.  Check with " &
                    Creator & " or my operator to get it changed.", Request.Destination);
               return false;
            end if;
         end if;
         return true;
      end Is_Owner_Operator;

      function Random_Select (Table : in string) return string is

         use Ada.Numerics.Float_Random, DB;

         Count  : natural;
         Data   : DB_Result;
         Handle : DB_Handle;
         Index  : positive;

      begin  -- Random_Select
         Connect (Handle, Host => "", DB => Config.Allegra_DB);

         Fetch (Handle, "count(msg)", Table, "", Data);
         if Rows (Data) > 0 then
            Count := Get_Value (Data, 1, "count");
         else
            Count := 0;
         end if;
         if Count < 1 then
            return "";
         end if;

         Index := (integer (float (Count) * Random (Randoms)) mod Count) + 1;
         Fetch (Handle, "msg", Table, "where num=" & positive'Image (Index), Data);
         Disconnect (Handle);
         if Rows (Data) > 0 then
            return Get_Value (Data, 1, "msg");
         else
            return "";
         end if;
      end Random_Select;

      procedure Random_Quote (Quote, Attrib : out Unbounded_String) is

         use Ada.Numerics.Float_Random, DB;

         Count  : natural;
         Data   : DB_Result;
         Handle : DB_Handle;
         Index  : positive;

      begin  -- Random_Quote
         Connect (Handle, Host => "", DB => Config.Allegra_DB);

         Fetch (Handle, "count(quote)", Quotes_Tbl, "", Data);
         if Rows (Data) > 0 then
            Count := Get_Value (Data, 1, "count");
         else
            Count := 0;
         end if;
         if Count > 0 then
            Index := (integer (float (Count) * Random (Randoms)) mod Count) + 1;
            Fetch (Handle, "quote,attr", Quotes_Tbl, "where num=" & positive'Image (Index), Data);
            if Rows (Data) > 0 then
               Disconnect (Handle);
               Quote  := US (Source => Get_Value (Data, 1, "quote"));
               Attrib := US (Source => Get_Value (Data, 1, "attr"));
               return;
            end if;
         end if;
         Disconnect (Handle);
         Quote  := Null_Unbounded_String;
         Attrib := Null_Unbounded_String;
      end Random_Quote;

      procedure Set_Factoid is

         use DB;

         Data   : DB_Result;
         Fact   : string := S (Request.Key);
         Handle : DB_Handle;
         Hits   : natural;
         Msg    : Unbounded_String;

      begin  -- Set_Factoid
         Connect (Handle, Host => "", DB => Config.Allegra_DB);
         Fetch (Handle, "name", Factoid_Tbl, "where name=" & Escape (To_Lower (Fact)), Data);
         Hits := Rows (Data);
         if Hits = 0 then
            Statement (Handle, "insert into " & Factoid_Tbl & " (name,value) values (" &
                       To_Lower (Escape (Fact)) & "," & Escape (S (Request.Data)) & ")");
            Statement (Handle, "insert into " & Factstats_Tbl & " (name,created,creator) values (" &
                       To_Lower (Escape (Fact)) & ",'now'," & Escape (To_Lower (S (Request.Origin))) & ")");
            Say ("""" & Fact & """ has been added!", Request.Destination);
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
            Say (S (Msg), Request.Destination);
         end if;
         Disconnect (Handle);
      end Set_Factoid;

      procedure Add_Factoid is

         use DB;

         Data   : DB_Result;
         Fact   : string := S (Request.Key);
         Handle : DB_Handle;
         Hits   : natural;

      begin  -- Add_Factoid
         Connect (Handle, Host => "", DB => Config.Allegra_DB);
         Fetch (Handle, "name", Factoid_Tbl, "where name=" & Escape (To_Lower (Fact)), Data);
         Hits := Rows (Data);
         if Hits = 0 then
            Say ("I don't know a factoid """ & """ yet, but what the heck.", Request.Destination);
            Disconnect (Handle);
            Set_Factoid;
            return;
         end if;

         Statement (Handle, "insert into " & Factoid_Tbl & " (name,value) values (" &
                    To_Lower (Escape (Fact)) & "," & Escape (S (Request.Data)) & ")");
         Say ("Another definition for """ & Fact & """ has been added!", Request.Destination);
         Disconnect (Handle);
      end Add_Factoid;

      procedure Factoid_Stats (Name : in string) is

         use DB;

         Data   : DB_Result;
         Handle : DB_Handle;
         Msg    : Unbounded_String;

      begin  -- Factoid_Stats
         Connect (Handle, Host => "", DB => Config.Allegra_DB);
         Fetch (Handle, "*", Factstats_Tbl, "where name=" & Escape (To_Lower (Name)), Data);
         Disconnect (Handle);
         if Rows (Data) > 0 then
            Say ("The factoid named """ & Name & """ was created by " & Get_Value (Data, 1, "creator") &
                 " on " & Get_Value (Data, 1, "created") & ".", Request.Destination);
            delay 1.5;
            declare
               Count : string := Get_Value (Data, 1, "acc_count");
            begin
               if Count = "0" then
                  Say ("It has never been accessed.", Request.Destination);
               else
                  if Count = "1" then
                     Msg := US ("It has been accessed once");
                  else
                     Msg := US ("It has been accessed " & Count & " times");
                  end if;
                  Msg := Msg & ", last by " & Get_Value (Data, 1, "acc_by") &
                    " on " & Get_Value (Data, 1, "acc_last") & ".";
                  Say (S (Msg), Request.Destination);
               end if;
            end;
         else
            Say ("I can't seem to locate a factoid named """ & Name & """, sorry.", Request.Destination);
         end if;
      end Factoid_Stats;

      procedure Forget_Factoid is

         use DB;

         Data   : DB_Result;
         Fact   : string := S (Request.Key);
         Handle : DB_Handle;
         Count  : natural;
         Msg    : Unbounded_String;

      begin  -- Forget_Factoid
         Connect (Handle, Host => "", DB => Config.Allegra_DB);
         Fetch (Handle, "creator", Factstats_Tbl, "where name=" & Escape (To_Lower (Fact)), Data);
         if Rows (Data) = 0 then
            Say ("I don't seem to know a factoid """ & Fact & """, so nothing was forgotten.",
                 Request.Destination);
            return;
         end if;

         if Is_Owner_Operator (S (Request.Origin), Get_Value (Data, 1, "creator")) then
            Fetch (Handle, "count(name)", Factoid_Tbl, "where name=" & Escape (To_Lower (Fact)), Data);
            if Rows (Data) > 0 then
               Count := Get_Value (Data, 1, "count");
            else
               Count := 1;  -- assuming
            end if;
            Statement (Handle, "delete from " & Factoid_Tbl   & " where name=" & Escape (To_Lower (Fact)));
            Statement (Handle, "delete from " & Factstats_Tbl & " where name=" & Escape (To_Lower (Fact)));
            Msg := US ("I've forgotten ");
            if Count > 1 then
               Msg := Msg & "all" & natural'Image (Count) & " definitions of";
            end if;
            Say (S (Msg & " factoid """ & Fact & """!"), Request.Destination);
         end if;

         Disconnect (Handle);
      end Forget_Factoid;

      procedure List_Factoids (Pat : string) is

         use DB;

         Data   : DB_Result;
         Handle : DB_Handle;
         Hits   : natural;
         Msg    : Unbounded_String;
         First  : boolean;

      begin  -- List_Factoids
         Connect (Handle, Host => "", DB => Config.Allegra_DB);

         Fetch (Handle, "distinct name", Factoid_Tbl, "where name ~* " & Escape (To_Lower (Pat)) & " order by name", Data);
         Disconnect (Handle);
         Hits := Rows (Data);
         if Hits > 0 then
            First := true;
            for Row in 1 .. Hits loop
               if Row = 1 then
                  if Pat = "." then
                     Msg := US ("All factoids:  ");
                  else
                     Msg := US ("Factoids matching """ & Pat & """:  ");
                  end if;
               end if;
               declare
                  Name : string := Get_Value (Data, Row, "name");
               begin
                  if Length (Msg) + Name'Length > Message_Limit then
                     if Length (Msg) > 0 then
                        Say (S (Msg), Request.Destination);
                     end if;
                     Msg := US (" - and:  ");
                     delay 2.0;
                     First := true;
                  end if;
                  if First then
                     Append (Msg, Name);
                     First := false;
                  else
                     Append (Msg, ", " & Name);
                  end if;
               end;
            end loop;
            if Length (Msg) > 0 then
               Say (S (Msg), Request.Destination);
            end if;
         else
            Say ("No factoids match """ & Pat & """", Request.Destination);
         end if;
      end List_Factoids;

      procedure Fetch_Factoid (Op : in string;   From : in Unbounded_String := Null_Unbounded_String) is

         use DB;

         Data   : DB_Result;
         AccCnt : DB_Result;
         Handle : DB_Handle;
         Hits   : natural;

      begin  -- Fetch_Factoid
         if Length (Request.Data) < 1 then
            Say (Random_Select (Quips_Tbl), Request.Destination);
            return;
         end if;

         Connect (Handle, Host => "", DB => Config.Allegra_DB);

         Fetch (Handle, "name,value", Factoid_Tbl, "where name" & Op & Escape (To_Lower (S (Request.Data))), Data);
         Hits := Rows (Data);
         if Hits > 0 then
            if From /= Null_Unbounded_String then
               if From = Request.Destination then
                  Say ("You wanted to know:", Request.Destination);
               else
                  Say (S (From) & " wanted me to tell you:", Request.Destination);
               end if;
            end if;
            declare
               Key : string := Get_Value (Data, 1, "name");
            begin
               for Row in 2 .. Hits loop
                  if Get_Value (Data, Row, "name") /= Key then
                     Say ("""" & S (Request.Data) & """ matches more than one factoid ...", Request.Destination);
                     List_Factoids (S (Request.Data));
                     Disconnect (Handle);
                     return;
                  end if;
               end loop;
               Fetch (Handle, "acc_count", Factstats_Tbl, "where name='" & Key & "'", AccCnt);
               Statement (Handle, "update " & Factstats_Tbl &
                          " set acc_count=" & natural'Image (Get_Value (AccCnt, 1, "acc_count") + 1) &
                          ",acc_last='now',acc_by=" & Escape (To_Lower (S (Request.Origin))) &
                          " where name='" & Key & "'");
               Disconnect (Handle);
            end;
            for Row in 1 .. Hits loop
               if Row = 1 then
                  Say (Get_Value (Data, Row, "name") & ":  " & Get_Value (Data, Row, "value"), Request.Destination);
               else
                  Say (Get_Value (Data, Row, "value"), Request.Destination);
               end if;
               if Hits > 1 and then Row /= Hits then
                  delay 1.0;
                  Say (" - or -", Request.Destination);
                  delay 1.0;
               end if;
            end loop;
            if From /= Null_Unbounded_String and then From /= Request.Destination then
               Say ("I told " & S (Request.Destination) & natural'Image (Hits) & " line(s).", Request.Origin);
            end if;
         else
            Disconnect (Handle);
            if From /= Null_Unbounded_String then
               if From = Request.Destination then
                  Say ("You wanted to know about """ & S (Request.Data) & """, but I couldn't find it.", From);
               else
                  Say (S (From) & " asked me to tell you about """ & S (Request.Data) & """, but I couldn't find it.",
                       Request.Destination);
                  Say ("Regrettably, I couldn't find """ & S (Request.Data) & """ to tell " & S (Request.Destination),
                       From);
               end if;
            else
               Say ("Sorry, I couldn't find anything that matches """ & S (Request.Data) & """", Request.Destination);
            end if;
         end if;
      end Fetch_Factoid;

      procedure Rename_Factoid is

         use DB;

         Data    : DB_Result;
         OldName : string := S (Request.Key);
         NewName : string := S (Request.Data);
         Handle  : DB_Handle;

      begin  -- Rename_Factoid
         Connect (Handle, Host => "", DB => Config.Allegra_DB);
         Fetch (Handle, "creator", Factstats_Tbl, "where name=" & Escape (To_Lower (OldName)), Data);
         if Rows (Data) = 0 then
            Say ("I don't seem to know a factoid """ & OldName & """, so there's nothing to rename.",
                 Request.Destination);
            return;
         end if;

         if Is_Owner_Operator (S (Request.Origin), Get_Value (Data, 1, "creator")) then
            Statement (Handle, "update " & Factoid_Tbl & " set name=" & Escape (To_Lower (NewName)) &
                       " where name=" & Escape (To_Lower (OldName)));
            Statement (Handle, "update " & Factstats_Tbl & " set name=" & Escape (To_Lower (NewName)) &
                       " where name=" & Escape (To_Lower (OldName)));
            Say ("I've renamed factoid """ & OldName & """ to """ & NewName & """!", Request.Destination);
         end if;

         Disconnect (Handle);
      end Rename_Factoid;

      procedure Reset_Factoid is

         use DB;

         Data   : DB_Result;
         Fact   : string := S (Request.Key);
         Handle : DB_Handle;

      begin  -- Reset_Factoid
         Connect (Handle, Host => "", DB => Config.Allegra_DB);
         Fetch (Handle, "creator", Factstats_Tbl, "where name=" & Escape (To_Lower (Fact)), Data);
         if Rows (Data) = 0 then
            Say ("I don't seem to know a factoid """ & Fact &
                 """ ... if you want to set it, just do ""factoid is definition"".",
                 Request.Destination);
            return;
         end if;

         if Is_Owner_Operator (S (Request.Origin), Get_Value (Data, 1, "creator")) then
            Statement (Handle, "delete from " & Factoid_Tbl & " where name=" & Escape (To_Lower (Fact)));
            Statement (Handle, "insert into " & Factoid_Tbl & " (name,value) values (" &
                       Escape (To_Lower (Fact)) & "," & Escape (S (Request.Data)) & ")");
            Say ("I've changed factoid """ & Fact & """!", Request.Destination);
         end if;

         Disconnect (Handle);
      end Reset_Factoid;

      procedure Set_Access is

         use DB;

         Data   : DB_Result;
         Handle : DB_Handle;
         Hits   : natural;

      begin  -- Set_Access
         Connect (Handle, Host => "", DB => Config.Allegra_DB);

         Fetch (Handle, "name", UserLvl_Tbl, "where name = '" & To_Lower (S (Request.Key)) & "'", Data);
         Hits := Rows (Data);
         if Hits > 0 then
            Statement (Handle, "update " & UserLvl_Tbl & " set level=" & S (Request.Data) &
                       "where name=" & Escape (To_Lower (S (Request.Key))));
            Say ("Access level for " & To_Lower (S (Request.Key)) & " updated to " & S (Request.Data), Request.Origin);
         else
            Statement (Handle, "insert into " & UserLvl_Tbl & " (name,level) values (" &
                       Escape (To_Lower (S (Request.Key))) & "," & S (Request.Data) & ")");
            Say ("Usermask " & To_Lower (S (Request.Key)) & " added with level " & S (Request.Data), Request.Origin);
         end if;
         Disconnect (Handle);
         Auth.Init;
      end Set_Access;

      procedure Set_Action is

         use DB;

         Handle : DB_Handle;

      begin  -- Set_Action
         Say ("The set-action command is not yet implemented.", Request.Destination);
      end Set_Action;

      procedure Set_Reply is

         use DB;

         Handle : DB_Handle;

      begin  -- Set_Reply
         Say ("The set-reply command is not yet implemented.", Request.Destination);
      end Set_Reply;

      procedure Show_Stats is

         use DB;

         Data   : DB_Result;
         Handle : DB_Handle;

      begin  -- Show_Stats
         DB.Connect (Handle, Host => "", DB => Config.Allegra_DB);
         Fetch (Handle, "count(distinct name)", Factoid_Tbl, "", Data);
         Disconnect (Handle);
         if Rows (Data) > 0 then
            Say ("I currently know " & Get_Value (Data, 1, "count") & " factoids.", Request.Destination);
         end if;
      end Show_Stats;

   begin  -- Database_Task
      Ada.Numerics.Float_Random.Reset (Randoms);
      loop
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
               if Ada.Numerics.Float_Random.Random (Randoms) <= Quip_Probability then
                  Say (Random_Select (Quips_Tbl), Request.Destination);
               end if;

            when Quote_Operation =>
               Random_Quote (Quote, Attrib);
               Say (S (Quote), Request.Destination);
               delay 1.5;
               Say (S ("  -- " & Attrib), Request.Destination);

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
               Output_Request.Operation := Output.Shutdown_Operation;
               Output_Request.Data := US (Random_Select (QuitMsg_Tbl));
               Output.Requests.Enqueue (Output_Request);
               exit;

            when Stats_Operation =>
               Show_Stats;

            when Tell_Operation =>
               Fetch_Factoid ("=", Request.Origin);

         end case;
      end loop;

   exception
      when E: others =>
         Err (Database_Name, "Exception:  " & Ada.Exceptions.Exception_Information (E));
         Crash_Request.Operation := Command.Crash_Operation;
         Crash_Request.Data      := US (Database_Name);
         Command.Requests.Enqueue (Crash_Request);
   end Database_Task;

end Database;
