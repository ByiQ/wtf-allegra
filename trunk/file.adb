
--
-- File -- File manipulation task package for Allegra info-bot
--


--
-- Standard packages
with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with System;  -- ugh!


--
-- Compiler-specific packages
with Ada.Text_IO.C_Streams;
with GNAT.OS_Lib;
with GNAT.Regpat;
with Interfaces.C_Streams;


--
-- Local library packages
with Strings;
use  Strings;


--
-- Application packages
with Config;
with CommandQ;
with Log;
with OutputQ;


--
-- Request-queue package
with FileQ;


package body File is

------------------------------------------------------------------------------
--
-- Package constants
--
------------------------------------------------------------------------------

   -- This task's name, for logging purposes
   File_Name     : constant string := "File";

   -- Maximum length of a help-file or RM-index line
   Line_Max      : constant := 512;

   -- The marker that makes a marker line
   Marker_Prefix : constant string := "---";

------------------------------------------------------------------------------
--
-- Package types
--
------------------------------------------------------------------------------

   -- A help-file or RM-index line
   subtype Line_Str is string (1 .. Line_Max);

   -- The help-message table's entry type, table type, and a pointer to the
   -- table
   type Help_Item is record
      Topic   : UString;
      Summary : UString;
      Pos     : natural;
   end record;
   type Help_Table is array (positive range <>) of Help_Item;
   type Help_Table_Ptr is access Help_Table;

   -- The RM index table's entry type, table type, and a pointer to the table
   type RM_Index_Item is record
      Text : UString;
      Refs : UString;
   end record;
   type RM_Index_Table is array (positive range <>) of RM_Index_Item;
   type RM_Index_Ptr is access RM_Index_Table;

------------------------------------------------------------------------------
--
-- Package variables
--
------------------------------------------------------------------------------

   -- Timestamp of the help file the last time we initialized the list
   Help_Time      : GNAT.OS_Lib.OS_Time;

   -- The help-message table
   Help_Msgs      : Help_Table_Ptr := null;

   -- Width of widest help topic name
   Topic_Width    : positive;

   -- The RM index table
   RM_Index       : RM_Index_Ptr := null;
   RM_Index_Count : natural;

------------------------------------------------------------------------------
--
-- Package subroutines
--
------------------------------------------------------------------------------

   -- Use GNAT-specific services to open a text file as a C-flavored stream
   procedure Open_Help_File (Path : in  string;
                             Strm : out Interfaces.C_Streams.FILEs;
                             File : out Ada.Text_IO.File_Type) is

      use Interfaces.C_Streams;
      use type System.Address;

      Name   : string := Path & ASCII.NUL;
      Mode   : string := "r" & ASCII.NUL;
      C_Name : chars := Name'Address;
      C_Mode : chars := Mode'Address;

   begin  -- Open_Help_File

      -- Open the stream
      Strm := fopen (C_Name, C_Mode);

      -- A null stream value means the open failed
      if Strm = NULL_Stream then
         Log.Warn (File_Name, "Could not open stream for help file """ & Path & """--help disabled");
         raise Ada.Text_IO.Name_Error;
      end if;

      -- The stream open worked, so map/convert it into a real Ada File_Type
      Ada.Text_IO.C_Streams.Open (File, Ada.Text_IO.In_File, Strm);
   end Open_Help_File;

   ---------------------------------------------------------------------------

   -- Compare two OS_Time values; returns true if they're equal
   function "=" (TS1, TS2 : in GNAT.OS_Lib.OS_Time) return boolean is

      use GNAT.OS_Lib;

   begin  -- "="
      return
        GM_Second (TS1) = GM_Second (TS2) and then
        GM_Minute (TS1) = GM_Minute (TS2) and then
        GM_Hour   (TS1) = GM_Hour   (TS2) and then
        GM_Day    (TS1) = GM_Day    (TS2) and then
        GM_Month  (TS1) = GM_Month  (TS2) and then
        GM_Year   (TS1) = GM_Year   (TS2);
   end "=";

   ---------------------------------------------------------------------------

   -- Returns true if the given line is a marker line
   function Is_Marker (Line : in string) return boolean is
   begin  -- Is_Marker
      return Line'Length > Marker_Prefix'Length and then Line (Line'First .. Line'First + Marker_Prefix'Length - 1) = Marker_Prefix;
   end Is_Marker;

   ---------------------------------------------------------------------------

   -- Initialize the help-item list
   procedure Init_Help is

      ------------------------------------------------------------------------

      use Ada.Text_IO;

      ------------------------------------------------------------------------

      -- Instantiate the deallocation procedure for our help-list node type
      procedure Free is new Ada.Unchecked_Deallocation (Help_Table, Help_Table_Ptr);

      ------------------------------------------------------------------------

      HelpPath : string := Config.Get_Value (Config.Item_HelpPath);
      HelpFile : File_Type;
      HelpStrm : Interfaces.C_Streams.FILEs;
      HelpLine : Line_Str;
      Last     : natural;
      HelpEnts : natural;

      ------------------------------------------------------------------------

   begin  -- Init_Help

      -- Start out with no help enabled; free old table if one exists
      if Help_Msgs /= null then
         Free (Help_Msgs);
         Help_Msgs := null;
      end if;

      -- Capture the timestamp
      Help_Time := GNAT.OS_Lib.File_Time_Stamp (HelpPath);

      -- Open the help file for reading
      begin
         Open_Help_File (HelpPath, HelpStrm, HelpFile);
      exception
         when Ada.Text_IO.Name_Error =>
            return;
         when others =>
            Log.Warn (File_Name, "Could not convert stream for help file """ & HelpPath & """--help disabled");
            return;
      end;

      -- Count the help entries in the file, to determine how big to make
      -- the help table.
      HelpEnts := 0;
      while not End_Of_File (HelpFile) loop
         Get_Line (HelpFile, HelpLine, Last);
         if Is_Marker (HelpLine (1..Last))  then
            HelpEnts := HelpEnts + 1;
         end if;
      end loop;

      -- Rewind the help file and collect data this time through.  Must use
      -- fseek stream call here, since Reset raises a Use_Error.
      if Interfaces.C_Streams.fseek (HelpStrm, 0, Interfaces.C_Streams.SEEK_SET) /= 0 then
         Log.Warn (File_Name, "Cannot rewind help file """ & HelpPath & """--help disabled");
         Close (HelpFile);
         return;
      end if;

      -- Allocate the help table
      Help_Msgs := new Help_Table (1 .. HelpEnts);

      -- Loop through the help items, collecting the topic names and summary
      -- lines
      Topic_Width := positive'First;
      HelpEnts := 0;
      while not End_Of_File (HelpFile) loop
         Get_Line (HelpFile, HelpLine, Last);

         -- Skip empty lines
         if Last > 0 then

            -- See if it's a marker line
            if Is_Marker (HelpLine (1 .. Last)) then

               -- Yes, it's a marker, grab the topic
               declare
                  Topic   : string := HelpLine (Marker_Prefix'Length + 1 .. Last);
                  ItemPos : natural;
               begin

                  -- Update the widest-topic value
                  Topic_Width := positive'Max (Topic_Width, Topic'Length);

                  -- Find the position of the first non-blank line after the marker
                  loop
                     ItemPos := natural (Interfaces.C_Streams.ftell (HelpStrm));
                     Get_Line (HelpFile, HelpLine, Last);
                     exit when Last > 0;
                  end loop;

                  -- We now have all the data we need to describe this help
                  -- item, so put it into the table
                  HelpEnts := HelpEnts + 1;
                  Help_Msgs (HelpEnts) := Help_Item'(US (Topic), US (HelpLine (1 .. Last)), ItemPos);
               end;
            end if;
         end if;
      end loop;

      -- And we're done; apparently this closes the stream too (doesn't it?)
      Close (HelpFile);

   exception
      -- Putting the message into the log file seems to obscure it too much
      when E : others =>
         Put_Line (Standard_Error, "Help init exception:  " & Ada.Exceptions.Exception_Information (E));
   end Init_Help;

   ---------------------------------------------------------------------------

   -- Init the RM index.  Note that this code is very sensitive to the format
   -- of the RM index file.  So go write a better version!  It currently works
   -- with the text RM index found on the ADAIC website.
   procedure Init_RM is

      ------------------------------------------------------------------------

      use Ada.Text_IO;

      ------------------------------------------------------------------------

      Ref_Sep : constant string := "  ";

      ------------------------------------------------------------------------

      RMPath : string := Config.Get_Value (Config.Item_ARMPath);
      RMFile : File_Type;
      RMLine : Line_Str;
      Last   : natural;
      RMEnts : natural;
      LNum   : natural;
      Seen_C : boolean;

      ------------------------------------------------------------------------

      -- Process one RM index entry; may create multiple entries in the RM
      -- index table
      procedure Process_Index_Entry (Line : in string) is

         use Ada.Characters.Handling, Ada.Strings.Fixed;

         RefsAt : natural;
         HTxt   : UString;
         STxt   : UString;
         Refs   : UString;

      begin  -- Process_Index_Entry

         -- Skip section headers, except the second (and presumably any
         -- subsequent) "C" is for "Interfaces.C", dangit
         if Line'Length = 1 and Is_Upper (Line (Line'First)) then
            if Line (Line'First) = 'C' and not Seen_C then
               Seen_C := true;
               return;
            end if;
         end if;

         -- Ensure that this is a head-term
         if Line (Line'First) = ' ' then
            Put_Line (Standard_Error, "RM index line " & Img (LNum) & " not a head-term; format unrecognized.");
            raise Program_Error;
         end if;

         -- Okay, head term; split into text and refs, if any
         RefsAt := Index (Line, Ref_Sep);
         if RefsAt > 0 then
            HTxt := US (Line (Line'First .. RefsAt - 1));
            Refs := US (LTrim (Line (RefsAt .. Line'Last)));
         else
            HTxt := US (Line);
            Refs := Null_UString;
         end if;

         -- Put this head-term into the table if it has refs, otherwise we can
         -- use its text for subsequent sub-terms
         if not Equal (Refs, Null_Ustring) then
            RM_Index_Count := RM_Index_Count + 1;
            RM_Index (RM_Index_Count) := RM_Index_Item'(HTxt, Refs);
         end if;

         -- Fetch sub-terms, if any
         while not End_Of_File (RMFile) loop
            Get_Line (RMFile, RMLine, Last);
            LNum := LNum + 1;

            -- Next blank line ends this term entry
            exit when Last = 0;

            -- See if it's a sub-term or a continuation of the previous
            -- sub-term
            if RMLine (Line'First) = ' ' then

               -- Sub-term; strip off leading blanks, then split it up
               declare
                  Sub : string := LTrim (RMLine (Line'First .. Last));
               begin
                  RefsAt := Index (Sub, Ref_Sep);
                  if RefsAt > 0 then
                     STxt := US (Sub (Sub'First .. RefsAt - 1));
                     Refs := US (LTrim (Sub (RefsAt .. Sub'Last)));
                     RM_Index_Count := RM_Index_Count + 1;
                     RM_Index (RM_Index_Count) := RM_Index_Item'(HTxt & " " & STxt, Refs);
                  else
                     -- Of course there have to be irregularities, sigh.  Some
                     -- sub-terms wrap to the next line; some are "See (also)"
                     -- with no refs.  We ignore the latter, and handle the
                     -- former in a rather brute-force fashion, by simply
                     -- reading the next line and using it as the refs.
                     if Sub'Length < 3 or else Sub (Line'First..Line'First+2) /= "See" then
                        Get_Line (RMFile, RMLine, Last);
                        LNum := LNum + 1;
                        RM_Index_Count := RM_Index_Count + 1;
                        RM_Index (RM_Index_Count) := RM_Index_Item'(HTxt & " " & US (Sub), US (LTrim (RMLine (Line'First..Last))));
                     end if;
                  end if;
               end;
            else

               -- Term-cont, which are just refs; nail them onto previous
               -- term's refs, which are still indexed by RM_Index_Count
               RM_Index (RM_Index_Count).Refs := RM_Index (RM_Index_Count).Refs & " " & US (RMLine (Line'First .. Last));
            end if;
         end loop;
      end Process_Index_Entry;

      ------------------------------------------------------------------------

   begin  -- Init_RM

      -- Open the help file for reading
      begin
         Open (RMFile, In_File, RMPath);
      exception
         when others =>
            Log.Warn (File_Name, "Could not open RM index file """ & RMPath & """--RM lookup disabled");
            return;
      end;

      -- Count the non-blank lines in the file, to determine how big to make
      -- the index table.  This will be more than we need, since some lines
      -- are section headers, sub-terms, refless, and such, but it won't be
      -- far off.
      RMEnts := 0;
      while not End_Of_File (RMFile) loop
         Get_Line (RMFile, RMLine, Last);
         if Last > 0 then
            RMEnts := RMEnts + 1;
         end if;
      end loop;

      -- Allocate the table
      RM_Index := new RM_Index_Table (1 .. RMEnts);
      RM_Index_Count := 0;

      -- Rewind the RM index file and parse it this time through
      Reset (RMFile);
      LNum := 0;

      -- First, find the start of the actual index entries
      while not End_Of_File (RMFile) loop
         Get_Line (RMFile, RMLine, Last);
         LNum := LNum + 1;
         exit when (Last > 0 and then Ada.Strings.Fixed.Index (RMLine (1 .. Last), "operator") > 0);
      end loop;

      -- Now process the index entries:
      --
      -- entry => head-term
      --          {sub-term|term-cont}
      -- blank line(s)
      --
      -- where head-term starts in column 1, sub-term in column N (N>1),
      -- term-cont in column 1.  A *-term is 1-N words separated by single
      -- blanks, followed either by eol or multiple blanks and the refs. A
      -- term-cont is just more refs from the previous term.  A special case
      -- is a line with just one capital letter on it, which is a section
      -- header and is skipped.
      --
      -- This code saves the head-term as-is, unless it doesn't have any refs.
      -- It also remembers the text (non-ref) part of it, and tacks it onto
      -- the front of each sub-term.  Term-conts are tacked onto the end of
      -- the latest term.
      Seen_C := false;
      while not End_Of_File (RMFile) loop
         if Last > 0 then
            Process_Index_Entry (RMLine (1 .. Last));
         end if;
         Get_Line (RMFile, RMLine, Last);
         LNum := LNum + 1;
      end loop;

      -- Done with the file
      Close (RMFile);

   exception
      -- Putting the message into the log file seems to obscure it too much
      when E : others =>
         Put_Line (Standard_Error, "RM init exception:  " & Ada.Exceptions.Exception_Information (E));
   end Init_RM;

   ---------------------------------------------------------------------------

   -- Init the file task
   procedure Init is
   begin  -- Init
      Init_Help;
      Init_RM;
   end Init;

   ---------------------------------------------------------------------------

   -- Process a "help" request
   procedure Help (Req : in FileQ.Request_Rec) is

      ------------------------------------------------------------------------

      use Ada.Strings.Fixed;

      ------------------------------------------------------------------------

      -- Magic keyword "help topics"
      K_Levels : constant string := "levels";

      ------------------------------------------------------------------------

      HelpPath : string := Config.Get_Value (Config.Item_HelpPath);
      HelpNow  : GNAT.OS_Lib.OS_Time;

      ------------------------------------------------------------------------

      -- Print item-specific help
      procedure Item_Help (Item : in natural) is

         ---------------------------------------------------------------------

         use Ada.Text_IO, Interfaces.C_Streams;

         ---------------------------------------------------------------------

         HelpFile : File_Type;
         HelpStrm : FILEs;
         HelpLine : Line_Str;
         Last     : natural;

         ---------------------------------------------------------------------

      begin  -- Item_Help

         -- Open the help file for reading
         begin
            Open_Help_File (HelpPath, HelpStrm, HelpFile);
         exception
            when Ada.Text_IO.Name_Error =>
               return;  -- Already issued a message in Open_Help_File
            when others =>
               Log.Warn (File_Name, "Could not convert stream for help file """ & HelpPath & """--help disabled");
               return;
         end;

         -- Seek to the first line of the item-specific help and start
         -- printing
         if fseek (HelpStrm, long (Help_Msgs (Item).Pos), SEEK_SET) /= 0 then
            OutputQ.Say ("I'm sorry, I can't seem to find the help text for that topic--tell the bot operator, please.",
                         Req.Destination);
            return;
         end if;

         -- Found the first line, print until eof or next marker line
         while not End_Of_File (HelpFile) loop
            Get_Line (HelpFile, HelpLine, Last);
            exit when Is_Marker (HelpLine (1 .. Last));
            OutputQ.Say (HelpLine (1 .. Last), Req.Destination);
            delay Config.Line_Pause;
         end loop;
      end Item_Help;

      ------------------------------------------------------------------------

      -- Lay out a help summary line
      procedure Fmt (Topic, Subject : in string) is
      begin  -- Fmt
         OutputQ.Say ("   " & Head (Topic, Topic_Width) & "  " & Subject, Req.Destination);
         delay Config.Line_Pause;
      end Fmt;

      ------------------------------------------------------------------------

   begin  -- Help

      -- See if we have anything to say
      if Help_Msgs = null then
         OutputQ.Say ("Help seems to be disabled at the moment, sorry.  Tell the bot operator, please.", Req.Destination);
         return;
      end if;

      -- We have some help topics, see if we're doing a summary or a specific topic
      if Equal (Req.Data, Null_UString) then

         -- No topic given, print summaries only
         OutputQ.Say ("I currently know the following help topics:", Req.Destination);
         delay Config.Line_Pause;

         -- Main summary listing, extracted from help file
         Fmt ("Topic", "Subject");
         Fmt ("=====", "=======");
         for Item in Help_Msgs'Range loop
            Fmt (S (Help_Msgs (Item).Topic), S (Help_Msgs (Item).Summary));
         end loop;

         -- Magic keyword topics
         Fmt (K_Levels, "List command access levels");

         -- Summary trailer
         OutputQ.Say ("The shortcut string is """ & Config.Get_Value (Config.Item_Shorthand) &
                      """; a leading ""~"" in a factoid name treats it as a regexp.", Req.Destination);
         delay Config.Line_Pause;
         OutputQ.Say ("Use ""help <topic>"" for details about one of the listed topics.", Req.Destination);
      else

         -- Check for magic keywords first
         if Equal (Req.Data, US (K_Levels)) then
            OutputQ.Say ("Required access levels for each command:", Req.Destination);
            delay Config.Line_Pause;
            for Cmd in Config.Valid_Commands loop
               OutputQ.Say ("   " & Config.Cmd_Names (Cmd) & Img (Config.Get_Auth_Level (Cmd), 2), Req.Destination);
               delay Config.Line_Pause;
            end loop;
            return;
         end if;

         -- See if we need to re-scan the help file
         HelpNow := GNAT.OS_Lib.File_Time_Stamp (HelpPath);
         if HelpNow /= Help_Time then
            Log.Dbg (File_Name, "Re-scanning help file """ & HelpPath & """");
            Init_Help;

            -- After the rescan, it's possible that the help table is now empty
            if Help_Msgs = null then
               OutputQ.Say ("I lost my help!  Please chide the bot op for cheesing the help file.", Req.Destination);
               return;
            end if;
         end if;

         -- Search the help list for the given topic
         for Item in Help_Msgs'Range loop
            if Equal (Help_Msgs (Item).Topic, Req.Data) then
               -- Found it, print the help text and quit
               Item_Help (Item);
               return;
            end if;
         end loop;

         -- If no find, say so and quit
         OutputQ.Say ("I couldn't find the help topic """ & S (Req.Data) & """, sorry.", Req.Destination);
      end if;
   end Help;

   ---------------------------------------------------------------------------

   -- Do a lookup in the RM index table
   procedure Lookup (Req : in FileQ.Request_Rec) is

      ------------------------------------------------------------------------

      use GNAT.Regpat;

      ------------------------------------------------------------------------

      -- Maximum matches that we'll print; arbitrary
      Max_Print : constant := 10;

      ------------------------------------------------------------------------

      -- Pointer to new pattern matcher
      type Matcher_Ptr is access Pattern_Matcher;

      -- Instantiate the deallocation procedure for our pattern-match pointer
      procedure Free is new Ada.Unchecked_Deallocation (Pattern_Matcher, Matcher_Ptr);

      ------------------------------------------------------------------------

      Pat     : Matcher_Ptr;
      Matched : natural;
      Matches : array (1 .. Max_Print) of RM_Index_Item;
      Refs    : UString;

      ------------------------------------------------------------------------

   begin  -- Lookup

      -- Compile the regex
      Pat := new Pattern_Matcher'(Compile (S (Req.Data),  Case_Insensitive));

      -- Scan the index for matches, count how many, and save the first few
      Matched := 0;
      for Item in 1 .. RM_Index_Count loop
         if Match (Pat.all, S (RM_Index (Item).Text)) = 1 then
            Matched := Matched + 1;
            if Matched <= Max_Print then
               Matches (Matched) := RM_Index (Item);
            end if;
         end if;
      end loop;

      -- If no matches, let user know that
      if Matched < 1 then
         OutputQ.Say ("The pattern """ & S (Req.Data) & """ did not match any RM index entries, sorry.",
                      Req.Destination);
         return;
      end if;

      -- If we got too many, just say so and we're done
      if Matched > Max_Print then
         OutputQ.Say ("The pattern """ & S (Req.Data) & """ matched " & Img (Matched) &
                      " RM index entries.  Try narrowing your search by using a more specific pattern.",
                      Req.Destination);
         return;
      end if;

      -- Few enough to show, so show them
      for Item in 1 .. Matched loop
         if Equal (Matches (Item).Refs, Null_UString) then
            Refs := US ("(no refs found)");
         else
            Refs := Matches (Item).Refs;
         end if;
         OutputQ.Say (Matches (Item).Text & " " & Refs, Req.Destination);
         delay Config.Line_Pause;
      end loop;

   exception
      when GNAT.Regpat.Expression_Error =>
         OutputQ.Say ("The pattern """ & S (Req.Data) & """ is illegal, sorry.  Try ""!help regex"" for tips.",
                      Req.Destination);

      when E : others =>
         Log.Err (File_Name, "RM lookup exception:  " & Ada.Exceptions.Exception_Information (E));
         CommandQ.Crash (File_Name);
   end Lookup;

------------------------------------------------------------------------------
--
-- Public task
--
------------------------------------------------------------------------------

   task body File_Task is

      use FileQ;

      Request : Request_Rec;

   begin  -- File_Task

      -- Main task request loop
      loop

         -- Fetch next request from our request queue and handle it
         Requests.Dequeue (Request);
         case Request.Operation is
            when Shutdown_Operation =>
               exit;  -- exit the main loop, thus shutting down the task

            when RM_Operation =>
               Lookup (Request);

            when Help_Operation =>
               Help (Request);

            when Stats_Operation =>
               -- Show the file stats as part of the "stats" command's overall
               -- summary output
               OutputQ.Say ("I know " & Img (Help_Msgs'Length) & " separate help topics, and have " &
                            Img (RM_Index_Count) & " searchable RM index entries.", Request.Destination);
         end case;
      end loop;

   exception
      when E : others =>
         Log.Err (File_Name, "Exception:  " & Ada.Exceptions.Exception_Information (E));
         CommandQ.Crash (File_Name);
   end File_Task;

   ---------------------------------------------------------------------------

begin  -- package File initialization
   Init;
end File;
