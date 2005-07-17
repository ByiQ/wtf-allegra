
--
-- File -- File manipulation task package for Allegra info-bot
--


--
-- Standard packages
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with System;  -- ugh!


--
-- Compiler-specific packages
with Ada.Text_IO.C_Streams;
with GNAT.OS_Lib;
with Interfaces.C_Streams;


--
-- Local library packages
with Strings;
use  Strings;


--
-- Application packages
with Config;
with Log;
with Output;


package body File is

------------------------------------------------------------------------------
--
-- Package constants
--
------------------------------------------------------------------------------

   -- This task's name, for logging purposes
   File_Name     : constant string := "File";

   -- Maximum length of a help-file line
   Help_Line_Max : constant := 256;

   -- The marker that makes a marker line
   Marker_Prefix : constant string := "---";

------------------------------------------------------------------------------
--
-- Package types
--
------------------------------------------------------------------------------

   -- A help-file line
   subtype Help_Line_Str is string (1 .. Help_Line_Max);

   -- A help-item node of the singly-linked help list
   type Help_Item;
   type Help_Item_Ptr is access Help_Item;
   type Help_Item is record
      Topic   : UString;
      Summary : UString;
      Pos     : natural;
      Next    : Help_Item_Ptr;
   end record;

------------------------------------------------------------------------------
--
-- Package variables
--
------------------------------------------------------------------------------

   -- Timestamp of the help file the last time we initialized the list
   Help_Time   : GNAT.OS_Lib.OS_Time;

   -- The help-item list
   Help_List   : Help_Item_Ptr := null;

   -- Width of widest help topic name
   Topic_Width : positive;

------------------------------------------------------------------------------
--
-- Package subroutines
--
------------------------------------------------------------------------------

   -- Instantiate the deallocation procedure for our help-list node type
   procedure Free is new Ada.Unchecked_Deallocation (Help_Item, Help_Item_Ptr);

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
      Strm := fopen (C_Name, C_Mode);
      if Strm = NULL_Stream then
         Log.Warn (File_Name, "Could not open stream for help file """ & Path & """--help disabled");
         raise Ada.Text_IO.Name_Error;
      end if;
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
      return Line'Length > Marker_Prefix'Length and then Line (1 .. Marker_Prefix'Length) = Marker_Prefix;
   end Is_Marker;

   ---------------------------------------------------------------------------

   -- Initialize the file task
   procedure Init is

      ------------------------------------------------------------------------

      use Ada.Text_IO;

      ------------------------------------------------------------------------

      HelpPath : string := Config.Get_Value (Config.Item_HelpPath);
      HelpFile : File_Type;
      HelpStrm : Interfaces.C_Streams.FILEs;
      HelpLine : Help_Line_Str;
      Last     : natural;
      HelpTail : Help_Item_Ptr;
      HelpGone : Help_Item_Ptr;

      ------------------------------------------------------------------------

   begin  -- Init

      -- Start out with no help enabled; free old list if one exists
      if Help_List /= null then
         HelpTail := Help_List;
         while HelpTail /= null loop
            HelpGone := HelpTail;
            HelpTail := HelpTail.Next;
            Free (HelpGone);
         end loop;
         Help_List := null;
      end if;
      HelpTail := null;
      Topic_Width := positive'First;

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

      -- Loop through the help items, collecting the topic names and summary
      -- lines
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
                  Item    : Help_Item_Ptr;
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
                  -- item, so build the new node
                  Item := new Help_Item'(US (Topic), US (HelpLine (1 .. Last)), ItemPos, null);

                  -- And link the new node into the end of the list
                  if HelpTail = null then
                     Help_List := Item;
                     HelpTail := Help_List;
                  else
                     HelpTail.Next := Item;
                     HelpTail := HelpTail.Next;
                  end if;
               end;
            end if;
         end if;
      end loop;

      -- And we're done; apparently this closes the stream too (doesn't it?)
      Close (HelpFile);

   exception
      when E : others =>
         Put_Line (Standard_Error, "File init exception:  " & Ada.Exceptions.Exception_Information (E));
   end Init;

   ---------------------------------------------------------------------------

   procedure Help (Req : in Request_Rec) is

      ------------------------------------------------------------------------

      use Ada.Strings.Fixed, Ada.Strings.Unbounded;

      ------------------------------------------------------------------------

      -- Pause between each line of help output, in seconds
      Line_Pause : constant Duration := 0.75;

      -- Magic keyword "help topics"
      K_Levels   : constant string := "levels";

      ------------------------------------------------------------------------

      HelpPath : string := Config.Get_Value (Config.Item_HelpPath);
      Item     : Help_Item_Ptr := Help_List;
      HelpNow  : GNAT.OS_Lib.OS_Time;

      ------------------------------------------------------------------------

      -- Print item-specific help
      procedure Item_Help is

         ---------------------------------------------------------------------

         use Ada.Text_IO, Interfaces.C_Streams;

         ---------------------------------------------------------------------

         HelpFile : File_Type;
         HelpStrm : FILEs;
         HelpLine : Help_Line_Str;
         Last     : natural;

         ---------------------------------------------------------------------

      begin  -- Item_Help

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

         -- Seek to the first line of the item-specific help and start
         -- printing
         if fseek (HelpStrm, long (Item.Pos), SEEK_SET) /= 0 then
            Output.Say ("I'm sorry, I can't seem to find the help text for that topic--tell the bot operator, please.",
                        Req.Destination);
            return;
         end if;

         -- Found the first line, print until eof or next marker line
         while not End_Of_File (HelpFile) loop
            Get_Line (HelpFile, HelpLine, Last);
            exit when Is_Marker (HelpLine (1 .. Last));
            Output.Say (HelpLine (1 .. Last), Req.Destination);
            delay Line_Pause;
         end loop;
      end Item_Help;

      ------------------------------------------------------------------------

   begin  -- Help

      -- See if we have anything to say
      if Item = null then
         Output.Say ("Help seems to be disabled at the moment, sorry.  Tell the bot operator, please.", Req.Destination);
         return;
      end if;

      -- We have some help topics, see if we're doing a summary or a specific topic
      if Req.Data = Null_UString then

         -- No topic given, print summaries only
         Output.Say ("I currently know the following help topics:", Req.Destination);
         delay Line_Pause;
         while Item /= null loop
            Output.Say ("   " & Head (S (Item.Topic), Topic_Width) & "  " & S (Item.Summary), Req.Destination);
            Item := Item.Next;
            delay Line_Pause;
         end loop;

         -- Magic keyword topics
         Output.Say ("   " & Head (K_Levels, Topic_Width) & "  List command access levels", Req.Destination);
         delay Line_Pause;

         -- Summary trailer
         Output.Say ("The shortcut string is """ & Config.Get_Value (Config.Item_Shorthand) &
                     """; a leading ""~"" in a factoid name treats it as a regexp.", Req.Destination);
         delay Line_Pause;
         Output.Say ("Use ""help <topic>"" for details about one of the listed topics.", Req.Destination);
      else

         -- Check for magic keywords first
         if Req.Data = US (K_Levels) then
            Output.Say ("Required access levels for each command:", Req.Destination);
            delay Line_Pause;
            for Cmd in Config.Valid_Commands loop
               Output.Say ("   " & Config.Cmd_Names (Cmd) & Img (Config.Get_Auth_Level (Cmd), 2), Req.Destination);
               delay Line_Pause;
            end loop;
            return;
         end if;

         -- See if we need to re-scan the help file
         HelpNow := GNAT.OS_Lib.File_Time_Stamp (HelpPath);
         if HelpNow /= Help_Time then
            Log.Dbg (File_Name, "Re-scanning help file """ & HelpPath & """");
            Init;

            -- After the rescan, it's possible that the help list is now empty
            Item := Help_List;
            if Item = null then
               Output.Say ("I lost my help!  Please chide the bot op for cheesing the help file.", Req.Destination);
               return;
            end if;
         end if;

         -- Search the help list for the given topic
         while Item /= null loop
            exit when Item.Topic = Req.Data;
            Item := Item.Next;
         end loop;

         -- If no find, say so and quit
         if Item = null then
            Output.Say ("I couldn't find the help topic """ & S (Req.Data) & """, sorry.", Req.Destination);
            return;
         end if;

         -- Found it, print the help text
         Item_Help;
      end if;
   end Help;

------------------------------------------------------------------------------
--
-- Public task
--
------------------------------------------------------------------------------

   task body File_Task is

      Request        : Request_Rec;
      Output_Request : Output.Request_Rec;

   begin  -- File_Task
      loop
         Requests.Dequeue (Request);
         case Request.Operation is
            when Shutdown_Operation =>
               exit;  -- exit the main loop, thus shutting down the task

            when RM_Operation =>
               null;  -- for now

            when Help_Operation =>
               Help (Request);
         end case;
      end loop;
   end File_Task;

   ---------------------------------------------------------------------------

begin  -- package File initialization
   Init;
end File;
