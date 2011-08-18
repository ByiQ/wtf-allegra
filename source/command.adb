
--
-- Command -- Command-processing task package for Allegra info-bot
--


--
-- Standard packages
with Ada.Characters.Handling;
use  Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Strings.Maps;
use  Ada.Strings;


--
-- Compiler-specific packages
with GNAT.Regpat;
use  GNAT.Regpat;


--
-- Local library packages
with Identity;
with Strings;
use  Strings;
with Times;


--
-- Application packages
with Auth;
with Config;
with DatabaseQ;
with FileQ;
with Input;
with IRC;
with Log;
use  Log;
with NetQ;
with OutputQ;
with Ping;


--
-- Request-queue package
with CommandQ;


package body Command is

------------------------------------------------------------------------------
--
-- Package constants
--
------------------------------------------------------------------------------

   -- This task's name, for logging purposes
   Command_Name        : constant string := "Command";

   -- Nowadays, most clients use "_", but older RFCs (like 1459) have a more
   -- limited legal charset
   Nick_Extension_Char : constant character := '-';

   -- An action prefix
   ActPfx              : constant string := IRC.CTCP_Marker & "ACTION";

   -- These characters are ones that the bot will recognize as indicating a
   -- "direct address":  that is, "<nick><sep><command>".  They are ones IRC
   -- clients commonly insert after auto-completed nicks.
   Separators          : constant Maps.Character_Set := Maps.To_Set (",:~");

   -- Maximum number of parenthesized components in our command regexps.
   -- Increase this if any patterns in Command_Table ever get more complicated
   -- than that.
   Max_Matches         : constant := 4;

------------------------------------------------------------------------------
--
-- Package types
--
------------------------------------------------------------------------------

   -- Index type for parenthesized components in a command regexp
   subtype Match_Range is Match_Count range 1 .. Max_Matches;

   -- Yes, a channel message is a PRIVMSG too, in IRC terms.  But we like to
   -- distinguish those sent to the channel versus those sent to our nick.
   type Message_Type_Enm is ( PrivMsg, ChanMsg );

   -- Our command table is an array of tuples: (pointer to compiled command
   -- regexp, pointer to procedure that handles that command)
   type Matcher_Ptr is access Pattern_Matcher;
   type Command_Processor is access procedure (Cmd     : in string;
                                               Sender  : in IRC.MsgTo_Rec);
   type Command_Descriptor is record
      Matcher  : Matcher_Ptr;
      Process  : Command_Processor;
   end record;

   -- A "last" buffer, which holds the last N privmsgs sent to the channel.
   -- We use a pointer so we can size and allocate it based on a config value.
   type Line_Rec is record
      Stamp : Times.Timestamp;
      From  : UString;
      Msg   : UString;
   end record;
   type Line_Buf is array (positive range <>) of Line_Rec;
   type Line_Buf_Ptr is access Line_Buf;

------------------------------------------------------------------------------
--
-- Package variables
--
------------------------------------------------------------------------------

   -- Our current nick, either our nominal nick or a bashed form because of
   -- a collision
   Current_Nick     : UString;

   -- The master command table
   Command_Table    : array (Config.Valid_Commands) of Command_Descriptor;

   -- The command request we're processing at the moment, and some components
   -- of it
   Request          : CommandQ.Request_Rec;
   Sender           : IRC.MsgTo_Rec;
   Destination      : UString;

   -- Request variables for making requests to other tasks
   Database_Request : DatabaseQ.Request_Rec;
   File_Request     : FileQ.Request_Rec;
   Net_Request      : NetQ.Request_Rec;
   Output_Request   : OutputQ.Request_Rec;

   -- Set true when we are to exit this task (set by the "quit" command)
   Do_Exit          : boolean;

   -- Alternate command pattern for the "fetch" command.  What an ugly hack!
   -- If we ever want very many of these, we should turn the command pattern
   -- into a list of patterns.
   Pat_Fetch2       : Matcher_Ptr;

   -- Sub-patterns within some commands
   Pat_ActionSet    : Matcher_Ptr;
   Pat_AlsoSet      : Matcher_Ptr;
   Pat_ReplySet     : Matcher_Ptr;

   -- Variables to support the "last" command
   NLines           : natural;
   NextLine         : positive;
   Lines            : Line_Buf_Ptr;

   -- Internal command statistics
   Start            : Times.Timestamp;
   Last_Connected   : Times.Timestamp;
   Cmds_Accepted    : natural;
   Cmds_Rejected    : natural;
   Reconnects       : natural;

------------------------------------------------------------------------------
--
-- Package subroutines
--
------------------------------------------------------------------------------

   -- Return the IRC form of our assigned channel name
   function Channel return string is
   begin  -- Channel
      return "#" & Config.Get_Value (Config.Item_Channel);
   end Channel;

   ---------------------------------------------------------------------------

   -- Local instance of this routine, that sends to the nominal destination
   procedure Say (Msg : in string) is
   begin  -- Say
      OutputQ.Say (Msg, Destination);
   end Say;

   ---------------------------------------------------------------------------

   -- Return true if a message was directly addressed to us: starts with given
   -- nick, and nick ends with one of our favorite separator characters.
   function Leading_Nick (Msg : in string;   Nick:  in string) return boolean is
   begin  -- Leading_Nick
      if Msg'Length > Nick'Length + 2 then
         if Fixed.Index (To_Lower (Msg), Nick) = 1 and Maps.Is_In (Msg (Nick'Length + 1), Separators) then
            return true;
         end if;
      end if;
      return false;
   end Leading_Nick;

   ---------------------------------------------------------------------------

   -- Terminate all other tasks, thus allowing the bot to shut down once this
   -- task terminates, and do other wrap-up things.
   procedure Shutdown is
   begin  -- Shutdown

      -- Tell those tasks who know how to shut themselves down
      Database_Request.Operation := DatabaseQ.Shutdown_Operation;
      File_Request.Operation     := FileQ.Shutdown_Operation;
      DatabaseQ.Requests.Enqueue (Database_Request);
      FileQ.Requests.Enqueue (File_Request);

      -- Abort the tasks that don't have a request queue
      abort Input.Input_Task;
      abort Ping.Ping_Task;

      -- Give things time to settle
      delay 3.0;

      -- Save cached config items
      Config.WrapUp;

      -- Log final run statistics
      Log.Info (Command_Name, "Shutdown of      " & Identity.App_ID);
      Log.Info (Command_Name, "Runtime:         " & Times.Elapsed (Start));
      Log.Info (Command_Name, "Connected:       " & Times.Elapsed (Last_Connected));
      Log.Info (Command_Name, "Reconnects:      " & Img (Reconnects - 1));
      Log.Info (Command_Name, "Cmds processed:  " & Img (Cmds_Accepted));
      Log.Info (Command_Name, "Cmds rejected:   " & Img (Cmds_Rejected));

      -- Close the log file
      Log.WrapUp;
   end Shutdown;

   ---------------------------------------------------------------------------

   -- Check for a botsnack; respond and return true if it is
   function Is_Snack (Msg : in UString) return boolean is
   begin  -- Is_Snack
      if Ada.Strings.Unbounded.Index (Msg, "botsnack") > 0 or Ada.Strings.Unbounded.Index (Msg, "botsnak") > 0 then
         Database_Request.Operation := DatabaseQ.Snack_Operation;
         Database_Request.Destination := Destination;
         DatabaseQ.Requests.Enqueue (Database_Request);
         return true;
      else
         return false;
      end if;
   end Is_Snack;

   ---------------------------------------------------------------------------

   -- Print a level error message, with a distinguishing string
   procedure Level_Error (Level : in string;
                          Msg   : in string) is
   begin  -- Level_Error
      Say ("Hmm, the access level """ & Level & """ " & Msg & "." &
           "  It needs to be a decimal integer in the range " &
           Img (Config.Min_Auth_Level) & " .. " & Img (Config.Max_Auth_Level));
   end Level_Error;

   ---------------------------------------------------------------------------

   -- Check command access level by usermask
   procedure CkAccess (Cmd     : in string;
                       Sender  : in IRC.MsgTo_Rec) is

      use Ada.Strings.Unbounded;

      Matches : Match_Array (Match_Range);

   begin  -- CkAccess

      -- Re-match to pick up pattern elements
      Match (Command_Table (Config.Cmd_CkAccess).Matcher.all, Cmd, Matches);

      -- Extract the usermask arg and parse it so we can use its nick
      declare
         Mask : string := Cmd (Matches (1).First .. Matches (1).Last);
         Who  : IRC.MsgTo_Rec;
      begin
         IRC.Parse_MsgTo (US (Mask), Who);

         -- Of course, we *do* need a nick, and with this command, it's easy
         -- to forget it
         if Who.Nick = IRC.Null_Field then
            Say ("Got nick?");
            return;
         end if;

         -- Do the actual command function
         Say ("The current command access level for " & S (Who.Nick) & " is " &
              Img (Auth.Level (US (Mask))) & ".");
      end;
   end CkAccess;

   ---------------------------------------------------------------------------

   -- Process a "fetch factoid" command
   procedure Fetch  (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is

      use Ada.Strings.Unbounded;

      Fact    : UString;
      Matches : Match_Array (Match_Range);

   begin  -- Fetch

      -- Start out with no factoid argument
      Fact := Null_UString;

      -- See if we're handling the alternate form of the command, "what is/are
      -- <factoid>?"
      if Match (Pat_Fetch2.all, Cmd) = Cmd'First then

         -- Yes; re-match to extract the components, then assume that the
         -- factoid is the last component
         Match (Pat_Fetch2.all, Cmd, Matches);
         if    Matches (3) /= No_Match then
            Fact := US (Cmd (Matches (3).First .. Matches (3).Last));
         elsif Matches (2) /= No_Match then
            Fact := US (Cmd (Matches (2).First .. Matches (2).Last));
         end if;
      else

         -- No, this is the "<factoid>?" form; rematch to strip the "?"
         Match (Command_Table (Config.Cmd_Fetch).Matcher.all, Cmd, Matches);
         if Matches (1) /= No_Match then
            Fact := US (Cmd (Matches (1).First .. Matches (1).Last));
         end if;
      end if;

      -- If we have a factoid component by now, process it
      if Fact /= Null_UString then

         -- Strip trailing blanks and question marks; shouldn't actually be
         -- necessary, since the regexp should have already, but at some point
         -- during development we did need this, and it remains due to inertia
         while Length (Fact) > 0 and then (Element (Fact, Length (Fact)) = '?' or Element (Fact, Length (Fact)) = ' ') loop
            Fact := Head (Fact, Length (Fact) - 1);
         end loop;

         -- If we still have a factoid string, see if it starts with a tilde.
         -- If so, strip it and do a regexp lookup; if not, do regular lookup.
         if Length (Fact) > 0 then
            if Element (Fact, 1) = '~' then
               Database_Request.Operation := DatabaseQ.RE_Fetch_Operation;
               Database_Request.Data      := US (Slice (Fact, 2, Length (Fact)));
            else
               Database_Request.Operation := DatabaseQ.Fetch_Operation;
               Database_Request.Data      := Fact;
            end if;
         else
            -- Factoid dwindled to null after stripping, so just do a quip
            Database_Request.Operation := DatabaseQ.Quip_Operation;
         end if;

         -- Submit whatever sort of database request we have built so far
         Database_Request.Origin      := Sender.Nick;
         Database_Request.Destination := Destination;
         DatabaseQ.Requests.Enqueue (Database_Request);
      else
         -- Didn't manage to recognize the command pattern
         Say ("I can't quite make out your question--try again, maybe?");
      end if;
   end Fetch;

   ---------------------------------------------------------------------------

   -- Process the "find in RM" command, currently just does an RM index search
   procedure Find   (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is

      Matches : Match_Array (Match_Range);

   begin  -- Find

      -- Re-match to extract the components
      Match (Command_Table (Config.Cmd_Find).Matcher.all, Cmd, Matches);

      -- Check shouldn't be necessary, right?  The regexp wouldn't have
      -- matched in the first place, I'd think.  Maybe these can be removed in
      -- all the places they're present.
      if Matches (1) = No_Match then
         Say ("The RM-find command is ""find regexp"".");
         return;
      end if;

      -- Bundle the request and send it to the file task for processing.  Note
      -- that we use Sender.Nick instead of Destination, to force RM search
      -- output to always go to the requestor as a private message.
      File_Request.Operation   := FileQ.RM_Operation;
      File_Request.Data        := US (Cmd (Matches (1).First .. Matches (1).Last));
      File_Request.Destination := Sender.Nick;
      FileQ.Requests.Enqueue (File_Request);
   end Find;

   ---------------------------------------------------------------------------

   -- Process the "forget factoid" command
   procedure Forget (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is

      Matches : Match_Array (Match_Range);

   begin  -- Forget

      -- Re-match to extract the components
      Match (Command_Table (Config.Cmd_Forget).Matcher.all, Cmd, Matches);

      -- Check shouldn't be necessary, right?  The regexp wouldn't have
      -- matched in the first place, I'd think.  Maybe these can be removed in
      -- all the places they're present.
      if Matches (1) = No_Match then
         Say ("The forget-factoid command is ""forget factoid"".");
         return;
      end if;

      -- Bundle the request and send it to the database task for processing
      Database_Request.Operation   := DatabaseQ.Forget_Operation;
      Database_Request.Key         := US (Cmd (Matches (1).First .. Matches (1).Last));
      Database_Request.Origin      := Sender.Nick;
      Database_Request.Requestor   := Request.Origin;
      Database_Request.Destination := Destination;
      DatabaseQ.Requests.Enqueue (Database_Request);
   end Forget;

   ---------------------------------------------------------------------------

   -- Process the "help" command
   procedure Help   (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is

      Matches : Match_Array (Match_Range);

   begin  -- Help

      -- Re-match to extract the components
      Match (Command_Table (Config.Cmd_Help).Matcher.all, Cmd, Matches);

      -- Start formatting a request for the file task.  Note that we use
      -- Sender.Nick instead of Destination, to force help output to always go
      -- to the requestor as a private message.
      File_Request.Operation   := FileQ.Help_Operation;
      File_Request.Destination := Sender.Nick;

      -- Tell the file task whether we have an argument (help topic) or not
      if Matches (1) = No_Match then
         File_Request.Data     := Null_UString;
      else
         File_Request.Data     := US (BTrim (Cmd (Matches (1).First .. Matches (1).Last)));
      end if;

      -- Send the request to the file task for processing
      FileQ.Requests.Enqueue (File_Request);
   end Help;

   ---------------------------------------------------------------------------

   -- Process the "last" command (show recent channel activity)
   procedure Last   (Cmd    : in string;
                     Sender : in IRC.MsgTo_Rec) is

      ------------------------------------------------------------------------

      Matches : Match_Array (Match_Range);
      Count   : natural;
      Show    : integer;

      ------------------------------------------------------------------------

      -- List a saved message for the "last" operation
      procedure List_Line (Index : in positive) is

         Nick : string := S (Lines (Index).From);
         Line : UString := Lines (Index).Msg;

      begin  -- List_Line

         -- If the saved line is an action, print it as an action, otherwise
         -- as a message.  This mimics the output format of most common IRC
         -- clients.
         if S (Unbounded.Head (Line, ActPfx'Length)) = ActPfx then
            -- Delete the real action prefix and substitute " * nick"
            Unbounded.Delete (Line, 1, ActPfx'Length);
            Unbounded.Delete (Line, Unbounded.Length (Line), Unbounded.Length (Line));
            Line := " * " & Nick & Line;
         else
            Line := "<" & Nick & "> " & Line;
         end if;

         -- Prefix all lines with a timestamp, so you can tell when in the
         -- past stuff happened
         Line := Times.Time_String (Lines (Index).Stamp, Short_Format => true) & " " & Line;

         -- Send the line to the user.  Note that we use Sender.Nick instead
         -- of Destination, to force "last" output to always go to the
         -- requestor as a private message.
         OutputQ.Say (Line, Sender.Nick);
      end List_Line;

      ------------------------------------------------------------------------

   begin  -- Last

      -- Re-match to extract the components
      Match (Command_Table (Config.Cmd_Last).Matcher.all, Cmd, Matches);

      -- If user gave a count argument, use the smaller of that and the actual
      -- number of lines available.  If not, show all saved lines.  Because of
      -- the way they're saved, in a circular buffer, NLines will never be
      -- more than the configured maximum, so the default count is "max".
      if Matches (1) = No_Match then
         Count := NLines;
      else
         Count := natural'Min (NLines, positive'Value (Cmd (Matches (1).First .. Matches (1).Last)));
      end if;

      -- Log the event
      Dbg (Command_Name, "Listing last " & Img (Count) & " lines of " & Img (NLines));

      -- Figure out where in the circular buffer to start
      Show := NextLine - Count;
      if Show < Lines'First then
         Show := Show + NLines;
      end if;

      -- Show up to Count lines
      for L in 1 .. Count loop

         -- List the next line
         List_Line (Show);
         delay Config.Line_Pause;

         -- See if we need to wrap around to the start of the circular buffer,
         -- or just advance to the next entry
         if Show = Lines'Last then
            Show := Lines'First;
         else
            Show := Show + 1;
         end if;
      end loop;
   end Last;

   ---------------------------------------------------------------------------

   -- Process the "list factoids" command
   procedure List   (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is

      Matches : Match_Array (Match_Range);
      Pat     : UString;

   begin  -- List

      -- Re-match to extract the components
      Match (Command_Table (Config.Cmd_List).Matcher.all, Cmd, Matches);

      -- If we don't have a (regexp) argument, use ".", which matches
      -- everything
      if Matches (1) = No_Match then
         Pat := US (".");
      else
         Pat := US (BTrim (Cmd (Matches (1).First .. Matches (1).Last)));
      end if;

      -- Bundle the request and send it to the database task for processing
      Database_Request.Operation   := DatabaseQ.List_Operation;
      Database_Request.Data        := Pat;
      Database_Request.Destination := Destination;
      DatabaseQ.Requests.Enqueue (Database_Request);
   end List;

   ---------------------------------------------------------------------------

   -- Process the form of the access command that checks one's own command
   -- access level, which is just "access"
   procedure MyAccess (Cmd     : in string;
                       Sender  : in IRC.MsgTo_Rec) is
   begin  -- MyAccess
      Say ("Your current command access level, " & S (Sender.Nick) & ", is " &
           Img (Auth.Level (Request.Origin)) & ".");
   end MyAccess;

   ---------------------------------------------------------------------------

   -- Process the "quit" command
   procedure Quit   (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is
   begin  -- Quit

      -- Log the event
      Info (Command_Name, "Executing shutdown request from " & S (Request.Origin));

      -- Shut down all the other tasks and wrap up
      Shutdown;

      -- Set the flag that causes this task to exit
      Do_Exit := true;
   end Quit;

   ---------------------------------------------------------------------------

   -- Process the "quit" command
   procedure Quote  (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is
   begin  -- Quote

      -- Bundle the request and send it to the database task for processing
      Database_Request.Operation   := DatabaseQ.Quote_Operation;
      Database_Request.Destination := Destination;
      DatabaseQ.Requests.Enqueue (Database_Request);
   end Quote;

   ---------------------------------------------------------------------------

   -- Process the "rename factoid" command
   procedure Rename (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is

      Matches : Match_Array (Match_Range);

   begin  -- Rename

      -- Re-match to extract the components
      Match (Command_Table (Config.Cmd_Rename).Matcher.all, Cmd, Matches);

      -- Check shouldn't be necessary, right?  The regexp wouldn't have
      -- matched in the first place, I'd think.  Maybe these can be removed in
      -- all the places they're present.
      if Matches (1) = No_Match or Matches (3) = No_Match then
         Say ("The rename-factoid command is ""rename oldname to newname"".");
         return;
      end if;

      -- Bundle the request and send it to the database task for processing
      Database_Request.Operation   := DatabaseQ.Rename_Operation;
      Database_Request.Key         := US (Cmd (Matches (1).First .. Matches (1).Last));
      Database_Request.Data        := US (Cmd (Matches (3).First .. Matches (3).Last));
      Database_Request.Origin      := Sender.Nick;
      Database_Request.Requestor   := Request.Origin;
      Database_Request.Destination := Destination;
      DatabaseQ.Requests.Enqueue (Database_Request);
   end Rename;

   ---------------------------------------------------------------------------

   -- Process the "reset factoid" command ("no, factoid is def")
   procedure Reset    (Cmd     : in string;
                       Sender  : in IRC.MsgTo_Rec) is

      Matches : Match_Array (Match_Range);

   begin  -- Reset

      -- Re-match to extract the components
      Match (Command_Table (Config.Cmd_Reset).Matcher.all, Cmd, Matches);

      -- Check shouldn't be necessary, right?  The regexp wouldn't have
      -- matched in the first place, I'd think.  Maybe these can be removed in
      -- all the places they're present.
      if Matches (1) = No_Match or Matches (3) = No_Match then
         Say ("Is that supposed to be a reset-factoid command?  If so, it should be ""no, factoid is definition"".");
         return;
      end if;

      -- Bundle the request and send it to the database task for processing
      declare
         Fact : string := Cmd (Matches (1).First .. Matches (1).Last);
         To   : string := Cmd (Matches (3).First .. Matches (3).Last);
      begin
         Database_Request.Operation   := DatabaseQ.ResetFactoid_Operation;
         Database_Request.Key         := US (Fact);
         Database_Request.Data        := US (To);
         Database_Request.Origin      := Sender.Nick;
         Database_Request.Requestor   := Request.Origin;
         Database_Request.Destination := Destination;
         DatabaseQ.Requests.Enqueue (Database_Request);
      end;
   end Reset;

   ---------------------------------------------------------------------------

   -- Process the "set factoid" command
   procedure Set    (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is

      Matches    : Match_Array (Match_Range);

   begin  -- Set

      -- Re-match to extract the components
      Match (Command_Table (Config.Cmd_Set).Matcher.all, Cmd, Matches);

      -- Check shouldn't be necessary, right?  The regexp wouldn't have
      -- matched in the first place, I'd think.  Maybe these can be removed in
      -- all the places they're present.
      if Matches (1) = No_Match or Matches (3) = No_Match then
         Say ("Is that supposed to be a set-factoid command?  If so, it should be ""factoid is definition"".");
         return;
      end if;

      -- Analyze our arguments to see what kind of database request to send
      declare
         Fact : string := Cmd (Matches (1).First .. Matches (1).Last);
         To   : string := Cmd (Matches (3).First .. Matches (3).Last);
      begin

         -- The "factoid is also def" adds a new definition
         if Match (Pat_AlsoSet.all, To) = To'First then
            Match (Pat_AlsoSet.all, To, Matches);
            Database_Request.Operation   := DatabaseQ.AddFactoid_Operation;
            Database_Request.Data        := US (To (Matches (1).First .. Matches (1).Last));

         -- The "factoid is action <action>" sets an action as a factoid response
         elsif Match (Pat_ActionSet.all, To) = To'First then
            Match (Pat_ActionSet.all, To, Matches);
            Database_Request.Operation   := DatabaseQ.SetAction_Operation;
            Database_Request.Data        := US (To (Matches (1).First .. Matches (1).Last));

         -- The "factoid is reply <reply>" sets a fixed reply as a factoid response
         elsif Match (Pat_ReplySet.all, To) = To'First then
            Match (Pat_ReplySet.all, To, Matches);
            Database_Request.Operation   := DatabaseQ.SetReply_Operation;
            Database_Request.Data        := US (To (Matches (1).First .. Matches (1).Last));

         -- Simple "factoid is def" form
         else
            Database_Request.Operation   := DatabaseQ.SetFactoid_Operation;
            Database_Request.Data        := US (To);
         end if;

         -- Finish the request and send it to the database task for processing
         Database_Request.Key         := US (Fact);
         Database_Request.Origin      := Sender.Nick;
         Database_Request.Destination := Destination;
         DatabaseQ.Requests.Enqueue (Database_Request);
      end;
   end Set;

   ---------------------------------------------------------------------------

   -- Process the form of the access command that sets a command access level
   -- for a usermask, which is "access usermask level"
   procedure SetAccess (Cmd     : in string;
                       Sender  : in IRC.MsgTo_Rec) is

      Matches : Match_Array (Match_Range);

   begin  -- SetAccess

      -- Re-match to extract the components
      Match (Command_Table (Config.Cmd_SetAccess).Matcher.all, Cmd, Matches);

      -- Extract the arguments
      declare
         Mask  : string := Cmd (Matches (1).First .. Matches (1).Last);
         Level : string := Cmd (Matches (2).First .. Matches (2).Last);
      begin

         -- Ensure that the level argument is valid and in range
         if natural'Value (Level) not in Config.Auth_Level then
            Level_Error (Level, "seems to be out of range");
            return;
         end if;

         -- Submit the operation to the database task
         Database_Request.Operation := DatabaseQ.Access_Operation;
         Database_Request.Origin    := Sender.Nick;
         Database_Request.Key       := US (Mask);
         Database_Request.Data      := US (Level);
         DatabaseQ.Requests.Enqueue (Database_Request);
      exception
         -- Mostly this will catch constraint errors from the Value call above
         when others =>
            Level_Error (Level, "didn't sit too well with me");
      end;
   end SetAccess;

   ---------------------------------------------------------------------------

   -- Process the "shorten URL" command
   procedure Short  (Cmd     : in String;
                     Sender  : in IRC.MsgTo_Rec) is

      Matches : Match_Array (Match_Range);

   begin  -- Short

      -- Re-match to extract the components
      Match (Command_Table (Config.Cmd_Short).Matcher.all, Cmd, Matches);

      -- Check shouldn't be necessary, right?  The regexp wouldn't have
      -- matched in the first place, I'd think.  Maybe these can be removed in
      -- all the places they're present.
      if Matches (1) = No_Match then
         Say ("The shorten-URL command is ""short URL"".");
         return;
      end if;

      -- Bundle the request and send it to the net task for processing.
      Net_Request.Operation   := NetQ.Shorten_URL_Operation;
      Net_Request.Data        := US (Cmd (Matches (1).First .. Matches (1).Last));
      Net_Request.Destination := Destination;
      NetQ.Requests.Enqueue (Net_Request);
   end Short;

   ---------------------------------------------------------------------------

   -- Process the "stats" command
   procedure Stats  (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is

      Matches : Match_Array (Match_Range);
      Msg     : UString;

   begin  -- Stats

      -- Re-match to extract the components
      Match (Command_Table (Config.Cmd_Stats).Matcher.all, Cmd, Matches);

      -- With no arguments, it's a request for bot operation statistics
      if Matches (1) = No_Match then

         -- First, print the stats we know in this task
         Say ("I am " & Identity.App_ID);
         delay Config.Line_Pause;
         Say ("I have currently been running for " & Times.Elapsed (Start) & ".");
         delay Config.Line_Pause;
         Msg := US ("I've been connected for " & Times.Elapsed (Last_Connected));
         if Reconnects = 1 then
            Msg := Msg & "; this is my first connect.";
         elsif Reconnects = 2 then
            Msg := Msg & ", after one reconnect.";
         else
            Msg := Msg & ", after " & Img (Reconnects - 1) & " reconnects.";
         end if;
         Say (S (Msg));
         delay Config.Line_Pause;
         Msg := US ("I've accepted " & Img (Cmds_Accepted) & " command");
         if Cmds_Accepted > 1 then
            Msg := Msg & "s";
         end if;
         Msg := Msg & ", including this one, and rejected " & Img (Cmds_Rejected) & ".";
         Say (S (Msg));
         delay Config.Line_Pause;

         -- Now ask the file and database tasks to print the statistics that
         -- they know about
         Database_Request.Operation := DatabaseQ.Stats_Operation;  -- operation is enqueued later
         File_Request.Operation     := FileQ.Stats_Operation;
         File_Request.Destination   := Destination;
         FileQ.Requests.Enqueue (File_Request);

      -- With an argument, it's either "stats <factoid>" or "stats commands"
      else
         declare
            About : string := BTrim (Cmd (Matches (1).First .. Matches (1).Last));
         begin

            -- If it's the magic keyword "commands", print command-usage
            -- statistics, which are kept in a table in the Config package.
            -- Note that we use Sender.Nick instead of Destination, to force
            -- command stats output to always go to the requestor as a private
            -- message.
            if To_Lower (About) = "commands" then
               OutputQ.Say ("Command statistics:", Sender.Nick);
               for VCmd in Config.Valid_Commands loop
                  OutputQ.Say ("   " & Config.Cmd_Names (VCmd) & Img (Config.Command_Usage (VCmd), 4), Sender.Nick);
                  delay Config.Line_Pause;
               end loop;

               -- Return directly, since we don't want to send a database
               -- request for "stats commands"
               return;
            else

               -- Not a magic keyword, treat as a factoid name
               Database_Request.Key         := US (About);
               Database_Request.Operation   := DatabaseQ.FactoidStats_Operation;
            end if;
         end;
      end if;

      -- Finish the database request and send it to the database task
      Database_Request.Destination := Destination;
      DatabaseQ.Requests.Enqueue (Database_Request);
   end Stats;

   ---------------------------------------------------------------------------

   -- Process the "tell" command
   procedure Tell   (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is

      Matches : Match_Array (Match_Range);

   begin  -- Tell

      -- Re-match to extract the components
      Match (Command_Table (Config.Cmd_Tell).Matcher.all, Cmd, Matches);

      -- Check shouldn't be necessary, right?  The regexp wouldn't have
      -- matched in the first place, I'd think.  Maybe these can be removed in
      -- all the places they're present.
      if Matches (1) = No_Match or Matches (3) = No_Match then
         Say ("That doesn't quite make sense to me ... try again, maybe?");
         return;
      end if;

      -- Examine our arguments to see what to do
      declare
         To   : string := Cmd (Matches (1).First .. Matches (1).Last);
         Fact : string := Cmd (Matches (3).First .. Matches (3).Last);
      begin

         -- A "tell bot-nick" isn't terribly useful
         if To_Lower (To) = To_Lower (S (Current_Nick)) then
            Say ("Hey, I already know that!");
            return;

         -- The magic keyword "me" means "send me a factoid def as a private
         -- message", hence our use of Sender.Nick as the destination.
         elsif To_Lower (To) = "me" then
            Database_Request.Destination := Sender.Nick;

         -- Destination must be somebody else's nick
         else
            Database_Request.Destination := US (To);
         end if;

         -- See if factoid name is a regexp
         if Fact (Fact'First) = '~' then
            Database_Request.Operation := DatabaseQ.RE_Tell_Operation;
            Database_Request.Data      := US (Fact (Fact'First + 1 .. Fact'Last));
         else
            Database_Request.Operation := DatabaseQ.Tell_Operation;
            Database_Request.Data      := US (Fact);
         end if;

         -- Finish the database request and send it to the database task
         Database_Request.Origin       := Sender.Nick;
         DatabaseQ.Requests.Enqueue (Database_Request);
      end;
   end Tell;

   ---------------------------------------------------------------------------

   -- Treat any random string that's not a command as a factoid to be fetched
   procedure Fetch_Bare (Cmd     : in string;
                         Sender  : in IRC.MsgTo_Rec) is
   begin  -- Fetch_Bare

      -- Okay, as a concession to IRC bot customs, let's do this minor check
      -- first
      if Is_Snack (US (Cmd)) then
         return;
      end if;

      -- Not a botsnack, treat as a factoid fetch
      Config.Command_Used (Config.Cmd_Fetch);

      -- See if factoid name is a regexp
      if Cmd (Cmd'First) = '~' then
         Database_Request.Operation := DatabaseQ.RE_Fetch_Operation;
         Database_Request.Data      := US (Cmd (Cmd'First + 1 .. Cmd'Last));
      else
         Database_Request.Operation := DatabaseQ.Fetch_Operation;
         Database_Request.Data      := US (Cmd);
      end if;

      -- Finish the database request and send it to the database task
      Database_Request.Origin       := Sender.Nick;
      Database_Request.Destination  := Destination;
      DatabaseQ.Requests.Enqueue (Database_Request);
   end Fetch_Bare;

   ---------------------------------------------------------------------------

   -- Handle all CTCP (client-to-client protocol) messages directed at us
   procedure Process_CTCP_Request (Cmd     : in string;
                                   Sender  : in IRC.MsgTo_Rec) is

      ------------------------------------------------------------------------

      Tgt : string := S (Destination);

      ------------------------------------------------------------------------

      -- Return true if the substring of Cmd after the CTCP marker is equal to
      -- the given keyword
      function Keywd (K : in string) return boolean is

         Req : string := Cmd (Cmd'First + 1 .. Cmd'Last);

      begin  -- Keywd
         return Req'Length >= K'Length and then Req (Req'First .. Req'First + K'Length - 1) = K;
      end Keywd;

      ------------------------------------------------------------------------

   begin  -- Process_CTCP_Request

      -- Freenode sends us a CTCP VERSION as soon as we register, so answer it
      -- (and anybody else who asks our version) intelligently
      if Keywd ("VERSION") then
         OutputQ.Note (IRC.CTCP_Marker & "VERSION " & Identity.App_ID & IRC.CTCP_Marker, Tgt);

      -- Standard answer to CTCP ACTION (but can this ever happen?)
      elsif Keywd ("TIME") then
         OutputQ.Note (IRC.CTCP_Marker & "TIME : Here in " & Config.Get_Value (Config.Item_CTCP_Location) & ", it is " &
                        Fixed.Trim (Times.Date_String & " " & Times.Time_String, Side => Both) &
                        IRC.CTCP_Marker, Tgt);

      -- Standard answer to CTCP ACTION (but can this ever happen?)
      elsif Keywd ("ACTION") then
         OutputQ.Note (IRC.CTCP_Marker & "ACTION don't play dat!" & IRC.CTCP_Marker, Tgt);

      -- Send standard ERRMSG reply to all other CTCP requests.  This is where
      -- things like FINGER, USERINFO, PING, etc., could be added.
      else
         OutputQ.Note (IRC.CTCP_Marker & "ERRMSG Sorry, I'm not that kind of bot." & IRC.CTCP_Marker, Tgt);
      end if;
   end Process_CTCP_Request;

   ---------------------------------------------------------------------------

   -- We've recognized a message as a bot command, so act on it
   procedure Process_Command (Cmd:  in string;   Sender:  in IRC.MsgTo_Rec) is

      ------------------------------------------------------------------------

      -- Found command in lookup table; verify that user is authorized to
      -- execute it, and if so, do it
      procedure Exec (CmdType : in Config.Command_Type;   Proc : in Command_Processor) is

         use Auth, Config;

         Authorized : Authorization;
         To         : string := S (Destination);

      begin  -- Exec

         -- Is this user permitted (by command auth level) to execute this
         -- particular command?
         Authorized := Permitted (Request.Origin, CmdType);

         -- If so, go ahead and call the handler procedure found in the
         -- command table
         if Authorized = Succeeded then
            Cmds_Accepted := Cmds_Accepted + 1;
            Config.Command_Used (CmdType);
            Proc (Cmd, Sender);

         -- User doesn't have a high enough auth level for this command; log
         -- the event, and print a command-specific rejection message back to
         -- the user.  These messages make assumptions about the configured
         -- levels for each command, so if those change radically, these
         -- messages may start being rather comical, if not badly misleading.
         -- They should probably be put in as a field in the auth-levels table
         -- in the db, so they can be adjusted at the same time as the levels
         -- themselves.
         else
            Info (Command_Name, "Rejecting command """ & Cmd & """ from " & S (Request.Origin) & " because " &
                  Authorization'Image (Authorized));
            Cmds_Rejected := Cmds_Rejected + 1;
            case CmdType is
               when Cmd_CkAccess | Cmd_Fetch | Cmd_Find | Cmd_Help | Cmd_List | Cmd_Last | Cmd_Short =>
                  -- These commands are assumed to require only the default
                  OutputQ.Say ("You must have pissed somebody off, cuz you're persona non grata.", To);
               when Cmd_MyAccess =>
                  OutputQ.Say ("Why don't you just ask them what their access level is?", To);
               when Cmd_SetAccess =>
                  OutputQ.Say ("Only my operator can do that, sorry.", To);
               when Cmd_Forget | Cmd_Rename | Cmd_Reset | Cmd_Set =>
                  OutputQ.Say ("Only known users can update the factoid database, sorry.", To);
               when Cmd_Quit =>
                  OutputQ.Say ("I'm sorry " & S (Sender.Nick) & ", I don't know you well enough to take orders from you.", To);
               when Cmd_Quote =>
                  OutputQ.Say ("You need a higher access level--ask the bot operator about it.", To);
               when Cmd_Stats =>
                  OutputQ.Say ("You'll be able to access the stats soon enough, if you hang around and contribute.", To);
               when Cmd_Tell =>
                  OutputQ.Say ("That's pretty personal, isn't it?  Let's give you a while, then we'll see.", To);
               when Cmd_None =>
                  -- This is a "shouldn't happen", but at least we can see it
                  -- if it ever does
                  OutputQ.Say ("This is another fine mess you've gotten us into, Stanley!", To);
            end case;
         end if;
      end Exec;

      ------------------------------------------------------------------------

   begin  -- Process_Command

      -- Report action if debugging
      Dbg (Command_Name, "Processing command """ & Cmd & """ from " & S (Sender.Nick));

      -- Null strings can't be commands
      if Cmd'Length < 1 then
         return;
      end if;

      -- Catch CTCP commands
      if Cmd'Length > 2 and then Cmd (Cmd'First) = IRC.CTCP_Marker then
         Process_CTCP_Request (Cmd, Sender);
         return;
      end if;

      -- See if it matches the "what is" form for a fetch
      if Match (Pat_Fetch2.all, Cmd) = Cmd'First then
         Exec (Config.Cmd_Fetch, Fetch'Access);
         return;
      end if;

      -- None of the above; look it up in the command table; skip commands
      -- that aren't associated with patterns
      for CmdType in Command_Table'Range loop
         if Command_Table (CmdType).Matcher /= null and then
              Match (Command_Table (CmdType).Matcher.all, Cmd) = Cmd'First then
            Exec (CmdType, Command_Table (CmdType).Process);
            return;
         end if;
      end loop;

      -- Not in the table, treat as a factoid to be fetched
      Fetch_Bare (Cmd, Sender);
   end Process_Command;

   ---------------------------------------------------------------------------

   -- Compile all the regexps we use during command recognition
   procedure Parser_Init is

      ------------------------------------------------------------------------

      use Config;

      ------------------------------------------------------------------------

      -- Set an entry for given command in the command table
      procedure Enter (Cmd      : in Command_Type;
                       Pat      : in string;
                       Proc     : in Command_Processor) is
      begin  -- Enter
         Command_Table (Cmd) := (new Pattern_Matcher'(Compile (Pat,  Case_Insensitive)), Proc);
      end Enter;

      ------------------------------------------------------------------------

   begin  -- Parser_Init

      -- Fill the command table with match patterns and pointers to their
      -- associated handler procedures
      Enter (Cmd_CkAccess,  "^access\s+(\S+)$",                                    CkAccess'Access);
      Enter (Cmd_Fetch,     "^(\S.*)\s*(\?)$",                                     Fetch'Access);
      Enter (Cmd_Find,      "^find\s+(\S.*)$",                                     Find'Access);
      Enter (Cmd_Forget,    "^forget\s+(\S.*)$",                                   Forget'Access);
      Enter (Cmd_Help,      "^help(\s+\S.*)?$",                                    Help'Access);
      Enter (Cmd_Last,      "^last(\s+[1-9]\d*)?$",                                Last'Access);
      Enter (Cmd_List,      "^list(\s+\S.*)?$",                                    List'Access);
      Enter (Cmd_MyAccess,  "^access$",                                            MyAccess'Access);
      Enter (Cmd_Quit,      "^quit$",                                              Quit'Access);
      Enter (Cmd_Quote,     "^quote$",                                             Quote'Access);
      Enter (Cmd_Rename,    "^rename\s+(\S.*)\s+(as|to)\s+(\S.*)$",                Rename'Access);
      Enter (Cmd_Reset,     "^no,\s*(\S.*?)\s+(is|are)\s+(\S.*)$",                 Reset'access);
      Enter (Cmd_Set,       "^(\S.*?)\s+(is|are)\s+(\S.*)$",                       Set'Access);
      Enter (Cmd_SetAccess, "^access\s+(\S+)\s+(\d+)$",                            SetAccess'Access);
      Enter (Cmd_Short,     "^short\s+(\S.*)$",                                    Short'Access);
      Enter (Cmd_Stats,     "^stats(\s+\S.*)?$",                                   Stats'Access);
      Enter (Cmd_Tell,      "^tell\s+(\S+)\s+(about\s+)?(\S.*)$",                  Tell'Access);

      -- This is an alternate form of the fetch command, checked explicitly
      -- before the regular match loop
      Pat_Fetch2    := new Pattern_Matcher'(Compile ("^(what)\s+(is\s+|are\s+)?(\S.*)$",  Case_Insensitive));

      -- Some useful command argument patterns
      Pat_ActionSet := new Pattern_Matcher'(Compile ("^action\s+(\S.*)$",                 Case_Insensitive));
      Pat_AlsoSet   := new Pattern_Matcher'(Compile ("^also\s+(\S.*)$",                   Case_Insensitive));
      Pat_ReplySet  := new Pattern_Matcher'(Compile ("^reply\s+(\S.*)$",                  Case_Insensitive));
   end Parser_Init;

   ---------------------------------------------------------------------------

   -- This procedure is called if we get an exception in the command task
   -- itself, or if we get a "crash" request from another task, usually
   -- because they have had an unhandled exception themselves.
   procedure Do_Crash is
   begin  -- Do_Crash
      OutputQ.Say ("I'm not feeling well ... think I'll go lie down.", Channel);
      Shutdown;
   end Do_Crash;

------------------------------------------------------------------------------
--
-- Public task
--
------------------------------------------------------------------------------

   -- This is the command task
   task body Command_Task is

      use CommandQ;

      Msg_Type : Message_Type_Enm;

   begin  -- Command_Task

      -- Set up the regexps
      Parser_Init;

      -- Start with our configured nick
      Current_Nick := US (Config.Get_Value (Config.Item_Nick));

      -- Initialize local statistics variables
      Start          := Times.Current;
      Last_Connected := Times.Current;
      Cmds_Accepted  := 0;
      Cmds_Rejected  := 0;
      Reconnects     := 0;

      -- Set up the (empty) circular buffer for the "last" command
      NLines    := 0;
      NextLine  := 1;
      Lines     := new Line_Buf (1 .. positive'Value (Config.Get_Value (Config.Item_LastSize)));

      -- Main task processing loop
      loop

         -- Fetch next request from our request queue and handle it
         Requests.Dequeue (Request);
         case Request.Operation is

            -- Log in to the IRC server
            when Login_Operation =>

               -- Begin login sequence by sending NICK message
               Info (Command_Name, "Log in as " & S (Current_Nick));
               Output_Request.Operation := OutputQ.Nick_Operation;
               Output_Request.Data      := Current_Nick;
               OutputQ.Requests.Enqueue (Output_Request);
               Output_Request.Operation := OutputQ.User_Operation;

               -- Let the server think about that for a bit; required for pircd, at least
               delay 3.0;

               -- Now send the rest of the login sequence
               Output_Request.Operation := OutputQ.User_Operation;
               Output_Request.Data      := US (Config.Get_Value (Config.Item_UserName) &
                                               " 0 * :" & Config.Get_Value (Config.Item_RealName));
               OutputQ.Requests.Enqueue (Output_Request);

            -- Now that the input task handles pings directly, nobody sends
            -- pings to us any more, so this can probably be removed
            when Ping_Operation =>
               Dbg (Command_Name, "Ping with " & S (Request.Data));
               Output_Request.Operation := OutputQ.Ping_Operation;
               Output_Request.Data      := Request.Data;
               OutputQ.Requests.Enqueue (Output_Request);

            -- Here's our main brains: IRC messages and actions come here
            when Message_Operation | Save_Operation =>

               -- Grab several interesting values to use during our analysis
               declare
                  Command    : UString;
                  Has_Prefix : boolean;
                  Is_Command : boolean;
                  Message    : string := S (Request.Data);
                  My_Nick    : string := To_Lower (S (Current_Nick));
                  Shorthand  : string := Config.Get_Value (Config.Item_Shorthand);
               begin

                  -- Split the origin field into its parts, and put them into
                  -- a global so the separate command handlers can see them
                  IRC.Parse_MsgTo (Request.Origin, Sender);

                  -- See if this message was directed at the bot, either
                  -- because it starts with the bot's nick, or with the
                  -- configured shorthand string.
                  Has_Prefix := false;
                  Command := US (Message);
                  if Shorthand'Length > 0 and then Fixed.Index (Message, Shorthand) = 1 then
                     -- Starts with shorthand, trim it and note that we have a prefix
                     Has_Prefix := true;
                     Command := US (Message (Shorthand'Length + 1 .. Message'Length));
                  elsif Leading_Nick (Message, My_Nick) then
                     -- Starts with <nick><sep>, trim it and note that we have a prefix
                     Has_Prefix := true;
                     Command := US (Message (My_Nick'Length + 2 .. Message'Length));
                  end if;

                  -- Strip blanks from both ends of the command string
                  Command := Unbounded.Trim (Command, Side => Both);

                  -- Determine whether it's a channel message, or private to the bot
                  Is_Command := false;
                  if To_Lower (S (Request.Target)) = My_Nick then
                     -- Private message to the bot, always treat it as a
                     -- command, and arrange to send the output back the same
                     -- way
                     Is_Command := true;
                     Msg_Type := PrivMsg;
                     Destination := Sender.Nick;
                  else
                     -- Channel message, it's a command if it starts with one
                     -- of our prefix strings.  Set output destination to
                     -- channel.
                     Is_Command := Has_Prefix;
                     Msg_Type := ChanMsg;
                     Destination := Request.Target;
                  end if;

                  -- Since we added the "last" command, we now get *every*
                  -- message that is sent to the channel, instead of having
                  -- ones that didn't contain the bot's nick or the shorthand
                  -- string filtered out by the input task.  But he still
                  -- makes that check, and sends such messages as a "save"
                  -- instead of a "message" operation.  So at least we know
                  -- which messages we might have to execute.
                  if Is_Command and Request.Operation /= Save_Operation then
                     Do_Exit := false;
                     Process_Command (S (Command), Sender);
                     exit when Do_Exit;

                  -- If it didn't qualify as a command somehow, then save it
                  -- for later retrieval by the "last" command.
                  else

                     -- Put entry into circular buffer
                     Lines (NextLine) := (Times.Current, Sender.Nick, Command);

                     -- If the buffer isn't full yet, then advance the count
                     -- of available lines.  Once we reach that limit, NLines
                     -- stays there, and we overwrite old entries.
                     if NLines < Lines'Length then
                        NLines := NLines + 1;
                     end if;

                     -- Classic circular-buffer logic: if at end of buffer,
                     -- wrap to beginning, else advance to next slot.
                     if NextLine < Lines'Last then
                        NextLine := NextLine + 1;
                     else
                        NextLine := Lines'First;
                     end if;

                     -- If it's not a command, but also not a save, then we
                     -- got here because the message contained the bot's nick,
                     -- but wasn't addressed directly to the bot.  Check for
                     -- botsnacks, then if it's not one of those, tell the
                     -- database task to think about doing a quip.
                     if Request.Operation /= Save_Operation then
                        if not Is_Snack (Command) then
                           Database_Request.Operation := DatabaseQ.Quip_Operation;
                           Database_Request.Destination := Destination;
                           DatabaseQ.Requests.Enqueue (Database_Request);
                        end if;
                     end if;
                  end if;
               end;

            -- Handle NOTICE messages.  Currently, the only one we care about
            -- is from nickserv asking us to identify.
            when Notice_Operation =>
               -- Notice from NickServ about identification causes us to try
               -- to identify to it.
               if Fixed.Index (To_Lower (S (Request.Origin)), "nickserv") > 0 and
                  Fixed.Index (To_Lower (S (Request.Data)), Config.Get_Value (Config.Item_Nickserv_Notice)) > 0 then
                  OutputQ.Say ("identify " & Config.Get_Value (Config.Item_NickPass), "nickserv");
               end if;

            -- Somebody (us or another task) wants us to quit
            when Crash_Operation =>
               Err (Command_Name, "Crashed:  " & S (Request.Data));
               Do_Crash;
               exit;

            -- Handle IRC numeric server replies.  Note that we just ignore
            -- ones we don't care about.
            when Reply_Operation =>
               if Request.Reply = IRC.RPL_ENDOFMOTD or Request.Reply = IRC.ERR_NOMOTD then
                  -- End of MOTD, or missing MOTD, is our signal that the
                  -- server has shut up for now, and it's time to try joining
                  -- our channel
                  Dbg (Command_Name, "Done with MOTD, joining " & Channel);
                  Output_Request.Operation := OutputQ.Join_Operation;
                  Output_Request.Data      := US (Channel);
                  OutputQ.Requests.Enqueue (Output_Request);
               elsif Request.Reply = IRC.RPL_WELCOME then
                  -- The welcome message tells us that we've succeeded in
                  -- reconnecting (and registering, which to us is the real
                  -- thing)
                  Reconnects := Reconnects + 1;
                  Dbg (Command_Name, "Completed reconnect #" & Img (Reconnects));
                  Last_Connected := Times.Current;
               elsif Request.Reply = IRC.ERR_NICKNAMEINUSE then
                  -- Oops, somebody using our nick; use an alternate.  If the
                  -- network has nickserv, try ghosting it; if not, live with
                  -- the alternate.  (FIXME: not quite done yet)
                  Current_Nick := Current_Nick & Nick_Extension_Char;
                  Info (Command_Name, "Nick collision, trying " & S (Current_Nick));
                  Request.Operation := Login_Operation;
                  Requests.Enqueue (Request);
               end if;
         end case;
      end loop;

   exception
      -- Unexpected exceptions in the command-processing code cause the bot to
      -- terminate abruptly, yet in a semi-controlled fashion, by logging the
      -- exception and doing a controlled shutdown.
      when E : others =>
         Err (Command_Name, "Exception:  " & Ada.Exceptions.Exception_Information (E));
         Do_Crash;
   end Command_Task;

   ---------------------------------------------------------------------------

end Command;
