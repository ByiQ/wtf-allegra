
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
with Strings;
use  Strings;
with Times;


--
-- Application packages
with Auth;
with Config;
with Database;
with File;
with Identity;
with Input;
with Log;
use  Log;
with Output;
with Ping;


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

   Separators          : constant Maps.Character_Set := Maps.To_Set (",:~");
   Max_Matches         : constant := 4;

------------------------------------------------------------------------------
--
-- Package types
--
------------------------------------------------------------------------------

   subtype Match_Range is Match_Count range 1 .. Max_Matches;

   type Message_Type_Enm is ( PrivMsg, ChanMsg );

   type Matcher_Ptr is access Pattern_Matcher;
   type Command_Processor is access procedure (Cmd     : in string;
                                               Sender  : in IRC.MsgTo_Rec);
   type Command_Descriptor is record
      Matcher  : Matcher_Ptr;
      Process  : Command_Processor;
   end record;

   -- A "last" buffer, which holds the last N privmsgs sent to the channel
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

   Command_Table    : array (Config.Valid_Commands) of Command_Descriptor;
   Database_Request : Database.Request_Rec;
   Destination      : UString;
   Do_Exit          : boolean;
   File_Request     : File.Request_Rec;
   Msg_Type         : Message_Type_Enm;
   Output_Request   : Output.Request_Rec;
   Pat_ActionSet    : Matcher_Ptr;
   Pat_AlsoSet      : Matcher_Ptr;
   Pat_Fetch2       : Matcher_Ptr;
   Pat_ReplySet     : Matcher_Ptr;
   Request          : Request_Rec;
   Sender           : IRC.MsgTo_Rec;
   Current_Nick     : UString;
   NLines           : natural;
   NextLine         : positive;
   Lines            : Line_Buf_Ptr;

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

   function Channel return string is
   begin  -- Channel
      return "#" & Config.Get_Value (Config.Item_Channel);
   end Channel;

   ---------------------------------------------------------------------------

   -- Local instance of this that sends to the nominal destination
   procedure Say (Msg : in string) is
   begin  -- Say
      Output.Say (Msg, Destination);
   end Say;

   ---------------------------------------------------------------------------

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
   -- task terminates
   procedure Shutdown is
   begin  -- Shutdown
      Database_Request.Operation := Database.Shutdown_Operation;
      File_Request.Operation     := File.Shutdown_Operation;
      Database.Requests.Enqueue (Database_Request);
      File.Requests.Enqueue (File_Request);
      abort Input.Input_Task;
      abort Ping.Ping_Task;
      delay 3.0;
      Config.WrapUp;
      Log.WrapUp;
   end Shutdown;

   ---------------------------------------------------------------------------

   -- Check for a botsnack; respond and return true if it is
   function Is_Snack (Msg : in UString) return boolean is
   begin  -- Is_Snack
      if Ada.Strings.Unbounded.Index (Msg, "botsnack") > 0 then
         Database_Request.Operation := Database.Snack_Operation;
         Database_Request.Destination := Destination;
         Database.Requests.Enqueue (Database_Request);
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

   procedure Fetch  (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is

      use Ada.Strings.Unbounded;

      Fact    : UString;
      Matches : Match_Array (Match_Range);

   begin  -- Fetch
      Fact := Null_UString;
      if Match (Pat_Fetch2.all, Cmd) = Cmd'First then
         Match (Pat_Fetch2.all, Cmd, Matches);
         if    Matches (3) /= No_Match then
            Fact := US (Cmd (Matches (3).First .. Matches (3).Last));
         elsif Matches (2) /= No_Match then
            Fact := US (Cmd (Matches (2).First .. Matches (2).Last));
         end if;
      else
         Match (Command_Table (Config.Cmd_Fetch).Matcher.all, Cmd, Matches);
         if Matches (1) /= No_Match then
            Fact := US (Cmd (Matches (1).First .. Matches (1).Last));
         end if;
      end if;

      if Fact /= Null_UString then
         while Length (Fact) > 0 and then (Element (Fact, Length (Fact)) = '?' or Element (Fact, Length (Fact)) = ' ') loop
            Fact := Head (Fact, Length (Fact) - 1);
         end loop;
         if Length (Fact) > 0 then
            if Element (Fact, 1) = '~' then
               Database_Request.Operation := Database.RE_Fetch_Operation;
               Database_Request.Data      := US (Slice (Fact, 2, Length (Fact)));
            else
               Database_Request.Operation := Database.Fetch_Operation;
               Database_Request.Data      := Fact;
            end if;
         else
            Database_Request.Operation := Database.Quip_Operation;
         end if;
         Database_Request.Origin      := Sender.Nick;
         Database_Request.Destination := Destination;
         Database.Requests.Enqueue (Database_Request);
      else
         Say ("I can't quite make out your question--try again, maybe?");
      end if;
   end Fetch;

   ---------------------------------------------------------------------------

   procedure Find   (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is

      Matches : Match_Array (Match_Range);

   begin  -- Find
      Match (Command_Table (Config.Cmd_Find).Matcher.all, Cmd, Matches);
      if Matches (1) = No_Match then
         Say ("The RM-find command is ""find regexp"".");
         return;
      end if;
      File_Request.Operation   := File.RM_Operation;
      File_Request.Data        := US (Cmd (Matches (1).First .. Matches (1).Last));
      File_Request.Destination := Sender.Nick;
      File.Requests.Enqueue (File_Request);
   end Find;

   ---------------------------------------------------------------------------

   procedure Forget (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is

      Matches : Match_Array (Match_Range);

   begin  -- Forget
      Match (Command_Table (Config.Cmd_Forget).Matcher.all, Cmd, Matches);
      if Matches (1) = No_Match then
         Say ("The forget-factoid command is ""forget factoid"".");
         return;
      end if;
      Database_Request.Operation   := Database.Forget_Operation;
      Database_Request.Key         := US (Cmd (Matches (1).First .. Matches (1).Last));
      Database_Request.Origin      := Sender.Nick;
      Database_Request.Requestor   := Request.Origin;
      Database_Request.Destination := Destination;
      Database.Requests.Enqueue (Database_Request);
   end Forget;

   ---------------------------------------------------------------------------

   procedure Help   (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is

      Matches : Match_Array (Match_Range);

   begin  -- Help
      Match (Command_Table (Config.Cmd_Help).Matcher.all, Cmd, Matches);
      File_Request.Operation   := File.Help_Operation;
      File_Request.Destination := Sender.Nick;
      if Matches (1) = No_Match then
         File_Request.Data     := Null_UString;
      else
         File_Request.Data     := US (BTrim (Cmd (Matches (1).First .. Matches (1).Last)));
      end if;
      File.Requests.Enqueue (File_Request);
   end Help;

   ---------------------------------------------------------------------------

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
         if S (Unbounded.Head (Line, ActPfx'Length)) = ActPfx then
            Unbounded.Delete (Line, 1, ActPfx'Length);
            Unbounded.Delete (Line, Unbounded.Length (Line), Unbounded.Length (Line));
            Line := " * " & Nick & Line;
         else
            Line := "<" & Nick & "> " & Line;
         end if;
         Line := Times.Time_String (Lines (Index).Stamp, Short_Format => true) & " " & Line;
         Output.Say (Line, Sender.Nick);
      end List_Line;

      ------------------------------------------------------------------------

   begin  -- Last
      Match (Command_Table (Config.Cmd_Last).Matcher.all, Cmd, Matches);
      if Matches (1) = No_Match then
         Count := NLines;
      else
         Count := natural'Min (NLines, positive'Value (Cmd (Matches (1).First .. Matches (1).Last)));
      end if;

      Dbg (Command_Name, "Listing last " & Img (Count) & " lines of " & Img (NLines));
      Show := NextLine - Count;
      if Show < Lines'First then
         Show := Show + NLines;
      end if;

      for L in 1 .. Count loop
         List_Line (Show);
         delay 0.5;
         if Show = Lines'Last then
            Show := Lines'First;
         else
            Show := Show + 1;
         end if;
      end loop;
   end Last;

   ---------------------------------------------------------------------------

   procedure List   (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is

      Matches : Match_Array (Match_Range);
      Pat     : UString;

   begin  -- List
      Match (Command_Table (Config.Cmd_List).Matcher.all, Cmd, Matches);
      if Matches (1) = No_Match then
         Pat := US (".");
      else
         Pat := US (BTrim (Cmd (Matches (1).First .. Matches (1).Last)));
      end if;
      Database_Request.Operation   := Database.List_Operation;
      Database_Request.Data        := Pat;
      Database_Request.Destination := Destination;
      Database.Requests.Enqueue (Database_Request);
   end List;

   ---------------------------------------------------------------------------

   -- Check one's own command access level
   procedure MyAccess (Cmd     : in string;
                       Sender  : in IRC.MsgTo_Rec) is
   begin  -- MyAccess
      Say ("Your current command access level, " & S (Sender.Nick) & ", is " &
           Img (Auth.Level (Request.Origin)) & ".");
   end MyAccess;

   ---------------------------------------------------------------------------

   procedure Quit   (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is
   begin  -- Quit
      Info (Command_Name, "Executing shutdown request from " & S (Request.Origin));
      Shutdown;
      Do_Exit := true;
   end Quit;

   ---------------------------------------------------------------------------

   procedure Quote  (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is
   begin  -- Quote
      Database_Request.Operation   := Database.Quote_Operation;
      Database_Request.Destination := Destination;
      Database.Requests.Enqueue (Database_Request);
   end Quote;

   ---------------------------------------------------------------------------

   procedure Rename (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is

      Matches : Match_Array (Match_Range);

   begin  -- Rename
      Match (Command_Table (Config.Cmd_Rename).Matcher.all, Cmd, Matches);
      if Matches (1) = No_Match or Matches (3) = No_Match then
         Say ("The rename-factoid command is ""rename oldname to newname"".");
         return;
      end if;
      Database_Request.Operation   := Database.Rename_Operation;
      Database_Request.Key         := US (Cmd (Matches (1).First .. Matches (1).Last));
      Database_Request.Data        := US (Cmd (Matches (3).First .. Matches (3).Last));
      Database_Request.Origin      := Sender.Nick;
      Database_Request.Requestor   := Request.Origin;
      Database_Request.Destination := Destination;
      Database.Requests.Enqueue (Database_Request);
   end Rename;

   ---------------------------------------------------------------------------

   procedure Reset    (Cmd     : in string;
                       Sender  : in IRC.MsgTo_Rec) is

      Matches : Match_Array (Match_Range);

   begin  -- Reset
      Match (Command_Table (Config.Cmd_Reset).Matcher.all, Cmd, Matches);
      if Matches (1) = No_Match or Matches (3) = No_Match then
         Say ("Is that supposed to be a reset-factoid command?  If so, it should be ""no, factoid is definition"".");
         return;
      end if;
      declare
         Fact : string := Cmd (Matches (1).First .. Matches (1).Last);
         To   : string := Cmd (Matches (3).First .. Matches (3).Last);
      begin
         Database_Request.Operation   := Database.ResetFactoid_Operation;
         Database_Request.Key         := US (Fact);
         Database_Request.Data        := US (To);
         Database_Request.Origin      := Sender.Nick;
         Database_Request.Requestor   := Request.Origin;
         Database_Request.Destination := Destination;
         Database.Requests.Enqueue (Database_Request);
      end;
   end Reset;

   ---------------------------------------------------------------------------

   procedure Set    (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is

      Matches    : Match_Array (Match_Range);

   begin  -- Set
      Match (Command_Table (Config.Cmd_Set).Matcher.all, Cmd, Matches);
      if Matches (1) = No_Match or Matches (3) = No_Match then
         Say ("Is that supposed to be a set-factoid command?  If so, it should be ""factoid is definition"".");
         return;
      end if;
      declare
         Fact : string := Cmd (Matches (1).First .. Matches (1).Last);
         To   : string := Cmd (Matches (3).First .. Matches (3).Last);
      begin
         if Match (Pat_AlsoSet.all, To) = To'First then
            Match (Pat_AlsoSet.all, To, Matches);
            Database_Request.Operation   := Database.AddFactoid_Operation;
            Database_Request.Data        := US (To (Matches (1).First .. Matches (1).Last));
         elsif Match (Pat_ActionSet.all, To) = To'First then
            Match (Pat_ActionSet.all, To, Matches);
            Database_Request.Operation   := Database.SetAction_Operation;
            Database_Request.Data        := US (To (Matches (1).First .. Matches (1).Last));
         elsif Match (Pat_ReplySet.all, To) = To'First then
            Match (Pat_ReplySet.all, To, Matches);
            Database_Request.Operation   := Database.SetReply_Operation;
            Database_Request.Data        := US (To (Matches (1).First .. Matches (1).Last));
         else
            Database_Request.Operation   := Database.SetFactoid_Operation;
            Database_Request.Data        := US (To);
         end if;
         Database_Request.Key         := US (Fact);
         Database_Request.Origin      := Sender.Nick;
         Database_Request.Destination := Destination;
         Database.Requests.Enqueue (Database_Request);
      end;
   end Set;

   ---------------------------------------------------------------------------

   -- Set a command access level for a usermask
   procedure SetAccess (Cmd     : in string;
                       Sender  : in IRC.MsgTo_Rec) is

      Matches : Match_Array (Match_Range);

   begin  -- SetAccess

      -- Re-match to pick up pattern elements
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
         Database_Request.Operation := Database.Access_Operation;
         Database_Request.Origin    := Sender.Nick;
         Database_Request.Key       := US (Mask);
         Database_Request.Data      := US (Level);
         Database.Requests.Enqueue (Database_Request);
      exception
         -- Mostly this will catch constraint errors from the Value call above
         when others =>
            Level_Error (Level, "didn't sit too well with me");
      end;
   end SetAccess;

   ---------------------------------------------------------------------------

   procedure Stats  (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is

      Matches : Match_Array (Match_Range);
      Msg     : UString;

   begin  -- Stats
      Match (Command_Table (Config.Cmd_Stats).Matcher.all, Cmd, Matches);
      if Matches (1) = No_Match then
         Say ("I am " & Identity.App_ID);
         Say ("I have currently been running for " & Times.Elapsed (Start) & ".");
         delay 1.5;
         Msg := US ("I've been connected for " & Times.Elapsed (Last_Connected));
         if Reconnects = 1 then
            Msg := Msg & "; this is my first connect.";
         elsif Reconnects = 2 then
            Msg := Msg & ", after one reconnect.";
         else
            Msg := Msg & ", after " & Img (Reconnects - 1) & " reconnects.";
         end if;
         Say (S (Msg));
         delay 1.5;
         Msg := US ("I've accepted " & Img (Cmds_Accepted) & " command");
         if Cmds_Accepted > 1 then
            Msg := Msg & "s";
         end if;
         Msg := Msg & ", including this one, and rejected " & Img (Cmds_Rejected) & ".";
         Say (S (Msg));
         delay 1.5;
         Database_Request.Operation   := Database.Stats_Operation;
      else
         declare
            About : string := BTrim (Cmd (Matches (1).First .. Matches (1).Last));
         begin
            if To_Lower (About) = "commands" then
               Output.Say ("Command statistics:", Sender.Nick);
               for VCmd in Config.Valid_Commands loop
                  Output.Say ("   " & Config.Cmd_Names (VCmd) & Img (Config.Command_Usage (VCmd), 4), Sender.Nick);
                  delay 0.5;
               end loop;
               return;  -- did it ourselves
            else
               Database_Request.Key         := US (About);
               Database_Request.Operation   := Database.FactoidStats_Operation;
            end if;
         end;
      end if;
      Database_Request.Destination := Destination;
      Database.Requests.Enqueue (Database_Request);
   end Stats;

   ---------------------------------------------------------------------------

   procedure Tell   (Cmd     : in string;
                     Sender  : in IRC.MsgTo_Rec) is

      Matches : Match_Array (Match_Range);

   begin  -- Tell
      Match (Command_Table (Config.Cmd_Tell).Matcher.all, Cmd, Matches);
      if Matches (1) = No_Match or Matches (3) = No_Match then
         Say ("That doesn't quite make sense to me ... try again, maybe?");
         return;
      end if;
      declare
         To   : string := Cmd (Matches (1).First .. Matches (1).Last);
         Fact : string := Cmd (Matches (3).First .. Matches (3).Last);
      begin
         if To_Lower (To) = To_Lower (S (Current_Nick)) then
            Say ("Hey, I already know that!");
            return;
         elsif To_Lower (To) = "me" then
            Database_Request.Destination := Sender.Nick;
         else
            Database_Request.Destination := US (To);
         end if;
         if Fact (Fact'First) = '~' then
            Database_Request.Operation := Database.RE_Tell_Operation;
            Database_Request.Data      := US (Fact (Fact'First + 1 .. Fact'Last));
         else
            Database_Request.Operation := Database.Tell_Operation;
            Database_Request.Data      := US (Fact);
         end if;
         Database_Request.Origin      := Sender.Nick;

         Database.Requests.Enqueue (Database_Request);
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
      if Cmd (Cmd'First) = '~' then
         Database_Request.Operation := Database.RE_Fetch_Operation;
         Database_Request.Data      := US (Cmd (Cmd'First + 1 .. Cmd'Last));
      else
         Database_Request.Operation := Database.Fetch_Operation;
         Database_Request.Data      := US (Cmd);
      end if;
      Database_Request.Origin      := Sender.Nick;
      Database_Request.Destination := Destination;
      Database.Requests.Enqueue (Database_Request);
   end Fetch_Bare;

   ---------------------------------------------------------------------------

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
      if Keywd ("VERSION") then
         Output.Say (IRC.CTCP_Marker & "VERSION " & Identity.App_ID & IRC.CTCP_Marker, Tgt);
      elsif Keywd ("ACTION") then
         Output.Say (IRC.CTCP_Marker & "ACTION don't play dat!" & IRC.CTCP_Marker, Tgt);
      else
         Output.Say (IRC.CTCP_Marker & "ERRMSG Sorry, I'm not that kind of bot." & IRC.CTCP_Marker, Tgt);
      end if;
   end Process_CTCP_Request;

   ---------------------------------------------------------------------------

   procedure Process_Command (Cmd:  in string;   Sender:  in IRC.MsgTo_Rec) is

      ------------------------------------------------------------------------

      procedure Exec (CmdType : in Config.Command_Type;   Proc : in Command_Processor) is

         use Auth, Config;

         Authorized : Authorization;
         To         : string := S (Destination);

      begin  -- Exec
         Authorized := Permitted (Request.Origin, CmdType);
         if Authorized = Succeeded then
            Cmds_Accepted := Cmds_Accepted + 1;
            Config.Command_Used (CmdType);
            Proc (Cmd, Sender);
         else
            Info (Command_Name, "Rejecting command """ & Cmd & """ from " & S (Request.Origin) & " because " &
                  Authorization'Image (Authorized));
            Cmds_Rejected := Cmds_Rejected + 1;
            case CmdType is
               when Cmd_CkAccess | Cmd_Fetch | Cmd_Find | Cmd_Help | Cmd_List | Cmd_Last =>
                  Output.Say ("You must have pissed somebody off, cuz you're persona non grata.", To);
               when Cmd_MyAccess =>
                  Output.Say ("Why don't you just ask them what their access level is?", To);
               when Cmd_SetAccess =>
                  Output.Say ("Only my operator can do that, sorry.", To);
               when Cmd_Forget | Cmd_Rename | Cmd_Reset | Cmd_Set =>
                  Output.Say ("Only known users can update the factoid database, sorry.", To);
               when Cmd_Quit =>
                  Output.Say ("I'm sorry " & S (Sender.Nick) & ", I don't know you well enough to take orders from you.", To);
               when Cmd_Quote =>
                  Output.Say ("You need a higher access level--ask the bot operator about it.", To);
               when Cmd_Stats =>
                  Output.Say ("You'll be able to access the stats soon enough, if you hang around and contribute.", To);
               when Cmd_Tell =>
                  Output.Say ("That's pretty personal, isn't it?  Let's give you a while, then we'll see.", To);
               when Cmd_None =>
                  Output.Say ("This is another fine mess you've gotten us into, Stanley!", To);
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

   procedure Parser_Init is

      ------------------------------------------------------------------------

      use Config;

      ------------------------------------------------------------------------

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

   procedure Do_Crash is
   begin  -- Do_Crash
      Output.Say ("I'm not feeling well ... think I'll go lie down.", Channel);
      Shutdown;
   end Do_Crash;

------------------------------------------------------------------------------
--
-- Public task
--
------------------------------------------------------------------------------

   task body Command_Task is
   begin  -- Command_Task
      Parser_Init;
      Current_Nick := US (Config.Get_Value (Config.Item_Nick));

      Start          := Times.Current;
      Last_Connected := Times.Current;
      Cmds_Accepted  := 0;
      Cmds_Rejected  := 0;
      Reconnects     := 0;

      NLines    := 0;
      NextLine  := 1;
      Lines     := new Line_Buf (1 .. positive'Value (Config.Get_Value (Config.Item_LastSize)));

      loop
         Requests.Dequeue (Request);
         case Request.Operation is

            when Login_Operation =>

               -- Begin login sequence by sending NICK message
               Info (Command_Name, "Log in as " & S (Current_Nick));
               Output_Request.Operation := Output.Nick_Operation;
               Output_Request.Data      := Current_Nick;
               Output.Requests.Enqueue (Output_Request);
               Output_Request.Operation := Output.User_Operation;

               -- Let the server think about that for a bit; required for pircd, at least
               delay 3.0;

               -- Now send the rest of the login sequence
               Output_Request.Operation := Output.User_Operation;
               Output_Request.Data      := US (Config.Get_Value (Config.Item_UserName) &
                                               " 0 * :" & Config.Get_Value (Config.Item_RealName));
               Output.Requests.Enqueue (Output_Request);

            when Ping_Operation =>
               Dbg (Command_Name, "Ping with " & S (Request.Data));
               Output_Request.Operation := Output.Ping_Operation;
               Output_Request.Data      := Request.Data;
               Output.Requests.Enqueue (Output_Request);

            when Message_Operation | Save_Operation =>
               declare
                  Command    : UString;
                  Has_Prefix : boolean;
                  Is_Command : boolean;
                  Message    : string := S (Request.Data);
                  My_Nick    : string := To_Lower (S (Current_Nick));
                  Shorthand  : string := Config.Get_Value (Config.Item_Shorthand);
               begin
                  IRC.Parse_MsgTo (Request.Origin, Sender);
                  Has_Prefix := false;
                  Command := US (Message);
                  if Shorthand'Length > 0 and then Fixed.Index (Message, Shorthand) = 1 then
                     Has_Prefix := true;
                     Command := US (Message (Shorthand'Length + 1 .. Message'Length));
                  elsif Leading_Nick (Message, My_Nick) then
                     Has_Prefix := true;
                     Command := US (Message (My_Nick'Length + 2 .. Message'Length));
                  end if;
                  Command := Unbounded.Trim (Command, Side => Both);
                  Is_Command := false;
                  if To_Lower (S (Request.Target)) = My_Nick then
                     Is_Command := true;
                     Msg_Type := PrivMsg;
                     Destination := Sender.Nick;
                  else
                     Is_Command := Has_Prefix;
                     Msg_Type := ChanMsg;
                     Destination := Request.Target;
                  end if;
                  if Is_Command and Request.Operation /= Save_Operation then
                     Do_Exit := false;
                     Process_Command (S (Command), Sender);
                     exit when Do_Exit;
                  else
                     Lines (NextLine) := (Times.Current, Sender.Nick, Command);
                     if NLines < Lines'Length then
                        NLines := NLines + 1;
                     end if;
                     if NextLine < Lines'Last then
                        NextLine := NextLine + 1;
                     else
                        NextLine := Lines'First;
                     end if;
                     if Request.Operation /= Save_Operation then
                        if not Is_Snack (Command) then
                           Database_Request.Operation := Database.Quip_Operation;
                           Database_Request.Destination := Destination;
                           Database.Requests.Enqueue (Database_Request);
                        end if;
                     end if;
                  end if;
               end;

            when Notice_Operation =>
               -- Notice from NickServ about identification causes us to try
               -- to identify
               if Fixed.Index (To_Lower (S (Request.Origin)), "nickserv") > 0 and
                  Fixed.Index (To_Lower (S (Request.Data)), "this nickname is owned") > 0 then
                  Output.Say ("identify " & Config.Get_Value (Config.Item_NickPass), "nickserv");
               end if;

            when Crash_Operation =>
               Err (Command_Name, "Crashed:  " & S (Request.Data));
               Do_Crash;
               exit;

            when Reply_Operation =>
               if Request.Reply = IRC.RPL_ENDOFMOTD or Request.Reply = IRC.ERR_NOMOTD then
                  -- End of MOTD, or missing MOTD, is our signal that the
                  -- server has shut up for now, and it's time to try joining
                  -- our channel
                  Dbg (Command_Name, "Done with MOTD, joining " & Channel);
                  Output_Request.Operation := Output.Join_Operation;
                  Output_Request.Data      := US (Channel);
                  Output.Requests.Enqueue (Output_Request);
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
      when E: others =>
         Err (Command_Name, "Exception:  " & Ada.Exceptions.Exception_Information (E));
         Do_Crash;
   end Command_Task;

   ---------------------------------------------------------------------------

end Command;
