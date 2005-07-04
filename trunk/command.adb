with
  Ada.Calendar,
  Ada.Characters.Handling,
  Ada.Exceptions,
  Ada.Strings.Fixed,
  Ada.Strings.Maps,
  Ada.Strings.Unbounded,
  GNAT.Regpat,
  Auth,
  Config,
  Database,
  File,
  Input,
  Log,
  Output,
  Ping;

use
  Ada.Calendar,
  Ada.Characters.Handling,
  Ada.Strings.Fixed,
  Ada.Strings.Maps,
  Ada.Strings.Unbounded,
  GNAT.Regpat,
  Log;

package body Command is

   task body Command_Task is
      function S (Source : in Ada.Strings.Unbounded.Unbounded_String) return string
        renames Ada.Strings.Unbounded.To_String;

      function US (Source : in string) return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      Separators  : Character_Set := To_Set (",:~");
      Max_Matches : constant := 4;

      subtype Match_Range is Match_Count range 1 .. Max_Matches;

      type Message_Type_Enm is ( PrivMsg, ChanMsg );

      type Matcher_Ptr is access Pattern_Matcher;
      type Command_Processor is access procedure (Cmd     : in string;
                                                  Sender  : in IRC.MsgTo_Rec);
      type Command_Descriptor is record
         Matcher  : Matcher_Ptr;
         Process  : Command_Processor;
      end record;

      subtype Valid_Commands is Config.Command_Type range Config.Cmd_Access .. Config.Command_Type'Last;

      Command_Table    : array (Valid_Commands) of Command_Descriptor;
      Database_Request : Database.Request_Rec;
      Destination      : Unbounded_String;
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

      Start            : Time;
      Last_Connected   : Time;
      Cmds_Accepted    : natural;
      Cmds_Rejected    : natural;
      Reconnects       : natural;

      Help_Table : array (positive range <>) of Unbounded_String :=
        (
         --      US ("<factoid-name> (is|are) [also|action|reply] <factoid-def>"),
         US ("<factoid-name> (is|are) [also] <factoid-def>"),
         --      US ("no, <factoid-name> (is|are) [action|reply] <factoid-def>"),
         US ("no, <factoid-name> (is|are) <factoid-def>"),
         US ("[~]<factoid-name>[?]"),
         US ("tell (<nick>|me) [about] <factoid>"),
         US ("what [is|are] [~]<factoid> [?]"),
         US ("forget <factoid>"),
         US ("rename <from-name> (as|to) <to-name>"),
         --      US ("find <string>"),
         US ("help"),
         US ("list [regexp]"),
         US ("quote"),
         US ("stats [<factoid>]"),
         US ("access [<mask> <level>]"),
         US ("quit")
        );

      function Channel return string is
      begin  -- Channel
         return "#" & Config.Get_Value (Config.Item_Channel);
      end Channel;

      function Elapsed (From : in Time) return string is

         type MilliMin is delta 0.001 range Duration'First .. Duration'Last;

         OneDay  : natural := natural (Day_Duration'Last);
         OneHour : natural := 60 * 60;

         Diff    : natural := natural (Clock - From);
         Days    : natural;
         Hours   : natural;
         Minutes : MilliMin;
         Answer  : Unbounded_String;

      begin  -- Elapsed
         Days    := Diff / OneDay;
         Hours   := (Diff mod OneDay) / OneHour;
         Minutes := Millimin (Diff mod OneHour) / 60.0;

         if Days > 0 then
            Answer := US (natural'Image (Days));
            if Days > 1 then
               Answer := Answer & " days";
            else
               Answer := Answer & " day";
            end if;
            if Hours > 0 then
               Answer := Answer & ",";
            else
               Answer := Answer & " and";
            end if;
         else
            Answer := Null_Unbounded_String;
         end if;
         if Hours > 0 then
            Answer := Answer & natural'Image (Hours);
            if Hours > 1 then
               Answer := Answer & " hours and";
            else
               Answer := Answer & " hour and";
            end if;
         end if;
         Answer := Answer & MilliMin'Image (Minutes) & " minutes";
         return Trim (S (Answer), Side => Ada.Strings.Left);
      end Elapsed;

      procedure Say (Msg : in string;  To : in string) is
      begin  -- Say
         Output_Request.Operation := Output.Message_Operation;
         Output_Request.Destination := US (To);
         Output_Request.Data := US (Msg);
         Output.Requests.Enqueue (Output_Request);
      end Say;

      function Leading_Nick (Msg : in string;   Nick:  in string) return boolean is
      begin  -- Leading_Nick
         if Msg'Length > Nick'Length + 2 then
            if Index (To_Lower (Msg), Nick) = 1 and Is_In (Msg (Nick'Length + 1), Separators) then
               return true;
            end if;
         end if;
         return false;
      end Leading_Nick;

      procedure Shutdown is
      begin  -- Shutdown
         Database_Request.Operation := Database.Shutdown_Operation;
         File_Request.Operation     := File.Shutdown_Operation;
         Database.Requests.Enqueue (Database_Request);
         File.Requests.Enqueue (File_Request);
         abort Input.Input_Task;
         abort Ping.Ping_Task;
         delay 3.0;
         Log.WrapUp;
      end Shutdown;

      procedure SetAuth (Cmd     : in string;
                         Sender  : in IRC.MsgTo_Rec) is

         Matches : Match_Array (Match_Range);

      begin  -- SetAuth
         Match (Command_Table (Config.Cmd_Access).Matcher.all, Cmd, Matches);
         if Matches (1) = No_Match or Matches (2) = No_Match then
            Say ("That may be your idea of an access command, but it isn't mine ... try again, maybe?", S (Destination));
            return;
         end if;
         declare
            Nick  : string := Cmd (Matches (1).First .. Matches (1).Last);
            Level : string := Cmd (Matches (2).First .. Matches (2).Last);
         begin
            if To_Lower (Nick) = To_Lower (Config.Get_Value (Config.Item_Nick)) then
               Say ("I am already the Lord of Access, O mortal!", S (Destination));
               return;
            end if;
            Database_Request.Operation := Database.Access_Operation;
            Database_Request.Origin    := Sender.Nick;
            Database_Request.Key       := US (Nick);
            Database_Request.Data      := US (Level);
            Database.Requests.Enqueue (Database_Request);
         end;
      end SetAuth;
      procedure GetAuth (Cmd     : in string;
                         Sender  : in IRC.MsgTo_Rec) is
      begin  -- GetAuth
         Say ("Your access level, " & S (Sender.Nick) & ", is" &
              natural'Image (Auth.Level (Request.Origin)) & ".",
              S (Destination));
      end GetAuth;
      procedure Fetch  (Cmd     : in string;
                        Sender  : in IRC.MsgTo_Rec) is

         Fact    : Unbounded_String;
         Matches : Match_Array (Match_Range);

      begin  -- Fetch
         Fact := Null_Unbounded_String;
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

         if Fact /= Null_Unbounded_String then
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
            Say ("I can't quite make out your question--try again, maybe?", S (Destination));
         end if;
      end Fetch;
      procedure Find   (Cmd     : in string;
                        Sender  : in IRC.MsgTo_Rec) is
      begin  -- Find
         Say ("The find command is not yet implemented.", S (Destination));
      end Find;
      procedure Forget (Cmd     : in string;
                        Sender  : in IRC.MsgTo_Rec) is

         Matches : Match_Array (Match_Range);

      begin  -- Forget
         Match (Command_Table (Config.Cmd_Forget).Matcher.all, Cmd, Matches);
         if Matches (1) = No_Match then
            Say ("The forget-factoid command is ""forget factoid"".", S (Destination));
            return;
         end if;
         Database_Request.Operation   := Database.Forget_Operation;
         Database_Request.Key         := US (Cmd (Matches (1).First .. Matches (1).Last));
         Database_Request.Origin      := Sender.Nick;
         Database_Request.Requestor   := Request.Origin;
         Database_Request.Destination := Destination;
         Database.Requests.Enqueue (Database_Request);
      end Forget;
      procedure Help   (Cmd     : in string;
                        Sender  : in IRC.MsgTo_Rec) is
      begin  -- Help
         Say ("I currently know the following commands:", S (Sender.Nick));
         for Msg in Help_Table'Range loop
            Say ("   " & S (Help_Table (Msg)), S (Sender.Nick));
            if Msg /= Help_Table'Last then
               delay 0.5;
            end if;
         end loop;
         Say ("The shortcut string is """ & Config.Get_Value (Config.Item_Shorthand) &
              """; a leading ""~"" in a factoid name treats it as a regexp.", S (Sender.Nick));
      end Help;
      procedure List   (Cmd     : in string;
                        Sender  : in IRC.MsgTo_Rec) is

         Matches : Match_Array (Match_Range);
         Pat     : Unbounded_String;

      begin  -- List
         Match (Command_Table (Config.Cmd_List).Matcher.all, Cmd, Matches);
         if Matches (1) = No_Match then
            Pat := US (".");
         else
            Pat := US (Trim (Cmd (Matches (1).First .. Matches (1).Last), Side => Ada.Strings.Both));
         end if;
         Database_Request.Operation   := Database.List_Operation;
         Database_Request.Data        := Pat;
         Database_Request.Destination := Destination;
         Database.Requests.Enqueue (Database_Request);
      end List;
      procedure Quit   (Cmd     : in string;
                        Sender  : in IRC.MsgTo_Rec) is
      begin  -- Quit
         Info (Command_Name, "Executing shutdown request from " & S (Request.Origin));
         Shutdown;
         Do_Exit := true;
      end Quit;
      procedure Quote  (Cmd     : in string;
                        Sender  : in IRC.MsgTo_Rec) is
      begin  -- Quote
         Database_Request.Operation   := Database.Quote_Operation;
         Database_Request.Destination := Destination;
         Database.Requests.Enqueue (Database_Request);
      end Quote;
      procedure Rename (Cmd     : in string;
                        Sender  : in IRC.MsgTo_Rec) is

         Matches : Match_Array (Match_Range);

      begin  -- Rename
         Match (Command_Table (Config.Cmd_Rename).Matcher.all, Cmd, Matches);
         if Matches (1) = No_Match or Matches (3) = No_Match then
            Say ("The rename-factoid command is ""rename oldname to newname"".", S (Destination));
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
      procedure Reset    (Cmd     : in string;
                          Sender  : in IRC.MsgTo_Rec) is

         Matches : Match_Array (Match_Range);

      begin  -- Reset
         Match (Command_Table (Config.Cmd_Reset).Matcher.all, Cmd, Matches);
         if Matches (1) = No_Match or Matches (3) = No_Match then
            Say ("Is that supposed to be a reset-factoid command?  If so, it should be ""no, factoid is definition"".", S (Destination));
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
      procedure Set    (Cmd     : in string;
                        Sender  : in IRC.MsgTo_Rec) is

         Matches    : Match_Array (Match_Range);

      begin  -- Set
         Match (Command_Table (Config.Cmd_Set).Matcher.all, Cmd, Matches);
         if Matches (1) = No_Match or Matches (3) = No_Match then
            Say ("Is that supposed to be a set-factoid command?  If so, it should be ""factoid is definition"".", S (Destination));
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
      procedure Stats  (Cmd     : in string;
                        Sender  : in IRC.MsgTo_Rec) is


         Matches : Match_Array (Match_Range);
         Msg     : Unbounded_String;

      begin  -- Stats
         Match (Command_Table (Config.Cmd_Stats).Matcher.all, Cmd, Matches);
         if Matches (1) = No_Match then
            Say ("I have currently been running for " & Elapsed (Start) & ".", S (Destination));
            delay 1.5;
            Msg := US ("I've been connected for " & Elapsed (Last_Connected));
            if Reconnects = 1 then
               Msg := Msg & "; this is my first connect.";
            elsif Reconnects = 2 then
               Msg := Msg & ", after one reconnect.";
            else
               Msg := Msg & ", after" & natural'Image (Reconnects - 1) & " reconnects.";
            end if;
            Say (S (Msg), S (Destination));
            delay 1.5;
            Msg := US ("I've accepted" & natural'Image (Cmds_Accepted) & " command");
            if Cmds_Accepted > 1 then
               Msg := Msg & "s";
            end if;
            Msg := Msg & ", including this one, and rejected" & natural'Image (Cmds_Rejected) & ".";
            Say (S (Msg), S (Destination));
            delay 1.5;
            Database_Request.Operation   := Database.Stats_Operation;
         else
            Database_Request.Key         := US (Trim (Cmd (Matches (1).First .. Matches (1).Last), Side => Ada.Strings.Both));
            Database_Request.Operation   := Database.FactoidStats_Operation;
         end if;
         Database_Request.Destination := Destination;
         Database.Requests.Enqueue (Database_Request);
      end Stats;
      procedure Tell   (Cmd     : in string;
                        Sender  : in IRC.MsgTo_Rec) is

         Matches : Match_Array (Match_Range);

      begin  -- Tell
         Match (Command_Table (Config.Cmd_Tell).Matcher.all, Cmd, Matches);
         if Matches (1) = No_Match or Matches (3) = No_Match then
            Say ("That doesn't quite make sense to me ... try again, maybe?", S (Destination));
            return;
         end if;
         declare
            To   : string := Cmd (Matches (1).First .. Matches (1).Last);
            Fact : string := Cmd (Matches (3).First .. Matches (3).Last);
         begin
            if To_Lower (To) = To_Lower (Config.Get_Value (Config.Item_Nick)) then
               Say ("Hey, I already know that!", S (Destination));
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
      procedure Unrecognized (Cmd     : in string;
                              Sender  : in IRC.MsgTo_Rec) is

         Msg : string := "I don't recognize that command.";

      begin  -- Unrecognized
         if Cmd (1) = '~' then
            Database_Request.Operation := Database.RE_Fetch_Operation;
            Database_Request.Data      := US (Cmd (2 .. Cmd'Last));
         else
            Database_Request.Operation := Database.Fetch_Operation;
            Database_Request.Data      := US (Cmd);
         end if;
         Database_Request.Origin      := Sender.Nick;
         Database_Request.Destination := Destination;
         Database.Requests.Enqueue (Database_Request);
         ---      if Msg_Type = PrivMsg then
         ---         Say (Msg, S (Destination));
         ---      else
         ---         Say ("Sorry " & S (Sender.Nick) & ", " & Msg, S (Destination));
         ---      end if;
      end Unrecognized;

      procedure Process_Command (Cmd:  in string;   Sender:  in IRC.MsgTo_Rec) is

         procedure Exec (CmdType : in Config.Command_Type;   Proc : in Command_Processor) is

            use Auth, Config;

            Authorized : Authorization;
            To         : string := S (Destination);

         begin  -- Exec
            Authorized := Permitted (Request.Origin, CmdType);
            if Authorized = Succeeded then
               Cmds_Accepted := Cmds_Accepted + 1;
               Proc (Cmd, Sender);
            else
               Info (Command_Name, "Rejecting command """ & Cmd & """ from " & S (Request.Origin) & " because " &
                     Authorization'Image (Authorized));
               Cmds_Rejected := Cmds_Rejected + 1;
               case CmdType is
                  when Cmd_Access =>
                     Say ("Only my operator can do that, sorry.", To);
                  when Cmd_CkAccess | Cmd_Fetch | Cmd_Find | Cmd_Help | Cmd_List =>
                     Say ("You must have pissed somebody off, cuz you're persona non grata.", To);
                  when Cmd_Forget | Cmd_Rename | Cmd_Reset | Cmd_Set =>
                     Say ("Only known users can update the factoid database, sorry.", To);
                  when Cmd_Quit =>
                     Say ("I'm sorry " & S (Sender.Nick) & ", I don't know you well enough to take orders from you.", To);
                  when Cmd_Quote =>
                     Say ("Here's a quote for you:  get an access level!", To);
                  when Cmd_Stats =>
                     Say ("You'll be able to access the stats soon enough, if you hang around and contribute.", To);
                  when Cmd_Tell =>
                     Say ("That's pretty personal, isn't it?  Let's give you a while, then we'll see.", To);
                  when Cmd_None =>
                     Say ("This is another fine mess you've gotten us into, Stanley!", To);
               end case;
            end if;
         end Exec;

      begin  -- Process_Command
         Dbg (Command_Name, "Processing command """ & Cmd & """ from " & S (Sender.Nick));
         if Cmd'Length < 1 then
            return;
         end if;
         if Match (Pat_Fetch2.all, Cmd) = Cmd'First then
            Exec (Config.Cmd_Fetch, Fetch'Access);
            return;
         end if;
         for CmdType in Command_Table'Range loop
            if Match (Command_Table (CmdType).Matcher.all, Cmd) = Cmd'First then
               Exec (CmdType, Command_Table (CmdType).Process);
               return;
            end if;
         end loop;
         Unrecognized (Cmd, Sender);
      end Process_Command;

      procedure Parser_Init is

         use Config;

         procedure Enter (Cmd      : in Command_Type;
                          Pat      : in string;
                          Proc     : in Command_Processor) is
         begin  -- Enter
            Command_Table (Cmd) := (new Pattern_Matcher'(Compile (Pat,  Case_Insensitive)), Proc);
         end Enter;

      begin  -- Parser_Init
         Enter (Cmd_Access,   "^access\s+(\S+)\s+(\d+)$",                            SetAuth'Access);
         Enter (Cmd_CkAccess, "^access$",                                            GetAuth'Access);
         Enter (Cmd_Fetch,    "^(\S.*)\s*(\?)$",                                     Fetch'Access);
         Enter (Cmd_Find,     "^find\s+(\S.*)$",                                     Find'Access);
         Enter (Cmd_Forget,   "^forget\s+(\S.*)$",                                   Forget'Access);
         Enter (Cmd_Help,     "^help$",                                              Help'Access);
         Enter (Cmd_List,     "^list(\s+\S.*)?$",                                    List'Access);
         Enter (Cmd_Quit,     "^quit$",                                              Quit'Access);
         Enter (Cmd_Quote,    "^quote$",                                             Quote'Access);
         Enter (Cmd_Rename,   "^rename\s+(\S.*)\s+(as|to)\s+(\S.*)$",                Rename'Access);
         Enter (Cmd_Reset,    "^no,\s*(\S.*?)\s+(is|are)\s+(\S.*)$",                 Reset'access);
         Enter (Cmd_Set,      "^(\S.*?)\s+(is|are)\s+(\S.*)$",                       Set'Access);
         Enter (Cmd_Stats,    "^stats(\s+\S.*)?$",                                   Stats'Access);
         Enter (Cmd_Tell,     "^tell\s+(\S+)\s+(about\s+)?(\S.*)$",                  Tell'Access);
         Pat_ActionSet := new Pattern_Matcher'(Compile ("^action\s+(\S.*)$",                 Case_Insensitive));
         Pat_AlsoSet   := new Pattern_Matcher'(Compile ("^also\s+(\S.*)$",                   Case_Insensitive));
         Pat_Fetch2    := new Pattern_Matcher'(Compile ("^(what)\s+(is\s+|are\s+)?(\S.*)$",  Case_Insensitive));
         Pat_ReplySet  := new Pattern_Matcher'(Compile ("^reply\s+(\S.*)$",                  Case_Insensitive));
      end Parser_Init;

      procedure Do_Crash is
      begin  -- Do_Crash
         Say ("I'm not feeling well ... think I'll go lie down.", Channel);
         Shutdown;
      end Do_Crash;


   begin  -- Command_Task
      Parser_Init;

      Start          := Clock;
      Last_Connected := Clock;
      Cmds_Accepted  := 0;
      Cmds_Rejected  := 0;
      Reconnects     := 0;

      loop
         Requests.Dequeue (Request);
         case Request.Operation is

            when Login_Operation =>
               Info (Command_Name, "Begin login as " & Config.Get_Value (Config.Item_Nick));
               Output_Request.Operation := Output.Nick_Operation;
               Output_Request.Data      := US (Config.Get_Value (Config.Item_Nick));
               Output.Requests.Enqueue (Output_Request);
               Output_Request.Operation := Output.User_Operation;

               delay 3.0;  -- let the server respond a bit; required for pircd, at least
               Request.Operation := Finish_Login_Operation;
               Requests.Enqueue (Request);

            when Finish_Login_Operation =>
               Info (Command_Name, "Finish login to " & "#" & Config.Get_Value (Config.Item_Channel));
               Output_Request.Operation := Output.User_Operation;
               Output_Request.Data      := US (Config.Get_Value (Config.Item_UserName) &
                                               " 0 * :" & Config.Get_Value (Config.Item_RealName));
               Output.Requests.Enqueue (Output_Request);
               delay 3.0;  -- doesn't delay the write, but ...
               Output_Request.Operation := Output.Join_Operation;
               Output_Request.Data      := US ("#" & Config.Get_Value (Config.Item_Channel));
               Output.Requests.Enqueue (Output_Request);
               Say ("identify nulet0", "nickserv");  -- quick hack for development
               Reconnects := Reconnects + 1;
               Last_Connected := Clock;

            when Ping_Operation =>
               Dbg (Command_Name, "Ping with " & S (Request.Data));
               Output_Request.Operation := Output.Ping_Operation;
               Output_Request.Data      := Request.Data;
               Output.Requests.Enqueue (Output_Request);

            when Pong_Operation =>
               Dbg (Command_Name, "Pong with " & S (Request.Data));
               Output_Request.Operation := Output.Pong_Operation;
               Output_Request.Data      := Request.Data;
               Output.Requests.Enqueue (Output_Request);

            when Message_Operation =>
               declare
                  Command    : Unbounded_String;
                  Has_Prefix : boolean;
                  Is_Command : boolean;
                  Message    : string := S (Request.Data);
                  My_Nick    : string := To_Lower (Config.Get_Value (Config.Item_Nick));
                  Shorthand  : string := Config.Get_Value (Config.Item_Shorthand);
               begin
                  IRC.Parse_MsgTo (Request.Origin, Sender);
                  Has_Prefix := false;
                  Command := US (Message);
                  if Shorthand'Length > 0 and then Index (Message, Shorthand) = 1 then
                     Has_Prefix := true;
                     Command := US (Message (Shorthand'Length + 1 .. Message'Length));
                  elsif Leading_Nick (Message, My_Nick) then
                     Has_Prefix := true;
                     Command := US (Message (My_Nick'Length + 2 .. Message'Length));
                  end if;
                  Command := Trim (Command, Side => Ada.Strings.Both);
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
                  if Is_Command then
                     Do_Exit := false;
                     Process_Command (S (Command), Sender);
                     exit when Do_Exit;
                  elsif Config.Get_Value (Config.Item_Quips) = "on" then
                     Database_Request.Operation := Database.Quip_Operation;
                     Database_Request.Destination := Destination;
                     Database.Requests.Enqueue (Database_Request);
                  end if;
               end;

            when Crash_Operation =>
               Err (Command_Name, "Crashed:  " & S (Request.Data));
               Do_Crash;
               exit;

            when Reply_Operation =>
               null;  -- for now
         end case;
      end loop;

   exception
      when E: others =>
         Err (Command_Name, "Exception:  " & Ada.Exceptions.Exception_Information (E));
         Do_Crash;
   end Command_Task;

end Command;
