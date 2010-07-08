
--
-- Input -- Server input task package for Allegra info-bot
--


--
-- Standard packages
with Ada.Exceptions;
with Ada.Characters.Handling;
use  Ada.Characters.Handling;
with Ada.Strings.Unbounded;


--
-- Local library packages
with Strings;
use  Strings;


--
-- Application packages
with CommandQ;
with Config;
with IRC;
with Log;
use  Log;
with OutputQ;
with Ping;


package body Input is

------------------------------------------------------------------------------
--
-- Package constants
--
------------------------------------------------------------------------------

   -- This task's name, for logging purposes
   Input_Name      : constant string := "Input";

   -- Delay this long between failed attempts to connect to the server
   Reconnect_Delay : constant Duration := 20.0;

------------------------------------------------------------------------------
--
-- Package subroutines
--
------------------------------------------------------------------------------

   -- Return true if given string is in the form of an IRC server numeric
   -- reply (three decimal digits)
   function Numeric_Reply (Field : in string) return boolean is
   begin  -- Numeric_Reply
      return Field'Length = IRC.Server_Reply_Length and then
        (Is_Digit (Field (Field'First)) and Is_Digit (Field (Field'First + 1)) and Is_Digit (Field (Field'First + 2)));
   end Numeric_Reply;

   ---------------------------------------------------------------------------

   -- Keep retrying until we connect to the server
   procedure Connect_To_Server (Msg : in string) is
   begin  -- Connect_To_Server
      loop
         begin
            IRC.Open_Server (Config.Get_Value (Config.Item_Host), positive'Value (Config.Get_Value (Config.Item_Port)));
            exit;
         exception
            when IRC.Connect_Error =>
               Dbg (Input_Name, "Connect error during " & Msg);

            when E : others =>
               Err (Input_Name, "Other exception " & Ada.Exceptions.Exception_Information (E) & " during " & Msg);
               CommandQ.Crash (Input_Name);
         end;
         delay Reconnect_Delay;  -- throttle connect requests
      end loop;
   end Connect_To_Server;

------------------------------------------------------------------------------
--
-- Public task
--
------------------------------------------------------------------------------

   task body Input_Task_Type is

      Got_Input       : Boolean;
      Input_Message   : IRC.Message_Rec;
      Command_Request : CommandQ.Request_Rec;
      Output_Request  : OutputQ.Request_Rec;

   begin  -- Input_Task_Type

      -- Connect and queue up a login sequence to begin with
      Connect_To_Server ("initial server connect");
      Command_Request.Operation := CommandQ.Login_Operation;
      CommandQ.Requests.Enqueue (Command_Request);

      -- Main input task loop
      loop

         -- Try to read some input from server; can be aborted by timeout from
         -- ping task, or by I/O exception, leaving Got_Input false
         begin
            Got_Input := false;
            select
               Ping.Timeout_Signal.Wait;
               Dbg (Input_Name, "Read aborted by timeout");
            then abort
               IRC.Read (Input_Message);
               Got_Input := true;
            end select;

         exception
            when IRC.Connect_Error =>
               Dbg (Input_Name, "Connect error during read");

            when E : others =>
               Err (Input_Name, "Other exception " & Ada.Exceptions.Exception_Information (E) & " during read");
               CommandQ.Crash (Input_Name);
         end;

         -- If we read a line from the server, do some initial processing and
         -- inspection of it
         if Got_Input then

            -- Let the ping task know that we got something, and log it
            Ping.Ping_Task.Input_Received;
            Dbg (Input_Name, "Got " & S (Input_Message.Prefix) & ", "  &
                 S (Input_Message.Command) & ", " &
                 S (Input_Message.Params));

            -- Now categorize the input into a few broad classes
            declare
               use Ada.Strings;  -- just so we can say "Unbounded.*" in a few places
               Cmd       : string := To_Lower (S (Input_Message.Command));
               Nick      : string := To_Lower (Config.Get_Value (Config.Item_Nick));
               Message   : UString := US (To_Lower (S (Input_Message.Params)));
               Shorthand : string := Config.Get_Value (Config.Item_Shorthand);
               Params    : IRC.Param_Arr;
               Count     : IRC.Param_Count;
               Has_SHand : boolean;
               Has_Nick  : boolean;
            begin

               -- If it's a PING, just respond immediately with a PONG
               if    Cmd = "ping"        then
                  Dbg (Input_Name, "Pong with " & S (Input_Message.Params));
                  Output_Request.Operation := OutputQ.Pong_Operation;
                  Output_Request.Data      := Input_Message.Params;
                  OutputQ.Requests.Enqueue (Output_Request);

               -- If it's a PRIVMSG, see if it's addressed to us in some way,
               -- either by starting with our shorthand string, or by
               -- containing our nick.
               elsif Cmd = "privmsg"     then

                  -- Start by looking for nick or shorthand string anywhere
                  Has_SHand := Shorthand'Length > 0 and then Unbounded.Index (Message, Shorthand) > 0;
                  Has_Nick  := Unbounded.Index (Message, Nick) > 0;

                  -- Split up the params, and make a closer examination of the
                  -- message to see if the shorthand string is at the start of
                  -- the message portion.
                  IRC.Parse_Params (Input_Message.Params, Params, Count);
                  if Has_SHand or else Has_Nick then
                     Has_SHand := not Has_Nick and then Count >= 2 and then Unbounded.Index (Params (2), Shorthand) = 1;
                  end if;

                  -- If it's addressed to us somehow, send it to the command
                  -- task as a "message"
                  if Has_SHand or else Has_Nick then
                     Dbg (Input_Name, "Recognized message from " & S (Input_Message.Prefix) & ": " &
                                      S (Input_Message.Params));
                     Command_Request.Operation := CommandQ.Message_Operation;
                     Command_Request.Origin    := Input_Message.Prefix;
                     Command_Request.Target    := IRC.Null_Field;
                     Command_Request.Data      := IRC.Null_Field;
                     if Count >= 1 then
                        Command_Request.Target := Params (1);
                     end if;
                     if Count >= 2 then
                        Command_Request.Data   := Params (2);
                     end if;
                     CommandQ.Requests.Enqueue (Command_Request);

                  -- If it's not addressed to us, still send it to the command
                  -- task, but as a "save" operation, so it can be retrieved
                  -- later by a "last" command
                  else
                     Command_Request.Operation := CommandQ.Save_Operation;
                     Command_Request.Origin    := Input_Message.Prefix;
                     Command_Request.Target    := IRC.Null_Field;
                     if Count >= 2 then
                        Command_Request.Data   := Params (2);
                     else
                        Command_Request.Data   := IRC.Null_Field;
                     end if;
                     CommandQ.Requests.Enqueue (Command_Request);
                  end if;

               -- Assume that all NOTICE messages that come to us need to be
               -- inspected by the command task
               elsif Cmd = "notice"      then
                  Dbg (Input_Name, "Notice from " & S (Input_Message.Prefix) & ": " &
                                    S (Input_Message.Params));
                  IRC.Parse_Params (Input_Message.Params, Params, Count);
                  Command_Request.Operation := CommandQ.Notice_Operation;
                  Command_Request.Origin    := Input_Message.Prefix;
                  Command_Request.Target    := IRC.Null_Field;
                  Command_Request.Data      := IRC.Null_Field;
                  if Count >= 1 then
                     Command_Request.Target := Params (1);
                  end if;
                  if Count >= 2 then
                     Command_Request.Data   := Params (2);
                  end if;
                  CommandQ.Requests.Enqueue (Command_Request);

               -- Send all numeric server replies to the command task
               elsif Numeric_Reply (Cmd) then
                  Dbg (Input_Name, "Reply " & Cmd & ": " & S (Input_Message.Params));
                  Command_Request.Operation := CommandQ.Reply_Operation;
                  Command_Request.Reply     := positive'Value (Cmd);
                  Command_Request.Data      := Input_Message.Params;
                  CommandQ.Requests.Enqueue (Command_Request);

               -- Not something we recognize, so just ignore it (though we log
               -- that fact in debug mode)
               else
                  Dbg (Input_Name, "Ignoring " & Cmd & ": " & S (Input_Message.Params));
               end if;
            end;
         else
            -- If we didn't get input, we should reconnect and re-login
            Info (Input_Name, "Reconnect to " & Config.Get_Value (Config.Item_Host) & ":" & Config.Get_Value (Config.Item_Port));
            IRC.Close_Server;
            Connect_To_Server ("server reconnect");
            Command_Request.Operation := CommandQ.Login_Operation;
            CommandQ.Requests.Enqueue (Command_Request);
         end if;
      end loop;

   exception
      when E : others =>
         Err (Input_Name, "Exception " & Ada.Exceptions.Exception_Information (E));
         CommandQ.Crash (Input_Name);
   end Input_Task_Type;

   ---------------------------------------------------------------------------

end Input;
