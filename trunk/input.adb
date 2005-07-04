with
  Ada.Exceptions,
  Ada.Text_IO,
  Ada.Characters.Handling,
  Ada.Strings.Unbounded,
  Command,
  Config,
  IRC,
  Log,
  Output,
  Ping;

use
  Ada.Text_IO,
  Ada.Characters.Handling,
  Ada.Strings.Unbounded,
  Log;

package body Input is

   Input_Name : constant string := "Input";

   task body Input_Task_Type is

      Input_Name:  constant string := "Input";

      function Numeric_Reply (Field:  string) return boolean is
      begin  -- Numeric_Reply
         return Field'Length = Command.Reply_Type'Length and then
           (Is_Digit (Field (1)) and Is_Digit (Field (2)) and Is_Digit (Field (3)));
      end Numeric_Reply;

      Got_Input       : boolean;
      Input_Message   : IRC.Message_Rec;
      Command_Request : Command.Request_Rec;
      Output_Request  : Output.Request_Rec;
      Dont_Care       : boolean;
   begin  -- Input_Task_Type
      loop
         begin
            IRC.Open_Server (Config.Get_Value (Config.Item_Host), positive'Value (Config.Get_Value (Config.Item_Port)));
            exit;
         exception
            when IRC.Connect_Error =>
               Dbg (Input_Name, "Connect error during initial server connect in " & Input_Name);

            when E: others =>
               Err (Input_Name, "Other exception " & Ada.Exceptions.Exception_Information (E) &
                    " during initial server connect in " & Input_Name);
         end;
         delay 20.0;  -- throttle connect requests
      end loop;
      Command_Request.Operation := Command.Login_Operation;
      Command.Requests.Enqueue (Command_Request);
      loop
         begin
            Got_Input := false;
            select
               Ping.Timeout_Queue.Dequeue (Dont_Care);
               Dbg (Input_Name, "Read aborted by timeout");
            then abort
               IRC.Read (Input_Message);
               Got_Input := true;
            end select;

         exception
            when IRC.Connect_Error =>
               Dbg (Input_Name, "Connect error during read in " & Input_Name);

            when E: others =>
               Err (Input_Name, "Other exception " & Ada.Exceptions.Exception_Information (E) &
                    " during read in " & Input_Name);
         end;

         if Got_Input then
            Ping.Ping_Task.Input_Received;
            Dbg (Input_Name, "Got " & To_String (Input_Message.Prefix) & ", "  &
                 To_String (Input_Message.Command) & ", " &
                 To_String (Input_Message.Params));
            declare
               Cmd       : string := To_Lower (To_String (Input_Message.Command));
               Nick      : string := To_Lower (Config.Get_Value (Config.Item_Nick));
               Message   : Unbounded_String := To_Unbounded_String (To_Lower (To_String (Input_Message.Params)));
               Shorthand : string := Config.Get_Value (Config.Item_Shorthand);
               Params    : IRC.Param_Arr;
               Count     : IRC.Param_Count;
               Has_SHand : boolean;
               Has_Nick  : boolean;
            begin
               if    Cmd = "ping"        then
                  Dbg (Input_Name, "Pong with " & To_String (Input_Message.Params));
                  Output_Request.Operation := Output.Pong_Operation;
                  Output_Request.Data      := Input_Message.Params;
                  Output.Requests.Enqueue (Output_Request);
               elsif Cmd = "privmsg"     then
                  Has_SHand := Shorthand'Length > 0 and then Index (Message, Shorthand) > 0;
                  Has_Nick  := Index (Message, Nick) > 0;
                  if Has_SHand or else Has_Nick then
                     IRC.Parse_Params (Input_Message.Params, Params, Count);
                     Has_SHand := not Has_Nick and then Count >= 2 and then Index (Params (2), Shorthand) = 1;
                  end if;
                  if Has_SHand or else Has_Nick then
                     Dbg (Input_Name, "Recognized message from " & To_String (Input_Message.Prefix) & ": " &
                          To_String (Input_Message.Params));
                     Command_Request.Operation := Command.Message_Operation;
                     Command_Request.Origin    := Input_Message.Prefix;
                     Command_Request.Target    := IRC.Null_Field;
                     Command_Request.Data      := IRC.Null_Field;
                     if Count >= 1 then
                        Command_Request.Target    := Params (1);
                     end if;
                     if Count >= 2 then
                        Command_Request.Data      := Params (2);
                     end if;
                     Command.Requests.Enqueue (Command_Request);
                  end if;
               elsif Numeric_Reply (Cmd) then
                  Dbg (Input_Name, "Reply " & Cmd & ": " & To_String (Input_Message.Params));
                  Command_Request.Operation := Command.Reply_Operation;
                  Command_Request.Reply     := Command.Reply_Type (Cmd);
                  Command_Request.Data      := Input_Message.Params;
                  Command.Requests.Enqueue (Command_Request);
               end if;
            end;
         else
            Info (Input_Name, "Reconnect to " & Config.Get_Value (Config.Item_Host) & ":" & Config.Get_Value (Config.Item_Port));
            begin
               IRC.Close_Server;
               delay 20.0;  -- throttle connect requests
               IRC.Open_Server (Config.Get_Value (Config.Item_Host), positive'Value (Config.Get_Value (Config.Item_Port)));
            exception
               when IRC.Connect_Error =>
                  Dbg (Input_Name, "Connect error during server reconnect in " & Input_Name);

               when E: others =>
                  Err (Input_Name, "Other exception " & Ada.Exceptions.Exception_Information (E) &
                       " during server reconnect in " & Input_Name);
            end;
            Command_Request.Operation := Command.Login_Operation;
            Command.Requests.Enqueue (Command_Request);
         end if;
      end loop;
   end Input_Task_Type;

end Input;
