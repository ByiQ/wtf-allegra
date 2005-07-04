with
  Ada.Strings.Unbounded;

use
  Ada.Strings.Unbounded;

package body Output is
   task body Output_Task is
      Request:  Request_Rec;
   begin  -- Output_Task
      loop
         Requests.Dequeue (Request);
         begin
            case Request.Operation is
               when Shutdown_Operation =>
                  Dbg (Output_Name, "Writing QUIT """ & To_String (Request.Data) & """");
                  IRC.Write (IRC.Message_Rec'(IRC.Null_Field, To_Unbounded_String ("QUIT"), ":" & Request.Data));
                  exit;

               when Nick_Operation =>
                  Dbg (Output_Name, "Writing NICK " & To_String (Request.Data));
                  IRC.Write (IRC.Message_Rec'(IRC.Null_Field, To_Unbounded_String ("NICK"), Request.Data));

               when User_Operation =>
                  Dbg (Output_Name, "Writing USER " & To_String (Request.Data));
                  IRC.Write (IRC.Message_Rec'(IRC.Null_Field, To_Unbounded_String ("USER"), Request.Data));

               when Join_Operation =>
                  Dbg (Output_Name, "Writing JOIN " & To_String (Request.Data));
                  IRC.Write (IRC.Message_Rec'(IRC.Null_Field, To_Unbounded_String ("JOIN"), Request.Data));

               when Message_Operation =>
                  Dbg (Output_Name, "Writing PRIVMSG to " & To_String (Request.Destination) & ": " &
                       To_String (Request.Data));
                  IRC.Write (IRC.Message_Rec'(IRC.Null_Field, To_Unbounded_String ("PRIVMSG"),
                                              Request.Destination & " :" & Request.Data));

               when Ping_Operation =>
                  Dbg (Output_Name, "Writing PING: " & To_String (Request.Data));
                  IRC.Write (IRC.Message_Rec'(IRC.Null_Field, To_Unbounded_String ("PING"), Request.Data));

               when Pong_Operation =>
                  Dbg (Output_Name, "Writing PONG: " & To_String (Request.Data));
                  IRC.Write (IRC.Message_Rec'(IRC.Null_Field, To_Unbounded_String ("PONG"), Request.Data));
            end case;

         exception
            when IRC.Connect_Error =>
               Err (Output_Name, "Connect error during write in " & Output_Name);

            when E: others =>
               Err (Output_Name, "Other exception " & Ada.Exceptions.Exception_Information (E) &
                    " during write in " & Output_Name);
         end;
      end loop;
   end Output_Task;
end Output;
