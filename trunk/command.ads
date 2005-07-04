with
  Ada.Strings.Unbounded,
  PQueue,
  IRC;

use
  Ada.Strings.Unbounded;

package Command is

   Command_Name:  constant string := "Command";

   type Operation_Type is ( Login_Operation, Finish_Login_Operation, Reply_Operation, Message_Operation,
                            Ping_Operation, Pong_Operation, Crash_Operation );

   subtype Reply_Type is string ( 1 .. 3 );

   type Request_Rec is record
      Operation:  Operation_Type;
      Origin:     Unbounded_String;
      Target:     Unbounded_String;
      Data:       Unbounded_String;
      Reply:      Reply_Type;
   end record;

   task Command_Task;

   package Command_Queue_Pkg is new PQueue (Request_Rec);

   Requests:  Command_Queue_Pkg.Protected_Queue_Type;
end Command;
