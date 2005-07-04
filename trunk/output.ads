with
  Ada.Exceptions,
  Ada.Strings.Unbounded,
  IRC,
  Log,
  PQueue;

use
  Ada.Strings.Unbounded,
  Log;

package Output is

   Output_Name:  constant string := "Output";

   type Operation_Type is ( Shutdown_Operation, Nick_Operation, User_Operation, Join_Operation,
                            Message_Operation, Ping_Operation, Pong_Operation );

   type Request_Rec is record
      Operation:    Operation_Type;
      Destination:  Unbounded_String;
      Data:         Unbounded_String;
   end record;

   task Output_Task;

   package Output_Queue_Pkg is new PQueue (Request_Rec);

   Requests:  Output_Queue_Pkg.Protected_Queue_Type;

end Output;
