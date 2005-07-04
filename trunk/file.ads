with
  Ada.Strings.Unbounded,
  PQueue;

use
  Ada.Strings.Unbounded;

package File is

   type Operation_Type is ( Shutdown_Operation, File_Operation );

   type Request_Rec is record
      Operation:    Operation_Type;
      Destination:  Unbounded_String;
      Data:         Unbounded_String;
   end record;

   task File_Task;

   package File_Queue_Pkg is new PQueue (Request_Rec);

   Requests:  File_Queue_Pkg.Protected_Queue_Type;

end File;
