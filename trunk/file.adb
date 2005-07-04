with
  Ada.Strings.Unbounded,
  Output;

use
  Ada.Strings.Unbounded;

package body File is

   task body File_Task is
      Request:           Request_Rec;
      Output_Request:    Output.Request_Rec;

   begin  -- File_Task
      loop
         Requests.Dequeue (Request);
         exit when Request.Operation = Shutdown_Operation;
         Output_Request.Operation := Output.Message_Operation;
         Output_Request.Destination := Request.Destination;
         Output_Request.Data := "File request: " & Request.Data;
         Output.Requests.Enqueue (Output_Request);
      end loop;
   end File_Task;

end File;
