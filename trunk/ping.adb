with
  Ada.Strings.Unbounded,
  Ada.Text_IO,
  Config,
  Log,
  Output;

use
  Ada.Strings.Unbounded,
  Log;

package body Ping is

   Ping_Name:  constant string := "Ping";

   task body Ping_Task_Type is
      Max_Missed_Pings : constant := 2;
      Ping_Delay       : constant Duration := 3600.0;  -- try an hour for testing

      Missed         : natural := 0;
      Output_Request : Output.Request_Rec;
      Dont_Care      : boolean;
   begin  -- Ping_Task_Type
      loop
         select
            accept Input_Received;
            Missed := 0;
            while Timeout_Queue.Length > 0 loop
               Timeout_Queue.Dequeue (Dont_Care);
            end loop;
         or
            delay Ping_Delay;
            Missed := Missed + 1;
            if Missed >= Max_Missed_Pings then
               Info (Ping_Name, "Link timed out after " &
                     Duration'Image (Ping_Delay * Duration (Missed)) & " seconds");
               Timeout_Queue.Enqueue (true);
               Missed := 0;
            else
               Output_Request.Operation := Output.Ping_Operation;
               Output_Request.Data := To_Unbounded_String (":" & Config.Get_Value (Config.Item_Host));
               Output.Requests.Enqueue (Output_Request);
            end if;
         end select;
      end loop;
   end Ping_Task_Type;
end Ping;
