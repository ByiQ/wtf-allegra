with
  PQueue;
package Ping is
   task type Ping_Task_Type is
      entry Input_Received;
   end Ping_Task_Type;
   Ping_Task:  Ping_Task_Type;
   package Timeout_Queue_Pkg is new PQueue (boolean);

   Timeout_Queue : Timeout_Queue_Pkg.Protected_Queue_Type;
end Ping;
