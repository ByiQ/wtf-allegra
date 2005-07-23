
--
-- Ping -- Periodic server connection task package for Allegra info-bot
--


--
-- Local library packages
with PQueue;


package Ping is

------------------------------------------------------------------------------
--
-- Timeout queue
--
------------------------------------------------------------------------------

   -- Instantiate the protected-queue package with boolean (because we don't
   -- care about the value, we're using the queue as a semaphore), and create
   -- an instance of it.  Unlike the other task's request queues, this is an
   -- "output" queue, in that its main purpose is to signal the input task
   -- that the server connection monitoring operation has timed out.
   package Timeout_Queue_Pkg is new PQueue (boolean);
   Timeout_Queue : Timeout_Queue_Pkg.Protected_Queue_Type;

------------------------------------------------------------------------------
--
-- Public task
--
------------------------------------------------------------------------------

   -- We declare the task object in public like this because it does not
   -- accept shutdown requests like most others, so must be explicitly
   -- aborted.  Also, unlike the other tasks, it has an actual entry.
   task type Ping_Task_Type is
      entry Input_Received;
   end Ping_Task_Type;
   Ping_Task :  Ping_Task_Type;

   ---------------------------------------------------------------------------

end Ping;
