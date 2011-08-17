
--
-- Ping -- Periodic server connection task package for Allegra info-bot
--


--
-- Local library packages
with PQueue;


package Ping is

------------------------------------------------------------------------------
--
-- The timeout semaphore
--
------------------------------------------------------------------------------

   -- This semaphore is used to tell the timeout task that a ping timeout has
   -- occurred
   protected Timeout_Signal is
      procedure Set;
      entry     Wait;
   private
      Is_Set : boolean := false;
   end Timeout_Signal;

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
