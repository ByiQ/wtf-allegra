
--
-- Input -- Server input task package for Allegra info-bot
--


package Input is

------------------------------------------------------------------------------
--
-- Public task
--
------------------------------------------------------------------------------

   -- We declare the task object in public like this because it does not
   -- accept shutdown requests like most others, so must be explicitly
   -- aborted.
   task type Input_Task_Type;
   Input_Task : Input_Task_Type;

   ---------------------------------------------------------------------------

end Input;
