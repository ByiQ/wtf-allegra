
--
-- CommandQ -- Command-processing task request queue for Allegra info-bot
--


--
-- Local library packages
with Strings;
use  Strings;


package body CommandQ is

------------------------------------------------------------------------------
--
-- Public subroutines
--
------------------------------------------------------------------------------

   -- Cause the bot to do an emergency shutdown, by telling the command task
   -- to crash it
   procedure Crash (Who : in string) is

      Crash_Request : Request_Rec;

   begin  -- Crash
      Crash_Request.Operation := Crash_Operation;
      Crash_Request.Data      := US (Who);
      Requests.Enqueue (Crash_Request);
   end Crash;

   ---------------------------------------------------------------------------

end CommandQ;
