
--
-- Ping -- Periodic server connection task package for Allegra info-bot
--


--
-- Standard packages
with Ada.Exceptions;


--
-- Local library packages
with Strings;
use  Strings;


--
-- Application packages
with CommandQ;
with Config;
with Log;
with OutputQ;


package body Ping is


------------------------------------------------------------------------------
--
-- Package constants
--
------------------------------------------------------------------------------

   -- This task's name, for logging purposes
   Ping_Name        : constant string := "Ping";

   -- The number of pings we can miss before we try reconnecting
   Max_Missed_Pings : constant := 2;

   -- The time in seconds we wait between sending pings to the server
   Ping_Delay       : constant Duration := 900.0;

------------------------------------------------------------------------------
--
-- The timeout semaphore
--
------------------------------------------------------------------------------

   -- Simple binary semaphore, used to tell the input task that a ping timeout
   -- has occurred
   protected body Timeout_Signal is

      -- Indicate that a timeout has occurred
      procedure Set is
      begin  -- Set
         Is_Set := true;
      end Set;

      -- Block until the signal is set, then clear it
      entry Wait when Is_Set is
      begin  -- Wait
         Is_Set := false;
      end Wait;
   end Timeout_Signal;

------------------------------------------------------------------------------
--
-- Public task
--
------------------------------------------------------------------------------

   task body Ping_Task_Type is

      -- Number of pings we've missed so far
      Missed         : Natural := 0;

      -- Request variable so we can send pings
      Output_Request : OutputQ.Request_Rec;

   begin  -- Ping_Task_Type

      -- Main task loop
      loop

         -- Do a selective accept, that will timeout after our delay interval
         -- if the input task doesn't call our entry to let us know we got
         -- some input
         select
            accept Input_Received;

            -- When we receive input, reset the missed-ping counter
            Missed := 0;
         or
            delay Ping_Delay;

            -- Come here if the accept timed out, and note that we missed
            -- another timeout
            Missed := Missed + 1;

            -- If we've missed too many, time the link out, and let the input
            -- task know by setting the timeout semaphore
            if Missed >= Max_Missed_Pings then
               Log.Info (Ping_Name, "Link timed out after " & Img (natural (Ping_Delay) * Missed) & " seconds");
               Timeout_Signal.Set;
               Missed := 0;

            -- Missed a ping, but not at our limit yet, so try again
            else
               Output_Request.Operation := OutputQ.Ping_Operation;
               Output_Request.Data := US (":" & Config.Get_Value (Config.Item_Host));
               OutputQ.Requests.Enqueue (Output_Request);
            end if;
         end select;
      end loop;

   exception
      when E : others =>
         Log.Err (Ping_Name, "Exception " & Ada.Exceptions.Exception_Information (E));
         CommandQ.Crash (Ping_Name);
   end Ping_Task_Type;

   ---------------------------------------------------------------------------

end Ping;
