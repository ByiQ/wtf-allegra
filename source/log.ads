
--
-- Log -- Log file management utility package for Allegra info-bot
--


package Log is

------------------------------------------------------------------------------
--
-- Public subroutines
--
------------------------------------------------------------------------------

   -- Logging procedures, for successive verbosity levels.  Each will only
   -- write to the log file if the configured logging level allows it.
   procedure Err  (From : in String;   Msg : in String);
   procedure Warn (From : in String;   Msg : in String);
   procedure Info (From : in String;   Msg : in String);
   procedure Dbg  (From : in String;   Msg : in String);

   -- Initialization and wrapup procedures
   procedure Init;
   procedure WrapUp;

   ---------------------------------------------------------------------------

end Log;
