
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
   procedure Err  (From : in string;   Msg : in string);
   procedure Warn (From : in string;   Msg : in string);
   procedure Info (From : in string;   Msg : in string);
   procedure Dbg  (From : in string;   Msg : in string);

   -- Initialization and wrapup procedures
   procedure Init;
   procedure WrapUp;

   ---------------------------------------------------------------------------

end Log;
