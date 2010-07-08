
--
-- Allegra IRC info-bot main procedure
--


--
-- Application packages, included here to ensure that they are bound into the
-- application and are elaborated.
with Command;
with Database;
with File;
with Input;
with Net;
with Output;
with Ping;


procedure Allegra is
begin  -- Allegra

   -- The main thread just quits immediately; all the work is done in the task
   -- packages.
   null;
end Allegra;
