
--
-- Auth -- Command authorization utility package for Allegra info-bot
--


--
-- Local library packages
with Strings;
use  Strings;


--
-- Application packages
with Config;


package Auth is

------------------------------------------------------------------------------
--
-- Public constants
--
------------------------------------------------------------------------------

   -- This auth level has ultimate power over the bot
   Bot_Operator_Level : constant := 10;

------------------------------------------------------------------------------
--
-- Public types
--
------------------------------------------------------------------------------

   -- Whether an auth request succeeded, or why it failed
   type Authorization is (Succeeded, Fail_Nick, Fail_User, Fail_Host, Fail_Level);

------------------------------------------------------------------------------
--
-- Public types
--
------------------------------------------------------------------------------

   -- Returns the current command authorization level for a user, by matching
   -- the given user ID against the list of usermasks.
   function Level (Who : in UString) return Config.Auth_Level;

   -- Match a user ID against a usermask.  Returns Succeeded if it matches, or
   -- one of the Fail* values to indicate which part was the first to fail.
   function Match (Who  : in UString;
                   Mask : in UString) return Authorization;

   -- Returns Succeeded if given user is permitted to do given command.
   -- Returns a generic "FailNick" if not.
   function Permitted (Who : in UString;
                       Cmd : in Config.Command_Type) return Authorization;

   -- Initialize the cached user auth list by (re)reading it from the database
   procedure Init;

   ---------------------------------------------------------------------------

end Auth;
