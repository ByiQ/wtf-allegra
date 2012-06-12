
--
-- Identity -- General application identity package
--

package Identity is

------------------------------------------------------------------------------
--
-- Public constants
--
------------------------------------------------------------------------------

   -- Application name only
   App_Name    : constant string := $App_Name;

   -- Application version only
   App_Version : constant string := $App_Version;

   -- Build date
   Built       : constant string := $Build_Date;

   -- Composite application identity string, "<appname> version <ver> of <build-date>"
   App_ID      : constant String := App_Name & " version " & App_Version & " of " & Built;

end Identity;

