
--
-- Identity -- Pre-processed body of general application identity package
--

package body Identity is

------------------------------------------------------------------------------
--
-- Package constants
--
------------------------------------------------------------------------------

   -- These values are filled in by gnatprep at build time
   Name   : constant string := $AppName;
   Number : constant string := $AppVer;
   Built  : constant string := $Build_Date;

------------------------------------------------------------------------------
--
-- Public subroutines
--
------------------------------------------------------------------------------

   -- Composite application identity string, "<appname> version <ver> of <build-date>"
   function App_ID return string is
   begin  -- App_ID
      return Name & " version " & Number & " of " & Built;
   end App_ID;

   ---------------------------------------------------------------------------

   -- Application name only
   function App_Name return string is
   begin  -- App_Name
      return Name;
   end App_Name;

   ---------------------------------------------------------------------------

   -- Application version only
   function App_Version return string is
   begin  -- App_Version
      return Number;
   end App_Version;

   ---------------------------------------------------------------------------

end Identity;

