
--
-- Identity -- Pre-processed body of general application identity package
--

package body Identity is

   ---------------------------------------------------------------------------

   Name   : constant string := $AppName;
   Number : constant string := $AppVer;
   Built  : constant string := $Build_Date;

   ---------------------------------------------------------------------------

   function App_ID return string is
   begin  -- App_ID
      return Name & " version " & Number & " of " & Built;
   end App_ID;

   ---------------------------------------------------------------------------

   function App_Name return string is
   begin  -- App_Name
      return Name;
   end App_Name;

   ---------------------------------------------------------------------------

   function App_Version return string is
   begin  -- App_Version
      return Number;
   end App_Version;

   ---------------------------------------------------------------------------

end Identity;

