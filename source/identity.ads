
--
-- Identity -- General application identity package
--

package Identity is

------------------------------------------------------------------------------
--
-- Public subroutines
--
------------------------------------------------------------------------------

   -- Composite application identity string, "<appname> version <ver> of <build-date>"
   function App_ID      return string;

   -- Application name only
   function App_Name    return string;

   -- Application version only
   function App_Version return string;

   ---------------------------------------------------------------------------

end Identity;

