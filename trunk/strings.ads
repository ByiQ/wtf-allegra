
--
-- Strings -- String manipulation utility package
--


--
-- Standard packages
with Ada.Strings.Unbounded;


package Strings is

------------------------------------------------------------------------------
--
-- Public types
--
------------------------------------------------------------------------------

   -- Give this one a shorter name, since it's used so much
   subtype UString is Ada.Strings.Unbounded.Unbounded_String;

------------------------------------------------------------------------------
--
-- Public subroutines
--
------------------------------------------------------------------------------

   -- Version of the 'Image attribute that does not include the leading space
   -- for positive values
   function Img (Num : in integer) return string;

   -- Return number padded on the left with Pad out to Width; returns full
   -- number if it's wider than Width.  Note that negative integers with
   -- non-blank Pad values don't work well.
   function Img (Number : in integer;
                 Width  : in positive;
                 Pad    : in character := ' ') return string;

   -- Rename these to shorter names, since we use them so much
   function S  (Source : in Ada.Strings.Unbounded.Unbounded_String) return string
     renames Ada.Strings.Unbounded.To_String;

   function US (Source : in string) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   ---------------------------------------------------------------------------

end Strings;
