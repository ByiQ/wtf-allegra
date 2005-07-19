
--
-- Strings -- String manipulation utility package
--


--
-- Standard packages
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;


package body Strings is

------------------------------------------------------------------------------
--
-- Public subroutines
--
------------------------------------------------------------------------------

   -- Version of the 'Image attribute that does not include the leading space
   -- for positive values
   function Img (Num : in integer) return string is

      Result : string := integer'Image (Num);

   begin  -- Img
      if Result (Result'First) = ' ' then
         return Result (Result'First + 1 .. Result'Last);
      else
         return Result;
      end if;
   end Img;

   ---------------------------------------------------------------------------

   -- Return number padded on the left with Pad out to Width; returns full
   -- number if it's wider than Width.  Note that negative integers with
   -- non-blank Pad values don't work well.
   function Img (Number : in integer;
                 Width  : in positive;
                 Pad    : in character := ' ') return string is

      use Ada.Strings.Fixed;

      Image : string := Trim (natural'Image (Number), Side => Ada.Strings.Left);

   begin  -- Img
      if Image'Length >= Width then
         return Image;
      else
         return ((Width - Image'Length) * Pad) & Image;
      end if;
   end Img;

   ---------------------------------------------------------------------------

   -- Like regular Trim, but always trims both ends
   function BTrim (Source : in string) return string is
   begin  -- BTrim
      return Ada.Strings.Fixed.Trim (Source, Side => Ada.Strings.Both);
   end BTrim;

   ---------------------------------------------------------------------------

   -- Like regular Trim, but always trims on the left
   function LTrim (Source : in string) return string is
   begin  -- LTrim
      return Ada.Strings.Fixed.Trim (Source, Side => Ada.Strings.Left);
   end LTrim;

   ---------------------------------------------------------------------------

   -- Like regular Trim, but always trims on the right
   function RTrim (Source : in string) return string is
   begin  -- RTrim
      return Ada.Strings.Fixed.Trim (Source, Side => Ada.Strings.Right);
   end RTrim;

   ---------------------------------------------------------------------------

   -- Convenient for avoiding "use Ada.Strings.Unbounded"
   function Equal (Left, Right : in UString) return boolean is
   begin  -- Equal
      return Left = Right;
   end Equal;

   ---------------------------------------------------------------------------

end Strings;
