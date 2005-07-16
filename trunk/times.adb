
--
-- Times -- Timestamp manipulation utility package
--


--
-- Standard packages
with Ada.Calendar;


--
-- Local library packages
with Strings;


package body Times is

------------------------------------------------------------------------------
--
-- Package constants
--
------------------------------------------------------------------------------

   -- A couple of useful time constants
   OneDay  : constant := natural (Ada.Calendar.Day_Duration'Last);
   OneHour : constant := 60 * 60;

   -- Month name abbreviation table
   Month_Names : constant array (Ada.Calendar.Month_Number) of string (1 .. 3) :=
     ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec");

------------------------------------------------------------------------------
--
-- Public subroutines
--
------------------------------------------------------------------------------

   -- Return current system time
   function Current return Timestamp is
   begin  -- Current
      return Ada.Calendar.Clock;
   end Current;

   ---------------------------------------------------------------------------

   -- Return the date portion of the given timestamp as a string.  The default
   -- (long) form is "DD Mmm YYYY", and the short form is the US "MM/DD/YY".
   function Date_String (DateOf       : in Timestamp;
                         Short_Format : in boolean := false) return string is

      use Strings;

      YYYY : string (1 .. 4);

   begin  -- Date_String

      -- Get the year as a full four-digit string.  Will fail in the year
      -- 10000, awww.
      YYYY := Img (Ada.Calendar.Year (DateOf), 4, Pad => '0');

      -- Return the requested format
      if Short_Format then
         return Img (Ada.Calendar.Month (DateOf), 2, Pad => '0') & "/" &
                Img (Ada.Calendar.Day   (DateOf), 2, Pad => '0') & "/" &
                YYYY (3 .. 4);
      else
         return Img (Ada.Calendar.Day   (DateOf), 2)     & " " &
                Month_Names (Ada.Calendar.Month (DateOf)) & " " &
                YYYY;
      end if;
   end Date_String;

   ---------------------------------------------------------------------------

   -- Return the date portion of the current system time as a string.  The
   -- default (long) form is "DD Mmm YYYY", and the short form is the US
   -- "MM/DD/YY".
   function Date_String (Short_Format : in boolean := false) return string is
   begin  -- Date_String
      return Date_String (Current, Short_Format);
   end Date_String;

   ---------------------------------------------------------------------------

   -- Return the date portion of the given timestamp broken down into three
   -- integers.  Year is returned as the full year value, eg. 2005
   procedure Date_Value (DateOf : in  Timestamp;
                         Month  : out natural;
                         Day    : out natural;
                         Year   : out natural) is
   begin  -- Date_Value
      Month := Ada.Calendar.Month (DateOf);
      Day   := Ada.Calendar.Day   (DateOf);
      Year  := Ada.Calendar.Year  (DateOf);
   end Date_Value;

   ---------------------------------------------------------------------------

   -- Return the date portion of the current system time broken down into
   -- three integers.  Year is returned as the full year value, eg. 2005
   procedure Date_Value (Month : out natural;
                         Day   : out natural;
                         Year  : out natural) is
   begin  -- Date_Value
      Date_Value (Current, Month, Day, Year);
   end Date_Value;

   ---------------------------------------------------------------------------

   -- Return the time elapsed from a given time until now, in a conversational
   -- form like "D days, H hours, and M minutes"
   function Elapsed (From : in Timestamp) return string is

      use Ada.Calendar, Strings;

      type MilliMin is delta 0.001 range Duration'First .. Duration'Last;

      Diff    : natural := natural (Current - From);
      Days    : natural;
      Hours   : natural;
      Minutes : MilliMin;
      Answer  : UString;

   begin  -- Elapsed
      Days    := Diff / OneDay;
      Hours   := (Diff mod OneDay) / OneHour;
      Minutes := Millimin (Diff mod OneHour) / 60.0;

      if Days > 0 then
         Answer := US (natural'Image (Days));
         if Days > 1 then
            Answer := Answer & " days";
         else
            Answer := Answer & " day";
         end if;
         if Hours > 0 then
            Answer := Answer & ",";
         else
            Answer := Answer & " and";
         end if;
      else
         Answer := Null_UString;
      end if;
      if Hours > 0 then
         Answer := Answer & natural'Image (Hours);
         if Hours > 1 then
            Answer := Answer & " hours and";
         else
            Answer := Answer & " hour and";
         end if;
      end if;
      Answer := Answer & MilliMin'Image (Minutes) & " minutes";
      return LTrim (S (Answer));
   end Elapsed;

   ---------------------------------------------------------------------------

   -- Return the time portion of the given timestamp as a string.  The default
   -- (long) format is "HH:MM:SS", and the short format is "HH:MM".
   function Time_String (TimeOf       : in Timestamp;
                         Short_Format : in boolean := false) return string is

      ------------------------------------------------------------------------

      use Strings;

      ------------------------------------------------------------------------

      HH:  natural;
      MM:  natural;
      SS:  natural;

      ------------------------------------------------------------------------

   begin  -- Time_String

      -- Calculate hours, minutes, and seconds
      Time_Value (TimeOf, HH, MM, SS);

      -- Return the requested format
      if Short_Format then
         return Img (HH, 2, Pad => ' ') & ":" &
                Img (MM, 2, Pad => '0');
      else
         return Img (HH, 2, Pad => ' ') & ":" &
                Img (MM, 2, Pad => '0') & ":" &
                Img (SS, 2, Pad => '0');
      end if;
   end Time_String;

   ---------------------------------------------------------------------------

   -- Return the time portion of the current system time as a string.  The
   -- default (long) format is "HH:MM:SS", and the short format is "HH:MM".
   function Time_String (Short_Format : in boolean := false) return string is
   begin  -- Time_String
      return Time_String (Current, Short_Format);
   end Time_String;

   ---------------------------------------------------------------------------

   -- Return the time portion of the given timestamp broken down into three
   -- integers.
   procedure Time_Value (TimeOf : in  Timestamp;
                         Hour   : out natural;
                         Minute : out natural;
                         Second : out natural) is

      HH : natural;
      MM : natural;
      SS : natural;

   begin  -- Time_Value

      -- Start with seconds since midnight
      SS := natural (Ada.Calendar.Seconds (TimeOf));

      -- Guard against overflow at midnight; sometimes the Seconds function
      -- returns a full day's worth
      if SS >= OneDay then
         SS := 0;
      end if;

      -- Split it up into its sexagesimal components
      HH := SS / 3600;
      SS := SS - HH * 3600;
      MM := SS / 60;
      SS := SS - MM * 60;
      Hour   := HH;
      Minute := MM;
      Second := SS;
   end Time_Value;

   ---------------------------------------------------------------------------

   -- Return the time portion of the current system time broken down into
   -- three integers.
   procedure Time_Value (Hour   : out natural;
                         Minute : out natural;
                         Second : out natural) is
   begin  -- Time_Value
      Time_Value (Current, Hour, Minute, Second);
   end Time_Value;

   ---------------------------------------------------------------------------

end Times;
