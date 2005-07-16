
--
-- Times -- Timestamp manipulation utility package
--


--
-- Standard packages
with Ada.Calendar;


package Times is

------------------------------------------------------------------------------
--
-- Public types
--
------------------------------------------------------------------------------

   -- Our own private timestamp type
   subtype Timestamp is Ada.Calendar.Time;

------------------------------------------------------------------------------
--
-- Public subroutines
--
------------------------------------------------------------------------------

   -- Return current system time
   function Current return Timestamp;
   pragma Inline (Current);

   -- Return the date portion of the given timestamp as a string.  The default
   -- (long) form is "DD Mmm YYYY", and the short form is the US "MM/DD/YY".
   function Date_String (DateOf       : in Timestamp;
                         Short_Format : in boolean := false) return string;

   -- Return the date portion of the current system time as a string.  The
   -- default (long) form is "DD Mmm YYYY", and the short form is the US
   -- "MM/DD/YY".
   function Date_String (Short_Format : in boolean := false) return string;

   -- Return the date portion of the given timestamp broken down into three
   -- integers.  Year is returned as the full year value, eg. 2005
   procedure Date_Value (DateOf : in  Timestamp;
                         Month  : out natural;
                         Day    : out natural;
                         Year   : out natural);

   -- Return the date portion of the current system time broken down into
   -- three integers.  Year is returned as the full year value, eg. 2005
   procedure Date_Value (Month : out natural;
                         Day   : out natural;
                         Year  : out natural);

   -- Return the time elapsed from a given time until now, in a conversational
   -- form like "D days, H hours, and M minutes"
   function Elapsed (From : in Timestamp) return string;

   -- Return the time portion of the given timestamp as a string.  The default
   -- (long) format is "HH:MM:SS", and the short format is "HH:MM".
   function Time_String (TimeOf       : in Timestamp;
                         Short_Format : in boolean := false) return string;

   -- Return the time portion of the current system time as a string.  The
   -- default (long) format is "HH:MM:SS", and the short format is "HH:MM".
   function Time_String (Short_Format : in boolean := false) return string;

   -- Return the time portion of the given timestamp broken down into three
   -- integers.
   procedure Time_Value (TimeOf : in  Timestamp;
                         Hour   : out natural;
                         Minute : out natural;
                         Second : out natural);

   -- Return the time portion of the current system time broken down into
   -- three integers.
   procedure Time_Value (Hour   : out natural;
                         Minute : out natural;
                         Second : out natural);

   ---------------------------------------------------------------------------

end Times;
