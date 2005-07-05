
--
-- IRC -- Low-level IRC communications utility package
--


--
-- Standard packages
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;


package IRC is

------------------------------------------------------------------------------
--
-- Public constants
--
------------------------------------------------------------------------------

   -- Length of IRC server numeric reply fields
   Server_Reply_Length : constant := 3;

   -- As per RFC 2812
   Max_Params          : constant := 15;

   -- Server reply constants that we are interested in
   Min_Server_Reply    : constant := 001;
   RPL_WELCOME         : constant := 001;
   RPL_ENDOFMOTD       : constant := 376;
   ERR_UNKNOWNCOMMAND  : constant := 421;
   ERR_NOMOTD          : constant := 422;
   ERR_NICKNAMEINUSE   : constant := 433;
   Max_Server_Reply    : constant := 999;

   -- The client-to-client protocol (CTCP) marker
   CTCP_Marker         : constant character := ASCII.SOH;

   -- Used to represent a missing field value
   Null_Field          : constant Unbounded_String := Null_Unbounded_String;

------------------------------------------------------------------------------
--
-- Public types
--
------------------------------------------------------------------------------

   -- A standard IRC server reply string
   subtype Server_Reply_String is string (1 .. Server_Reply_Length);

   -- A server reply once we've decoded it
   subtype Server_Reply        is positive range Min_Server_Reply .. Max_Server_Reply;

   -- An IRC message split into its separate fields
   type Message_Rec is record
      Prefix  : Unbounded_String := Null_Field;
      Command : Unbounded_String;
      Params  : Unbounded_String;
   end record;

   subtype Param_Count is natural range 0 .. Max_Params;

   type Param_Arr is array ( 1 .. Max_Params ) of Unbounded_String;

   type MsgTo_Rec is record
      Nick:   Unbounded_String;
      User:   Unbounded_String;
      Host:   Unbounded_String;
   end record;

------------------------------------------------------------------------------
--
-- Public variables
--
------------------------------------------------------------------------------

   -- The exception most routines can raise when something goes wrong with the
   -- connection
   Connect_Error : exception;

------------------------------------------------------------------------------
--
-- Exported subroutines
--
------------------------------------------------------------------------------

   procedure Open_Server (Name:  in string;
                          Port:  in positive);

   procedure Close_Server;

   procedure Read (Message: out Message_Rec);

   procedure Write (Message: in Message_Rec);

   procedure Parse_Params (Param_Str:  in  Unbounded_String;
                           Params:     out Param_Arr;
                           Count:      out Param_Count);

   procedure Parse_MsgTo (Str:   in  Unbounded_String;
                          MsgTo: out MsgTo_Rec);

   ---------------------------------------------------------------------------

end IRC;
