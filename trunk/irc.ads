
--
-- IRC -- Low-level IRC communications utility package
--


--
-- Local library packages
with Strings;
use  Strings;


package IRC is

------------------------------------------------------------------------------
--
-- Public constants
--
------------------------------------------------------------------------------

   -- Length of IRC server numeric reply fields
   Server_Reply_Length : constant := 3;

   -- As per RFC 2812, maximum parameters in an IRC message
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
   Null_Field          : constant UString := Null_UString;

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
      Prefix  : UString := Null_Field;
      Command : UString;
      Params  : UString;
   end record;

   -- The array in which we return parsed message parameters
   subtype Param_Count is natural range 0 .. Max_Params;
   type Param_Arr is array (1 .. Max_Params) of UString;

   type MsgTo_Rec is record
      Nick : UString;
      User : UString;
      Host : UString;
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
-- Public subroutines
--
------------------------------------------------------------------------------

   -- Connect to given IRC server on given port
   procedure Open_Server (Name : in string;
                          Port : in positive);

   -- Close server connection
   procedure Close_Server;

   -- Read a message from the server and parse into its components; blocks
   -- until one is available
   procedure Read (Message : out Message_Rec);

   -- Write a message to the server
   procedure Write (Message : in Message_Rec);

   -- Parse a server message parameter string into separate fields
   procedure Parse_Params (Param_Str : in  UString;
                           Params    : out Param_Arr;
                           Count     : out Param_Count);

   -- Parse an IRC "msgto" string into nick/user/host fields
   procedure Parse_MsgTo (Str   : in  UString;
                          MsgTo : out MsgTo_Rec);

   ---------------------------------------------------------------------------

end IRC;
