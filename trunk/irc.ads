
with
  Ada.Strings.Unbounded;

use
  Ada.Strings.Unbounded;

package IRC is

   Null_Field:  constant Unbounded_String := Null_Unbounded_String;

   Max_Params:  constant natural := 15;

   RPL_ENDOFNAMES   : string := "366";
   RPL_ENDOFMOTD    : string := "376";

   type Message_Rec is record
      Prefix:   Unbounded_String := Null_Field;
      Command:  Unbounded_String;
      Params:   Unbounded_String;
   end record;

   subtype Param_Count is natural range 0 .. Max_Params;

   type Param_Arr is array ( 1 .. Max_Params ) of Unbounded_String;

   type MsgTo_Rec is record
      Nick:   Unbounded_String;
      User:   Unbounded_String;
      Host:   Unbounded_String;
   end record;

   Connect_Error:  exception;

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

end IRC;
