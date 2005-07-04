
with
  Ada.Strings.Unbounded,
  Config;

use
  Ada.Strings.Unbounded;

package Auth is

   Bot_Operator_Level : constant := 10;

   type Authorization is (Succeeded, Fail_Nick, Fail_User, Fail_Host, Fail_Level);

   function Level (Who:  in Unbounded_String)
                  return Config.Auth_Level;

   function Match (Who:  in Unbounded_String;
                   Mask: in Unbounded_String)
                  return Authorization;

   function Permitted (Who:  in Unbounded_String;
                       Cmd:  in Config.Command_Type)
                      return Authorization;

   procedure Init;

end Auth;
