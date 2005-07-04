
with
  Ada.Unchecked_Deallocation,
  Ada.Strings.Unbounded,
  Ada.Text_IO,
  GNAT.Regexp,
  Config,
  IRC,
  DB;

package body Auth is

   Default_Auth_Level : constant := 1;

   type User_Rec;
   type User_Ptr is access User_Rec;
   type User_Rec is record
      ID:     Unbounded_String;
      Level:  Config.Auth_Level;
      Next:   User_Ptr;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (User_Rec, User_Ptr);

   User_Head:  User_Ptr := null;
   User_Tail:  User_Ptr;

   function Level (Who:  in Unbounded_String)
                  return Config.Auth_Level is

      User : User_Ptr;

   begin  -- Level
      User := User_Head;
      while User /= null loop
         exit when Match (Who, User.ID) = Succeeded;
         User := User.Next;
      end loop;

      if User = null then
         return Default_Auth_Level;
      else
         return User.Level;
      end if;
   end Level;

   function Match (Who:  in Unbounded_String;
                   Mask: in Unbounded_String)
                  return Authorization is

      use GNAT.Regexp;

      Check  : IRC.MsgTo_Rec;
      UserID : IRC.MsgTo_Rec;

   begin  -- Match
      IRC.Parse_MsgTo (Who,  UserID);
      IRC.Parse_MsgTo (Mask, Check);
      if not Match (To_String (UserID.Nick), Compile (To_String (Check.Nick), Glob => true, Case_Sensitive => false)) then
         return Fail_Nick;
      end if;
      if not Match (To_String (UserID.User), Compile (To_String (Check.User), Glob => true, Case_Sensitive => false)) then
         return Fail_User;
      end if;
      if not Match (To_String (UserID.Host), Compile (To_String (Check.Host), Glob => true, Case_Sensitive => false)) then
         return Fail_Host;
      end if;
      return Succeeded;
   end Match;

   function Permitted (Who:  in Unbounded_String;
                       Cmd:  in Config.Command_Type)
                      return Authorization is
   begin  -- Permitted
      if Level (Who) >= Config.Get_Auth_Level (Cmd) then
         return Succeeded;
      else
         return Fail_Nick;
      end if;
   end Permitted;

   procedure Init is

      Handle    : DB.DB_Handle;
      Auth_Data : DB.DB_Result;
      Next      : User_Ptr;

   begin  -- Init
      while User_Head /= null loop
         Next := User_Head.Next;
         Free (User_Head);
      end loop;

      DB.Connect (Handle, Host => "", DB => Config.Allegra_DB);
      DB.Fetch (Handle, "*", Config.UserLvl_Tbl, "", Auth_Data);
      for Row in 1 .. DB.Rows (Auth_Data) loop
         declare
            UName    : string := DB.Get_Value (Auth_Data, Row, "name");
            Name     : Unbounded_String := To_Unbounded_String (UName);
            Level    : Config.Auth_Level := DB.Get_Value (Auth_Data, Row, "level");
            New_User : User_Ptr := new User_Rec'(ID => Name, Level => Level, Next => null);
         begin
            if User_Head = null then
               User_Head := New_User;
            else
               User_Tail.Next := New_User;
            end if;
            User_Tail := New_User;
         end;
      end loop;
      DB.Disconnect (Handle);
   end Init;

begin  -- package Auth initialization
   Init;
end Auth;
