with
  Ada.Command_Line,
  Ada.Strings.Unbounded,
  DB;

use
  Ada.Command_Line,
  Ada.Strings.Unbounded;

package body Config is

   protected Access_Control is
      procedure Data_Available;
      entry Wait_For_Data;
   private
      Is_Available:  boolean := false;
   end Access_Control;

   protected body Access_Control is
      procedure Data_Available is
      begin
         Is_Available := true;
      end Data_Available;

      entry Wait_For_Data when Is_Available is
      begin
         null;
      end Wait_For_Data;
   end Access_Control;

   Name_Len_Max:  constant := 16;

   subtype Item_Name is string (1 .. Name_Len_Max);

   type Config_Mapping_Rec is record
      Name:  Item_Name;
      Item:  Config_Item;
   end record;

   type Cfg_Map_Array is array ( positive range <> ) of Config_Mapping_Rec;

   type Cmd_Auth_Mapping_Rec is record
      Name:  Item_Name;
      Item:  Command_Type;
   end record;

   type Cmd_Map_Array is array ( positive range <> ) of Cmd_Auth_Mapping_Rec;

   Cfg_Map:  Cfg_Map_Array :=
     (
      ("                ", Item_None),
      ("armpath         ", Item_ARMPath),
      ("channel         ", Item_Channel),
      ("host            ", Item_Host),
      ("lastsize        ", Item_LastSize),
      ("loglevel        ", Item_LogLevel),
      ("logpath         ", Item_LogPath),
      ("nick            ", Item_Nick),
      ("nickpass        ", Item_NickPass),
      ("port            ", Item_Port),
      ("quips           ", Item_Quips),
      ("realname        ", Item_RealName),
      ("shorthand       ", Item_Shorthand),
      ("username        ", Item_UserName)
     );

   Cmd_Map:  Cmd_Map_Array :=
     (
      ("                ", Cmd_None),
      ("ckaccess        ", Cmd_CkAccess),
      ("fetch           ", Cmd_Fetch),
      ("find            ", Cmd_Find),
      ("forget          ", Cmd_Forget),
      ("help            ", Cmd_Help),
      ("last            ", Cmd_Last),
      ("list            ", Cmd_List),
      ("myaccess        ", Cmd_MyAccess),
      ("quit            ", Cmd_Quit),
      ("quote           ", Cmd_Quote),
      ("rename          ", Cmd_Rename),
      ("reset           ", Cmd_Reset),
      ("set             ", Cmd_Set),
      ("setaccess       ", Cmd_SetAccess),
      ("stats           ", Cmd_Stats),
      ("tell            ", Cmd_Tell)
     );

   Config_Values:  array ( Config_Item ) of Unbounded_String;

   Cmd_Auth_Levels:  array ( Command_Type ) of Auth_Level;

   function Get_Value (Item:  Config_Item)
                      return string is
   begin  -- Get_Value
      Access_Control.Wait_For_Data;
      return To_String (Config_Values (Item));
   end Get_Value;

   function Get_Auth_Level (Command:  Command_Type)
                           return Auth_Level is
   begin  -- Get_Auth_Level
      Access_Control.Wait_For_Data;
      return Cmd_Auth_Levels (Command);
   end Get_Auth_Level;

   procedure Init is

      Handle:    DB.DB_Handle;
      Auth_Data: DB.DB_Result;
      Cfg_Data:  DB.DB_Result;
      Item:      Config_Item;
      Cmd:       Command_Type;

   begin  -- Init
      DB.Connect (Handle, Host => "", DB => Allegra_DB);
      DB.Fetch (Handle, "*", Config_Tbl,   "", Cfg_Data);
      DB.Fetch (Handle, "*", Cmd_Auth_Tbl, "", Auth_Data);
      for Row in 1 .. DB.Rows (Cfg_Data) loop
         declare
            Name:  string := DB.Get_Value (Cfg_Data, Row, "name");
         begin
            Item := Item_None;
            for Index in Cfg_Map'Range loop
               if Cfg_Map (Index).Name (1 .. Name'Length) = Name then
                  Item := Cfg_Map (Index).Item;
                  exit;
               end if;
            end loop;
            if Item /= Item_None then
               declare
                  Value_Str:  string := DB.Get_Value (Cfg_Data, Row, "value");
               begin
                  Config_Values (Item) := To_Unbounded_String (Value_Str);
               end;
            end if;
         end;
      end loop;
      for Row in 1 .. DB.Rows (Auth_Data) loop
         declare
            Name:  string := DB.Get_Value (Auth_Data, Row, "name");
         begin
            Cmd := Cmd_None;
            for Index in Cmd_Map'Range loop
               if Cmd_Map (Index).Name (1 .. Name'Length) = Name then
                  Cmd := Cmd_Map (Index).Item;
                  exit;
               end if;
            end loop;
            if Cmd /= Cmd_None then
               Cmd_Auth_Levels (Cmd) := DB.Get_Value (Auth_Data, Row, "level");
            end if;
         end;
      end loop;
      if Argument_Count >= 1 then
         Config_Values (Item_Host) := To_Unbounded_String (Argument (1));
      end if;
      if Argument_Count >= 2 then
         Config_Values (Item_Port) := To_Unbounded_String (Argument (2));
      end if;
      if Argument_Count >= 3 then
         Config_Values (Item_Nick) := To_Unbounded_String (Argument (3));
      end if;
      if Argument_Count >= 4 then
         Config_Values (Item_Channel) := To_Unbounded_String (Argument (4));
      end if;
      if Argument_Count >= 5 then
         Config_Values (Item_LogPath) := To_Unbounded_String (Argument (5));
      end if;
      DB.Disconnect (Handle);
      Access_Control.Data_Available;
   end Init;

begin  -- Config
   Init;
end Config;
