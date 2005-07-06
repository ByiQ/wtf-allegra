package Config is

   Allegra_DB:     string := "allegra";
   Config_Tbl:     string := "config";
   Cmd_Auth_Tbl:   string := "cmdlevels";
   UserLvl_Tbl:    string := "usrlevels";

   type Config_Item is
     (
      Item_None,
      Item_ARMPath,
      Item_Channel,
      Item_Host,
      Item_LastSize,
      Item_LogLevel,
      Item_LogPath,
      Item_Nick,
      Item_NickPass,
      Item_Port,
      Item_Quips,
      Item_RealName,
      Item_Shorthand,
      Item_UserName
     );

   type Command_Type is
     (
      Cmd_None,
      Cmd_Access,
      Cmd_CkAccess,
      Cmd_Fetch,
      Cmd_Find,
      Cmd_Forget,
      Cmd_Help,
      Cmd_Last,
      Cmd_List,
      Cmd_Quit,
      Cmd_Quote,
      Cmd_Rename,
      Cmd_Reset,
      Cmd_Set,
      Cmd_Stats,
      Cmd_Tell
     );

   Min_Auth_Level : constant :=  0;
   Max_Auth_Level : constant := 10;
   subtype Auth_Level is integer range Min_Auth_Level .. Max_Auth_Level;

   function Get_Value (Item:  Config_Item)
                      return string;

   function Get_Auth_Level (Command:  Command_Type)
                           return Auth_Level;

   procedure Init;

end Config;
