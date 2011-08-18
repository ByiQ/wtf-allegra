
--
-- Config -- Program configuration utility package for Allegra info-bot
--


package Config is

------------------------------------------------------------------------------
--
-- Public constants
--
------------------------------------------------------------------------------


   -- Name of the database host, new for PostgreSQL 8
   DB_Hostname    : constant string := "";

   -- Names of our database tables
   Allegra_DB     : constant string := "allegra";
   Config_Tbl     : constant string := "config";
   Cmd_Auth_Tbl   : constant string := "cmdlevels";
   Cmd_Stat_Tbl   : constant string := "cmdstats";
   UserLvl_Tbl    : constant string := "usrlevels";
   ActQuip_Tbl    : constant string := "actquips";
   MsgQuip_Tbl    : constant string := "quips";

   -- Maximum length of a configuration item name or command name
   Name_Len_Max   : constant := 16;

   -- Range of authorization levels
   Min_Auth_Level : constant :=  0;
   Max_Auth_Level : constant := 10;

   -- Pause between each line of multi-line output, in seconds
   Line_Pause     : constant Duration := 0.75;

------------------------------------------------------------------------------
--
-- Public types
--
------------------------------------------------------------------------------

   -- Known configuration items
   type Config_Item is
     (
      Item_None,
      Item_ARMPath,
      Item_Channel,
      Item_HelpPath,
      Item_Host,
      Item_HTTP_Action,
      Item_HTTP_Method,
      Item_HTTP_Port,
      Item_LastSize,
      Item_Line_Pause,
      Item_LogLevel,
      Item_LogPath,
      Item_Nick,
      Item_NickPass,
      Item_Nickserv_Notice,
      Item_Port,
      Item_Quips,
      Item_RealName,
      Item_Shorten_URL,
      Item_Shorthand,
      Item_UserName
     );

   -- The bot's commands
   type Command_Type is
     (
      Cmd_None,
      Cmd_CkAccess,
      Cmd_Fetch,
      Cmd_Find,
      Cmd_Forget,
      Cmd_Help,
      Cmd_Last,
      Cmd_List,
      Cmd_MyAccess,
      Cmd_Quit,
      Cmd_Quote,
      Cmd_Rename,
      Cmd_Reset,
      Cmd_Set,
      Cmd_SetAccess,
      Cmd_Short,
      Cmd_Stats,
      Cmd_Tell
     );

   -- The range of commands that can actually be issued by users
   subtype Valid_Commands is Command_Type range Command_Type'Succ (Cmd_None) .. Command_Type'Last;

   -- A name type, used for configuration item names and command names
   subtype Item_Name is string (1 .. Name_Len_Max);

   -- Range of authorization levels
   subtype Auth_Level is integer range Min_Auth_Level .. Max_Auth_Level;

------------------------------------------------------------------------------
--
-- The command name table
--
------------------------------------------------------------------------------

   -- The canonical command names, used for individual command help messages
   -- and statistics.  Must agree with the entries in the cmdlevels and
   -- cmdstats database tables.
   type Cmd_Name_Array is array (Command_Type) of Item_Name;

   Cmd_Names : Cmd_Name_Array :=
     (
      Cmd_None      => "                ",
      Cmd_CkAccess  => "ckaccess        ",
      Cmd_Fetch     => "fetch           ",
      Cmd_Find      => "find            ",
      Cmd_Forget    => "forget          ",
      Cmd_Help      => "help            ",
      Cmd_Last      => "last            ",
      Cmd_List      => "list            ",
      Cmd_MyAccess  => "myaccess        ",
      Cmd_Quit      => "quit            ",
      Cmd_Quote     => "quote           ",
      Cmd_Rename    => "rename          ",
      Cmd_Reset     => "reset           ",
      Cmd_Set       => "set             ",
      Cmd_SetAccess => "setaccess       ",
      Cmd_Short     => "short           ",
      Cmd_Stats     => "stats           ",
      Cmd_Tell      => "tell            "
     );

------------------------------------------------------------------------------
--
-- Public subroutines
--
------------------------------------------------------------------------------

   -- Return ratio of action quips to message quips
   function Act_Vs_Msg return float;

   -- Return a command's usage count
   function Command_Usage (Command : in Command_Type) return natural;

   -- Increment a command's usage count by one
   procedure Command_Used (Command : in Command_Type);

   -- Return the required auth level for the given command
   function Get_Auth_Level (Command : in Command_Type) return Auth_Level;

   -- Return the value of given configuration item
   function Get_Value (Item : in Config_Item) return string;

   -- Save any cached configuration values to the db
   procedure WrapUp;

   ---------------------------------------------------------------------------

end Config;
