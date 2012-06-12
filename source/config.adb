
--
-- Config -- Program configuration utility package for Allegra info-bot
--

--
-- Standard packages
with Ada.Command_Line;

--
-- Local library packages
with Strings;
use  Strings;


--
-- Application packages
with DB;


package body Config is

------------------------------------------------------------------------------
--
-- Access control semaphore
--
------------------------------------------------------------------------------

   -- This package returns config values from memory variables where it caches
   -- the values after reading them from the database at init time.  This
   -- semaphore is used to prevent other tasks from fetching config items
   -- before the init code can initalize the cache.  The Wait_For_Data entry
   -- should be called by all config routines before they try to use the
   -- cached values.  The Data_Available procedure is called by the init code
   -- once the cache variables have all been set.
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

------------------------------------------------------------------------------
--
-- Table to map configuration item names to their corresponding enum values
--
------------------------------------------------------------------------------

   -- The configuration item names
   type Cfg_Map_Array is array (Config_Item) of Item_Name;
   Cfg_Map : constant Cfg_Map_Array :=
     (
      Item_None             => "                ",
      Item_ARMPath          => "armpath         ",
      Item_Channel          => "channel         ",
      Item_CTCP_Location    => "ctcplocation    ",
      Item_HelpPath         => "helppath        ",
      Item_Host             => "host            ",
      Item_LastSize         => "lastsize        ",
      Item_Line_Pause       => "linepause       ",
      Item_LogLevel         => "loglevel        ",
      Item_LogPath          => "logpath         ",
      Item_Nick             => "nick            ",
      Item_NickPass         => "nickpass        ",
      Item_Nickserv_Notice  => "nickservnotice  ",
      Item_Port             => "port            ",
      Item_Quips            => "quips           ",
      Item_RealName         => "realname        ",
      Item_Shortener_Action => "httpaction      ",
      Item_Shortener_Method => "httpmethod      ",
      Item_Shortener_Port   => "httpport        ",
      Item_Shortener_Host   => "shortenurl      ",
      Item_Shorthand        => "shorthand       ",
      Item_UserName         => "username        "
     );

------------------------------------------------------------------------------
--
-- Package variables
--
------------------------------------------------------------------------------

   -- The cache variables.  Cmd_Usages is really more of a database thing than
   -- a configuration thing, but the code sure is a lot nicer with it here.
   Config_Values   : array (Config_Item)  of UString;
   Cmd_Auth_Levels : array (Command_Type) of Auth_Level;
   Cmd_Usages      : array (Command_Type) of natural;

   -- Another cache value, representing the number of action quips versus the
   -- number of message quips, so we can distribute our quipping evenly over
   -- both tables.
   Action_Message_Ratio : float := 0.0;

------------------------------------------------------------------------------
--
-- Package subroutines
--
------------------------------------------------------------------------------

   -- Initialize the cache variables from the database
   procedure Init is

      Handle    : DB.DB_Handle;
      Auth_Data : DB.DB_Result;
      Cfg_Data  : DB.DB_Result;
      Stat_Data : DB.DB_Result;
      Act_Count : DB.DB_Result;
      Msg_Count : DB.DB_Result;
      Item      : Config_Item;
      Cmd       : Command_Type;
      NumActs   : natural;
      NumMsgs   : natural;

   begin  -- Init

      -- Connect to the db and get all three tables we need to init, then
      -- disconnect
      DB.Connect (Handle, Host => DB_Hostname, DB => Allegra_DB);
      DB.Fetch (Handle, "*", Config_Tbl,   "", Cfg_Data);
      DB.Fetch (Handle, "*", Cmd_Auth_Tbl, "", Auth_Data);
      DB.Fetch (Handle, "*", Cmd_Stat_Tbl, "", Stat_Data);
      DB.Fetch (Handle, "count(msg)", ActQuip_Tbl, "", Act_Count);
      DB.Fetch (Handle, "count(msg)", MsgQuip_Tbl, "", Msg_Count);
      DB.Disconnect (Handle);

      -- Initialize the configuration table from the db
      for Row in 1 .. DB.Rows (Cfg_Data) loop
         declare
            Name:  string := DB.Get_Value (Cfg_Data, Row, "name");
         begin
            Item := Item_None;
            for Index in Cfg_Map'Range loop
               if Cfg_Map (Index) (1 .. Name'Length) = Name then
                  Item := Index;
                  exit;
               end if;
            end loop;
            if Item /= Item_None then
               declare
                  Value_Str:  string := DB.Get_Value (Cfg_Data, Row, "value");
               begin
                  Config_Values (Item) := US (Value_Str);
               end;
            end if;
         end;
      end loop;

      -- Initialize the command authorization level table from the db
      for Row in 1 .. DB.Rows (Auth_Data) loop
         declare
            Name:  string := DB.Get_Value (Auth_Data, Row, "name");
         begin
            Cmd := Cmd_None;
            for Index in Cmd_Names'Range loop
               if Cmd_Names (Index) (1 .. Name'Length) = Name then
                  Cmd := Index;
                  exit;
               end if;
            end loop;
            if Cmd /= Cmd_None then
               Cmd_Auth_Levels (Cmd) := DB.Get_Value (Auth_Data, Row, "level");
            end if;
         end;
      end loop;

      -- Initialize the command statistics table from the db
      for Row in 1 .. DB.Rows (Stat_Data) loop
         declare
            Name:  string := DB.Get_Value (Stat_Data, Row, "name");
         begin
            Cmd := Cmd_None;
            for Index in Valid_Commands loop
               if Cmd_Names (Index) (1 .. Name'Length) = Name then
                  Cmd := Index;
                  exit;
               end if;
            end loop;
            if Cmd /= Cmd_None then
               Cmd_Usages (Cmd) := DB.Get_Value (Stat_Data, Row, "used");
            end if;
         end;
      end loop;

      -- Determine the ratio of action quips to message quips
      if DB.Rows (Act_Count) > 0 then
         NumActs := DB.Get_Value (Act_Count, 1, "count");
      else
         NumActs := 0;
      end if;
      if DB.Rows (Msg_Count) > 0 then
         NumMsgs := DB.Get_Value (Msg_Count, 1, "count");
         Action_Message_Ratio := float (NumActs) / float (NumMsgs);
      else
         NumMsgs := 0;
         Action_Message_Ratio := 0.0;
      end if;

      -- Override certain config items from the command line.  Yes, this is
      -- awfully clunky, but it's also simple, and has been adequate for
      -- testing up to this point.
      declare
         use Ada.Command_Line;
      begin
         if Argument_Count >= 1 then
            Config_Values (Item_Host) := US (Argument (1));
         end if;
         if Argument_Count >= 2 then
            Config_Values (Item_Port) := US (Argument (2));
         end if;
         if Argument_Count >= 3 then
            Config_Values (Item_Nick) := US (Argument (3));
         end if;
         if Argument_Count >= 4 then
            Config_Values (Item_Channel) := US (Argument (4));
         end if;
         if Argument_Count >= 5 then
            Config_Values (Item_LogPath) := US (Argument (5));
         end if;
      end;

      -- Now other tasks can start getting configuration values
      Access_Control.Data_Available;
   end Init;

------------------------------------------------------------------------------
--
-- Public subroutines
--
------------------------------------------------------------------------------

   -- Return ratio of action quips to message quips
   function Act_Vs_Msg return float is
   begin  -- Act_Vs_Msg
      Access_Control.Wait_For_Data;
      return Action_Message_Ratio;
   end Act_Vs_Msg;

   ---------------------------------------------------------------------------

   -- Return a command's usage count
   function Command_Usage (Command : in Command_Type) return natural is
   begin  -- Command_Usage
      Access_Control.Wait_For_Data;
      return Cmd_Usages (Command);
   end Command_Usage;

   ---------------------------------------------------------------------------

   -- Increment a command's usage count by one
   procedure Command_Used (Command : in Command_Type) is
   begin  -- Command_Used
      Access_Control.Wait_For_Data;
      Cmd_Usages (Command) := Cmd_Usages (Command) + 1;
   end Command_Used;

   ---------------------------------------------------------------------------

   -- Return the required auth level for the given command
   function Get_Auth_Level (Command : in Command_Type) return Auth_Level is
   begin  -- Get_Auth_Level
      Access_Control.Wait_For_Data;
      return Cmd_Auth_Levels (Command);
   end Get_Auth_Level;

   ---------------------------------------------------------------------------

   -- Return the value of given configuration item
   function Get_Value (Item : in Config_Item) return string is
   begin  -- Get_Value
      Access_Control.Wait_For_Data;
      return S (Config_Values (Item));
   end Get_Value;

   ---------------------------------------------------------------------------

   -- Save any cached configuration values to the db
   procedure WrapUp is

      Handle    : DB.DB_Handle;

   begin  -- WrapUp

      -- The only cached thing we currently update is the command statistics
      -- table
      Access_Control.Wait_For_Data;
      DB.Connect (Handle, Host => DB_Hostname, DB => Allegra_DB);
      for Cmd in Valid_Commands loop
         DB.Statement (Handle, "update " & Cmd_Stat_Tbl & " set used=" & Img (Cmd_Usages (Cmd)) &
                               " where name=" & DB.Escape (RTrim (Cmd_Names (Cmd))));

      end loop;
      DB.Disconnect (Handle);
   end WrapUp;

   ---------------------------------------------------------------------------

begin  -- package Config initialization
   Init;
end Config;
