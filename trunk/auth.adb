
--
-- Auth -- Command authorization utility package for Allegra info-bot
--


--
-- Standard packages
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;


--
-- Compiler-specific packages
with GNAT.Regexp;


--
-- Local library packages
with Strings;
use  Strings;


--
-- Application packages
with Config;
with DB;
with IRC;


package body Auth is

------------------------------------------------------------------------------
--
-- Package constants
--
------------------------------------------------------------------------------

   -- The auth level all new users get automatically
   Default_Auth_Level : constant := 1;

------------------------------------------------------------------------------
--
-- Package types
--
------------------------------------------------------------------------------

   -- We keep a linked list of configured users and their auth levels, read by
   -- Init from the database.  The "ID" field is an IRC usermask pattern.
   type User_Rec;
   type User_Ptr is access User_Rec;
   type User_Rec is record
      ID:     UString;
      Level:  Config.Auth_Level;
      Next:   User_Ptr;
   end record;

------------------------------------------------------------------------------
--
-- Package variables
--
------------------------------------------------------------------------------

   -- The current user-auth list
   User_Head : User_Ptr := null;

------------------------------------------------------------------------------
--
-- Package subroutines
--
------------------------------------------------------------------------------

   -- Lets us free the old user list when we re-init
   procedure Free is new Ada.Unchecked_Deallocation (User_Rec, User_Ptr);

------------------------------------------------------------------------------
--
-- Public subroutines
--
------------------------------------------------------------------------------

   -- Returns the current command authorization level for a user, by matching
   -- the given user ID against the list of usermasks.
   function Level (Who : in UString) return Config.Auth_Level is

      User : User_Ptr;

   begin  -- Level

      -- Search through the user auth list for a match on the given usermask
      User := User_Head;
      while User /= null loop
         exit when Match (Who, User.ID) = Succeeded;
         User := User.Next;
      end loop;

      -- If match, return that user's auth level, otherwise return default
      if User = null then
         return Default_Auth_Level;
      else
         return User.Level;
      end if;
   end Level;

   ---------------------------------------------------------------------------

   -- Match a user ID against a usermask.  Returns Succeeded if it matches, or
   -- one of the Fail* values to indicate which part was the first to fail.
   function Match (Who  : in UString;
                   Mask : in UString) return Authorization is

      use GNAT.Regexp;

      Check  : IRC.MsgTo_Rec;
      UserID : IRC.MsgTo_Rec;

   begin  -- Match

      -- Split each value into its component parts
      IRC.Parse_MsgTo (Who,  UserID);
      IRC.Parse_MsgTo (Mask, Check);

      -- Use the glob-style matching provided by GNAT.Regexp to match each
      -- component in turn
      if not Match (S (UserID.Nick), Compile (S (Check.Nick), Glob => true, Case_Sensitive => false)) then
         return Fail_Nick;
      end if;
      if not Match (S (UserID.User), Compile (S (Check.User), Glob => true, Case_Sensitive => false)) then
         return Fail_User;
      end if;
      if not Match (S (UserID.Host), Compile (S (Check.Host), Glob => true, Case_Sensitive => false)) then
         return Fail_Host;
      end if;

      -- If we haven't failed, and thus returned, so far, then all parts match
      return Succeeded;
   end Match;

   ---------------------------------------------------------------------------

   -- Returns Succeeded if given user is permitted to do given command.
   -- Returns a generic "FailNick" if not.
   function Permitted (Who : in UString;
                       Cmd : in Config.Command_Type) return Authorization is
   begin  -- Permitted
      if Level (Who) >= Config.Get_Auth_Level (Cmd) then
         return Succeeded;
      else
         return Fail_Nick;
      end if;
   end Permitted;

   ---------------------------------------------------------------------------

   -- Initialize the cached user auth list by (re)reading it from the database
   procedure Init is

      Handle    : DB.DB_Handle;
      Auth_Data : DB.DB_Result;
      Next      : User_Ptr;
      Tail      : User_Ptr;

   begin  -- Init

      -- If there's an old list, this isn't our first call, so throw it away
      Next := User_Head;
      while Next /= null loop
         Tail := Next.Next;
         Free (Next);
         Next := Tail;
      end loop;
      User_Head := null;

      -- Connect to the database, fetch the user auth level table, then disconnect
      DB.Connect (Handle, Host => Config.DB_Hostname, DB => Config.Allegra_DB);
      DB.Fetch (Handle, "*", Config.UserLvl_Tbl, "", Auth_Data);
      DB.Disconnect (Handle);

      -- Copy the user auth data into our linked list
      for Row in 1 .. DB.Rows (Auth_Data) loop
         declare
            UName    : string := DB.Get_Value (Auth_Data, Row, "name");
            Name     : UString := US (UName);
            Level    : Config.Auth_Level := DB.Get_Value (Auth_Data, Row, "level");
            New_User : User_Ptr := new User_Rec'(ID => Name, Level => Level, Next => null);
         begin
            if User_Head = null then
               User_Head := New_User;
            else
               Tail.Next := New_User;
            end if;
            Tail := New_User;
         end;
      end loop;
   end Init;

   ---------------------------------------------------------------------------

begin  -- package Auth initialization
   Init;
end Auth;
