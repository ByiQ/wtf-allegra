
--
-- Log -- Log file management utility package for Allegra info-bot
--


--
-- Standard packages
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;


--
-- Local library packages
with Strings;
use  Strings;
with Times;


--
-- Application packages
with Config;


package body Log is

------------------------------------------------------------------------------
--
-- Package constants
--
------------------------------------------------------------------------------

   Name_Width : constant := 12;  -- arbitrary, "wide enough"

------------------------------------------------------------------------------
--
-- Package types
--
------------------------------------------------------------------------------

   -- The order of these enum constants determine the logging "levels".  Each
   -- level includes all preceding levels.
   type Log_Level_Enm is (Level_None, Level_Err, Level_Warn, Level_Info, Level_Dbg);

------------------------------------------------------------------------------
--
-- Package variables
--
------------------------------------------------------------------------------

   -- Currently configured log level
   Log_Level                : Log_Level_Enm := Level_None;

   -- Currently configured log file pathname
   Current_Logfile_Pathname : UString := Null_UString;

   -- Log file handle
   Log_File                 : Ada.Text_IO.File_Type;

------------------------------------------------------------------------------
--
-- Package subroutines
--
------------------------------------------------------------------------------

   -- Print a standard format log line
   procedure Log_It (From : in string;  Msg : in string) is

      use Ada.Text_IO, Times;

   begin  -- Log_It
      Put_Line (Log_File, Date_String & " " & Time_String & " " & Ada.Strings.Fixed.Head (From, Name_Width) & ":  " & Msg);
      Flush (Log_File);
   end Log_It;

------------------------------------------------------------------------------
--
-- Public subroutines
--
------------------------------------------------------------------------------

   -- Log if "err" verbosity level or above is set
   procedure Err  (From : in string;   Msg : in string) is
   begin  -- Err
      if Log_Level >= Level_Err then
         Log_It (From, Msg);
      end if;
   end Err;

   ---------------------------------------------------------------------------

   -- Log if "warn" verbosity level or above is set
   procedure Warn (From : in string;   Msg : in string) is
   begin  -- Warn
      if Log_Level >= Level_Warn then
         Log_It (From, Msg);
      end if;
   end Warn;

   ---------------------------------------------------------------------------

   -- Log if "info" verbosity level or above is set
   procedure Info (From : in string;   Msg : in string) is
   begin  -- Info
      if Log_Level >= Level_Info then
         Log_It (From, Msg);
      end if;
   end Info;

   ---------------------------------------------------------------------------

   -- Log if "dbg" verbosity level is set
   procedure Dbg  (From : in string;   Msg : in string) is
   begin  -- Dbg
      if Log_Level >= Level_Dbg then
         Log_It (From, Msg);
      end if;
   end Dbg;

   ---------------------------------------------------------------------------

   -- Get the configured log level and logfile pathname, and open the log file
   -- (for appending)
   procedure Init is
   begin  -- Init

      -- Get the configured log level string and map it into our enum type
      declare
         use Config;
         Current_Level_Str:  string := Get_Value (Item_LogLevel);
      begin
         if    Current_Level_Str = "none" then
            Log_Level := Level_None;
         elsif Current_Level_Str = "err" then
            Log_Level := Level_Err;
         elsif Current_Level_Str = "warn" then
            Log_Level := Level_Warn;
         elsif Current_Level_Str = "info" then
            Log_Level := Level_Info;
         elsif Current_Level_Str = "dbg" then
            Log_Level := Level_Dbg;
         else
            raise Program_Error;
         end if;
      end;

      -- Get the configured log file pathname string
      declare
         use Ada.Text_IO;
         Logfile_Pathname : string := Config.Get_Value (Config.Item_LogPath);
      begin

         -- See if the log file pathname has changed since last init, and if
         -- so, close the old one (if it's open) and open the new one
         if S (Current_Logfile_Pathname) /= Logfile_Pathname then
            if Is_Open (Log_File) then
               Close (Log_File);
            end if;

            -- Try opening an existing log file, for append
            begin
               Open (Log_File, Append_File, Logfile_Pathname);

            exception

               -- If the file didn't exist, try to create it; an exception
               -- here will propagate to the outer handler
               when Name_Error =>
                  Create (Log_File, Append_File, Logfile_Pathname);

               -- This exception usually means we don't have write permissions
               -- for an existing log file, so report it and disable logging
               when Use_Error =>
                  Put_Line (Standard_Error, "Cannot open log file """ & Logfile_Pathname & """ -- check permissions");
                  Log_Level := Level_None;

               -- No idea what to do about other exceptions, just report them
               -- and disable logging
               when E : others =>
                  Put_Line (Standard_Error, "Cannot open log file """ & Logfile_Pathname & """ -- " &
                            Ada.Exceptions.Exception_Name (E));
                  Log_Level := Level_None;
            end;

            -- If we make it here, things must have worked, so save the
            -- pathname
            Current_Logfile_Pathname := US (Logfile_Pathname);
         end if;

      -- These handlers are for exceptions on the Create above. They just
      -- report the error, and disable logging by setting the log level to
      -- "none"
      exception
         when Name_Error =>
            Put_Line (Standard_Error, "Cannot create log file """ & Logfile_Pathname & """ -- badly-formed pathname");
            Log_Level := Level_None;
         when Use_Error =>
            Put_Line (Standard_Error, "Cannot create log file """ & Logfile_Pathname & """ -- check permissions");
            Log_Level := Level_None;
         when E : others =>
            Put_Line (Standard_Error, "Cannot create log file """ & Logfile_Pathname & """ -- " &
                      Ada.Exceptions.Exception_Name (E));
            Log_Level := Level_None;
      end;
   end Init;

   ---------------------------------------------------------------------------

   -- Close the log file if it's open
   procedure WrapUp is
   begin  -- WrapUp
      if Ada.Text_IO.Is_Open (Log_File) then
         Ada.Text_IO.Close (Log_File);
      end if;
   end WrapUp;

   ---------------------------------------------------------------------------

begin  -- package Log initialization
   Init;
end Log;
