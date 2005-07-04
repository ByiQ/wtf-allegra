with
  Ada.Calendar,
  Ada.Exceptions,
  Ada.Integer_Text_IO,
  Ada.Strings.Fixed,
  Ada.Strings.Unbounded,
  Ada.Text_IO,
  Config;

package body Log is

   -- Declarations
   Name_Width:  constant := 12;  -- arbitrary, "wide enough"

   type Log_Level_Enm is ( Level_None, Level_Err, Level_Warn, Level_Info, Level_Dbg );

   Log_Level:                 Log_Level_Enm := Level_None;
   Log_File:                  Ada.Text_IO.File_Type;
   Current_Logfile_Pathname:  Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;

   -- Local routines
   Month_Names:  constant array (Ada.Calendar.Month_Number) of string (1 .. 3) :=
     ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec");

   function Timestamp return string is

      use Ada.Calendar;

      function Str (Num:    in natural;
                    Width:  in positive  := 10;
                    Pad:    in character := ' ') return string is

         S:  string ( 1 .. Width );

      begin  -- Str
         Ada.Integer_Text_IO.Put (S, Num);
         if Pad /= ' ' then  -- Put pads with space by default
            for Char in S'Range loop
               if S (Char) = ' ' then
                  S (Char) := Pad;
               end if;
            end loop;
         end if;
         return S;
      end Str;

      Now:   Time := Clock;  -- current system time
      HH:    natural;
      MM:    natural;
      SS:    natural;

   begin  -- Timestamp
      SS := natural (Seconds (Now));
      HH := SS / 3600;
      SS := SS - HH * 3600;
      MM := SS / 60;
      SS := SS - MM * 60;

      return Str (Day (Now), 2) & " " & Month_Names (Month (Now)) & " " & Str (Year (Now), 4)
        & " " &
        Str (HH, 2, Pad => ' ') & ":" & Str (MM, 2, Pad => '0') & ":" & Str (SS, 2, Pad => '0');
   end Timestamp;

   -- Exported procedures
   procedure Err  (From:  string;   Msg:  string) is
   begin  -- Err
      if Log_Level >= Level_Err then
         Ada.Text_IO.Put_Line (Log_File, Timestamp & " " & Ada.Strings.Fixed.Head (From, Name_Width) & ":  " & Msg);
         Ada.Text_IO.Flush (Log_File);
      end if;
   end Err;

   procedure Warn (From:  string;   Msg:  string) is
   begin  -- Warn
      if Log_Level >= Level_Warn then
         Ada.Text_IO.Put_Line (Log_File, Timestamp & " " & Ada.Strings.Fixed.Head (From, Name_Width) & ":  " & Msg);
         Ada.Text_IO.Flush (Log_File);
      end if;
   end Warn;

   procedure Info (From:  string;   Msg:  string) is
   begin  -- Info
      if Log_Level >= Level_Info then
         Ada.Text_IO.Put_Line (Log_File, Timestamp & " " & Ada.Strings.Fixed.Head (From, Name_Width) & ":  " & Msg);
         Ada.Text_IO.Flush (Log_File);
      end if;
   end Info;

   procedure Dbg  (From:  string;   Msg:  string) is
   begin  -- Dbg
      if Log_Level >= Level_Dbg then
         Ada.Text_IO.Put_Line (Log_File, Timestamp & " " & Ada.Strings.Fixed.Head (From, Name_Width) & ":  " & Msg);
         Ada.Text_IO.Flush (Log_File);
      end if;
   end Dbg;

   procedure Init is
   begin  -- Init
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

      declare
         use Ada.Strings.Unbounded, Ada.Text_IO, Config;
         Logfile_Pathname:  string := Get_Value (Item_LogPath);
      begin
         if Current_Logfile_Pathname /= Logfile_Pathname then
            if Is_Open (Log_File) then
               Close (Log_File);
            end if;
            begin
               Open (Log_File, Append_File, Logfile_Pathname);
            exception
               when Name_Error =>
                  Create (Log_File, Append_File, Logfile_Pathname);
               when Use_Error =>
                  Put_Line (Standard_Error, "Cannot open log file """ & Logfile_Pathname & """ -- check permissions");
                  Log_Level := Level_None;
               when others =>
                  raise;  -- propagate to outer handler
            end;
            Current_Logfile_Pathname := To_Unbounded_String (Logfile_Pathname);
         end if;

      exception
         when Name_Error =>
            Put_Line (Standard_Error, "Cannot create log file """ & Logfile_Pathname & """ -- badly-formed pathname");
            Log_Level := Level_None;
         when Use_Error =>
            Put_Line (Standard_Error, "Cannot create log file """ & Logfile_Pathname & """ -- check permissions");
            Log_Level := Level_None;
         when E: others =>
            Put_Line (Standard_Error, "Cannot create log file """ & Logfile_Pathname & """ -- " &
                      Ada.Exceptions.Exception_Name (E));
            Log_Level := Level_None;
      end;
   end Init;

   procedure WrapUp is
   begin  -- WrapUp
      if Ada.Text_IO.Is_Open (Log_File) then
         Ada.Text_IO.Close (Log_File);
      end if;
   end WrapUp;

begin  -- package Log initialization
   Init;
end Log;
