
--
-- IRC -- Low-level IRC communications utility package
--


--
-- Standard packages
with Ada.Strings.Unbounded;


--
-- Third-party library packages
with Sockets;  -- adasockets


--
-- Local library packages
with Strings;
use  Strings;


package body IRC is

------------------------------------------------------------------------------
--
-- Package constants
--
------------------------------------------------------------------------------

   -- We like this one a lot
   Space : constant character := Ada.Strings.Space;

------------------------------------------------------------------------------
--
-- Package variables
--
------------------------------------------------------------------------------

   -- The socket handle of our current server connection.  This package only
   -- handles one server connection at a time, but that's sufficient for most
   -- needs.
   Handle : Sockets.Socket_FD;

------------------------------------------------------------------------------
--
-- Package variables
--
------------------------------------------------------------------------------

   -- Connect to given IRC server on given port
   procedure Open_Server (Name : in string;
                          Port : in positive) is
   begin  -- Open_Server

      -- Create a socket to use for the IRC server connection, then connect it
      Sockets.Socket (Sockets.Socket_FD (Handle));
      Sockets.Connect (Sockets.Socket_FD (Handle), Name, Port);

   exception
      -- Map adasockets exceptions into our local generic exception
      when others =>
         raise Connect_Error;
   end Open_Server;

   ---------------------------------------------------------------------------

   -- Close server connection
   procedure Close_Server is
   begin  -- Close_Server
      Sockets.Shutdown (Sockets.Socket_FD (Handle));

   exception
      -- Map adasockets exceptions into our local generic exception
      when others =>
         raise Connect_Error;
   end Close_Server;

   ---------------------------------------------------------------------------

   -- Read a message from the server and parse into its components; blocks
   -- until one is available
   procedure Read (Message : out Message_Rec) is

      Scan       : positive;
      Start      : positive;
      Len        : natural;

   begin  -- Read

      -- This inner block is necessary so we can catch socket exceptions that
      -- may occur when we read the line from the server, and map them into
      -- our package's exception.  Doing the Get_Line in the declarations
      -- (like we used to) means that this procedure block's context hasn't
      -- been established yet, thus letting the adasockets exception get
      -- propagated to the caller.
      declare
         Input_Line : string := Sockets.Get_Line (Handle);
      begin

         -- Now parse the message into its components: prefix, command, and
         -- parameters
         Scan := Input_Line'First;
         Len  := Input_Line'Last;

         -- First, find the prefix if it's there
         Message.Prefix := Null_Field;
         if Scan <= Len and then Input_Line (Scan) = ':' then
            Scan := Scan + 1;
            Start := Scan;
            while Scan <= Len and then Input_Line (Scan) /= Space loop
               Scan := Scan + 1;
            end loop;
            if Scan > Start then
               Message.Prefix := US (Input_Line (Start .. Scan - 1));
            end if;
         end if;

         -- Next, find the command
         Message.Command := Null_Field;
         if Scan <= Len then
            if Input_Line (Scan) = Space then
               Scan := Scan + 1;
            end if;
            Start := Scan;
            while Scan <= Len and then Input_Line (Scan) /= Space loop
               Scan := Scan + 1;
            end loop;
            if Scan > Start then
               Message.Command := US (Input_Line (Start .. Scan - 1));
            end if;
         end if;

         -- Finally, find the parameter string
         Message.Params := Null_Field;
         if Scan <= Len then
            if Input_Line (Scan) = Space then
               Scan := Scan + 1;
            end if;
            if Scan <= Len then
               Message.Params := US (Input_Line (Scan .. Len));
            end if;
         end if;
      end;

   exception
      -- Map adasockets exceptions, and any other exceptions we might
      -- encounter during parsing, into our local generic exception
      when others =>
         raise Connect_Error;
   end Read;

   ---------------------------------------------------------------------------

   -- Write a message to the server
   procedure Write (Message : in Message_Rec) is
   begin  -- Write

      -- Assemble and write the actual message string from the components of
      -- the given message record; specifically, the prefix may be null
      if not Equal (Message.Prefix, Null_Field) then
         Sockets.Put_Line (Handle, ":" & S (Message.Prefix) & Space &
                           S (Message.Command) & Space &
                           S (Message.Params));
      else
         Sockets.Put_Line (Handle, S (Message.Command) & Space &
                           S (Message.Params));
      end if;

   exception
      -- Map adasockets exceptions, and any other exceptions we might
      -- encounter during message assembly, into our local generic exception
      when others =>
         raise Connect_Error;
   end Write;

   ---------------------------------------------------------------------------

   -- Parse a server message parameter string into separate fields
   procedure Parse_Params (Param_Str : in  UString;
                           Params    : out Param_Arr;
                           Count     : out Param_Count) is

      use Ada.Strings.Unbounded;

      Scan:   positive;
      Start:  positive;
      Len:    natural;

   begin  -- Parse_Params

      -- Initialize scan variables
      Count := 0;
      Scan := 1;
      Len := Length (Param_Str);

      -- Scan through the string until we hit the end
      loop

         -- Skip leading blanks
         while Scan <= Len and then Element (Param_Str, Scan) = Space loop
            Scan := Scan + 1;
         end loop;

         -- Done parsing if we've hit the end of the string
         exit when Scan > Len;

         -- Remember the starting column number
         Start := Scan;

         -- A ":" means that all the rest of the string is one parameter, so
         -- adjust scan variables to reflect that
         if Element (Param_Str, Start) = ':' then
            Start := Start + 1;
            Scan := Len + 1;

         -- Guard against illegal messages with too many params
         elsif Count >= Max_Params - 1 then
            Scan := Len + 1;

         -- Regular (non-":") parameter ends at the next space
         else
            while Scan <= Len and then Element (Param_Str, Scan) /= Space loop
               Scan := Scan + 1;
            end loop;
         end if;

         -- If the parameter is within bounds, add it to the result array
         if Start <= Len then
            Count := Count + 1;
            Params (Count) := Trim (US (Slice (Param_Str, Start, Scan - 1)), Side => Ada.Strings.Both);
         end if;
      end loop;
   end Parse_Params;

   ---------------------------------------------------------------------------

   -- Parse an IRC "msgto" string into nick/user/host fields
   procedure Parse_MsgTo (Str   : in  UString;
                          MsgTo : out MsgTo_Rec) is

      use Ada.Strings.Unbounded;

      Scan:   positive;
      Start:  positive;
      Len:    natural;

   begin  -- Parse_MsgTo

      -- Initialize scan variables and set results to null
      Len := Length (Str);
      MsgTo.Nick := Null_Field;
      MsgTo.User := Null_Field;
      MsgTo.Host := Null_Field;
      Scan := 1;
      Start := Scan;

      -- Nick ends with "!", or at end of string
      while Scan <= Len and then Element (Str, Scan) /= '!' loop
         Scan := Scan + 1;
      end loop;

      -- If we got a nick, return it and advance past it
      if Scan <= Len and then Element (Str, Scan) = '!' then
         MsgTo.Nick := Trim (US (Slice (Str, Start, Scan - 1)), Side => Ada.Strings.Both);
         Scan := Scan + 1;
      else
         Scan := 1;
      end if;

      -- If there's more to the string, look for user@host portion
      Start := Scan;
      if Start <= Len then

         -- Username ends at "@", or at end of string
         while Scan <= Len and then Element (Str, Scan) /= '@' loop
            Scan := Scan + 1;
         end loop;

         -- If we got a username, return it and advance past it
         if Scan <= Len and then Element (Str, Scan) = '@' then
            MsgTo.User := Trim (US (Slice (Str, Start, Scan - 1)), Side => Ada.Strings.Both);
            Scan := Scan + 1;
         else

            -- If we hit the end of the string without finding "@", then this
            -- is a nick, unless we already saw a "nick!", in which case this
            -- is a username with no host
            if MsgTo.Nick /= Null_Field then
               MsgTo.User := Trim (US (Slice (Str, Start, Len)), Side => Ada.Strings.Both);
            else
               MsgTo.Nick := Trim (Str, Side => Ada.Strings.Both);
            end if;
         end if;
      end if;

      -- If there's still more to the string, look for host portion
      Start := Scan;
      if Start <= Len then
         MsgTo.Host := Trim (US (Slice (Str, Start, Len)), Side => Ada.Strings.Both);
      end if;

      -- Not sure why this happens, but it did during development, so we
      -- convert null strings to, um, null strings (?!).  This may have been a
      -- compiler/runtime bug, or may be historical cruft that can be removed.
      if MsgTo.Nick /= Null_Field and then Length (MsgTo.Nick) = 0 then
         MsgTo.Nick := Null_Field;
      end if;
      if MsgTo.User /= Null_Field and then Length (MsgTo.User) = 0 then
         MsgTo.User := Null_Field;
      end if;
      if MsgTo.Host /= Null_Field and then Length (MsgTo.Host) = 0 then
         MsgTo.Host := Null_Field;
      end if;
   end Parse_MsgTo;

   ---------------------------------------------------------------------------

end IRC;
