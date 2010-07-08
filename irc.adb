
--
-- IRC -- Low-level IRC communications utility package
--


-- Standard packages
with Ada.Characters.Latin_1;
with Ada.Streams;
with Ada.Strings.Unbounded;


-- GNAT-specific library packages
with GNAT.Sockets;


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

   -- Far beyond the max line length an IRC server should ever send us (the
   -- RFC says 512).
   Max_Line_Length : constant := 1024;

------------------------------------------------------------------------------
--
-- Package variables
--
------------------------------------------------------------------------------

   -- The socket handle of our current server connection.  This package only
   -- handles one server connection at a time, but that's sufficient for most
   -- needs.
   Handle : GNAT.Sockets.Socket_Type;

------------------------------------------------------------------------------
--
-- Package routines
--
------------------------------------------------------------------------------

   -- Read a line (string of characters terminated by a newline, hex 0A) from
   -- a socket, ignoring CRs (hex 0D) while we're at it.  Adapted from Sam
   -- Tardieu's adasockets code.
   procedure Get_Line (Socket : in  GNAT.Sockets.Socket_Type;
                       Line   : out String;
                       Last   : out Natural) is

      Index   : Positive := Line'First;
      Char    : Character;
      Channel : constant GNAT.Sockets.Stream_Access := GNAT.Sockets.Stream (Socket);

   begin  -- Get_Line
      loop
         Char := Character'Input (Channel);
         if Char = Ada.Characters.Latin_1.LF then
            Last := Index - 1;
            return;
         elsif Char /= Ada.Characters.Latin_1.CR then
            Line (Index) := Char;
            Index := Index + 1;
            if Index > Line'Last then
               Last := Line'Last;
               return;
            end if;
         end if;
      end loop;
   end Get_Line;

   ---------------------------------------------------------------------------

   -- Write a string to a socket.  Unlike adasockets' Put_Line, does not add a
   -- CRLF.
   procedure Put_String (Socket : in GNAT.Sockets.Socket_Type;
                         Str    : in String) is

      Channel : constant GNAT.Sockets.Stream_Access := GNAT.Sockets.Stream (Socket);

   begin  -- Put_String
      for C in Str'Range loop
         Character'Output (Channel, Str (C));
      end loop;
   end Put_String;

------------------------------------------------------------------------------
--
-- Exported routines
--
------------------------------------------------------------------------------

   -- Connect to given IRC server on given port
   procedure Open_Server (Name : in String;
                          Port : in Positive) is

      use GNAT.Sockets;

      Address : Sock_Addr_Type;

   begin  -- Open_Server

      -- Create a socket to use for the IRC server connection, then connect it
      Create_Socket (Handle);
      Set_Socket_Option (Handle, Socket_Level, (Reuse_Address, True));
      Address.Addr := Addresses (Get_Host_By_Name (Name));
      Address.Port := Port_Type (Port);
      Connect_Socket (Handle, Address);

   exception
      -- Map socket exceptions into our local generic exception
      when others =>
         raise Connect_Error;
   end Open_Server;

   ---------------------------------------------------------------------------

   -- Close server connection
   procedure Close_Server is
   begin  -- Close_Server
      GNAT.Sockets.Finalize;

   exception
      -- Map socket exceptions into our local generic exception
      when others =>
         raise Connect_Error;
   end Close_Server;

   ---------------------------------------------------------------------------

   -- Read a message from the server and parse into its components; blocks
   -- until one is available
   procedure Read (Message : out Message_Rec) is

      Input_Line : String (1 .. Max_Line_Length);
      Scan       : positive;
      Start      : positive;
      Len        : natural;

   begin  -- Read

      -- Fetch the line from the server
      Get_Line (Handle, Input_Line, Len);

      -- Now parse the message into its components: prefix, command, and
      -- parameters
      Scan := Input_Line'First;

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

   exception
      -- Map socket exceptions, and any other exceptions we might encounter
      -- during parsing, into our local generic exception
      when others =>
         raise Connect_Error;
   end Read;

   ---------------------------------------------------------------------------

   -- Write a message to the server
   procedure Write (Message : in Message_Rec) is

      CRLF : constant String := Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;

   begin  -- Write

      -- Assemble and write the actual message string from the components of
      -- the given message record; specifically, the prefix may be null
      if not Equal (Message.Prefix, Null_Field) then
         Put_String (Handle, ":" & S (Message.Prefix) & Space &
                                   S (Message.Command) & Space &
                                   S (Message.Params) & CRLF);
      else
         Put_String (Handle, S (Message.Command) & Space &
                             S (Message.Params) & CRLF);
      end if;

   exception
      -- Map socket exceptions, and any other exceptions we might encounter
      -- during message assembly, into our local generic exception
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
