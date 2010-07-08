
-- Net -- Non-IRC network communication task package for Allegra info-bot


-- Standard packages
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;

-- Compiler-specific packages
with GNAT.Sockets;

-- Local library packages
with Strings;

-- Application packages
with Config;
with CommandQ;
with Identity;
with Log;
with OutputQ;

-- Request-queue package
with NetQ;


package body Net is

------------------------------------------------------------------------------
--
-- Package constants
--
------------------------------------------------------------------------------

   -- This task's name, for logging purposes
   Net_Name       : constant string := "Net";

   -- Rename a couple of our faves
   Newline        : constant Character := Ada.Characters.Latin_1.LF;
   CRLF           : constant String := Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;

   -- Parts of HTTP GET to fetch a shortened URL
   Shorten_GET_1  : constant String := "POST /s?Long_URL=";
   Shorten_GET_2  : constant String := " HTTP/1.1" & CRLF & "Host: ";
   Shorten_GET_3  : constant String := "User-Agent: ";

   -- The Content-Length header line's prefix
   Content_Length : constant String := "Content-Length:";

   -- What port the shortener server listens on
   Shortener_Port : constant := 80;

------------------------------------------------------------------------------
--
-- Package variables
--
------------------------------------------------------------------------------

   URL_Shortener_Address : Strings.UString;

------------------------------------------------------------------------------
--
-- Package subroutines
--
------------------------------------------------------------------------------

   -- Init the net task
   procedure Init is

      use Ada.Text_IO;
      use Strings;

   begin  -- Init

      URL_Shortener_Address := US (Config.Get_Value (Config.Item_Shorten_URL));

   exception
      -- Putting the message into the log file seems to obscure it too much
      when E : others =>
         Put_Line (Standard_Error, "Net init exception:  " & Ada.Exceptions.Exception_Information (E));
   end Init;

   ---------------------------------------------------------------------------

   -- Use Jerrid's ada.cx URL shortener
   procedure Shorten_URL (Req : in NetQ.Request_Rec) is

      ------------------------------------------------------------------------

      use Ada.Streams;
      use GNAT.Sockets;
      use Strings;

      ------------------------------------------------------------------------

      Shortener : String := S (URL_Shortener_Address);

      Handle    : Socket_Type;
      Address   : Sock_Addr_Type;
      Server    : Stream_Access;
      Short_Len : Natural;

      ------------------------------------------------------------------------

   begin  -- Shorten_URL

      -- Create a socket to use for the HTTP connection, then connect it.  If
      -- no connect, say so.
      begin
         Create_Socket (Handle);
         Set_Socket_Option (Handle, Socket_Level, (Reuse_Address, True));
         Address.Addr := Addresses (Get_Host_By_Name (Shortener));
         Address.Port := Port_Type (Shortener_Port);
         Connect_Socket (Handle, Address);
         Server := Stream (Handle);
      exception
         when E : others =>
            Log.Err (Net_Name, "URL shortener connect error:  " & Ada.Exceptions.Exception_Information (E));
            OutputQ.Say ("Sorry, I couldn't talk to the URL shortener at " & Shortener, Req.Destination);
            return;
      end;

      -- Assemble and send the request
      declare
         Get : String := Shorten_GET_1 & S (Req.Data) & Shorten_GET_2 & Shortener & CRLF &
                         Shorten_GET_3 & Identity.App_Name & " InfoBot v" & Identity.App_Version & CRLF &
                         CRLF;
      begin
         String'Write (Server, Get);
         Log.Info (Net_Name, "Shortening URL """ & S (Req.Data) & """");
      end;

      -- Get the answer's header a character at a time
      declare

         -- Maximum length of an HTTP header line (at least, that we care about)
         Line_Max : constant := 512;

         Hdr   : String (1 .. Line_Max);
         Index : Natural := Hdr'First;

      begin

         -- Read header lines until we see the Content-Length line
         loop

            Character'Read (Server, Hdr (Index));

            -- Check each header line to see if it's the Content-Length line
            if Hdr (Index) = Newline then
               if Hdr (Hdr'First .. Content_Length'Last) = Content_Length then

                  -- It's the length, so extract the number
                  while Index >= Hdr'First and then Hdr (Index) not in '0' .. '9' loop
                     Index := Index - 1;
                  end loop;
                  Short_Len := Natural'Value (Hdr (Content_Length'Last + 1 .. Index));

                  exit;
               else
                  -- Not the right header line, throw it away
                  Index := Hdr'First;
               end if;
            else
               Index := Index + 1;
            end if;

         end loop;

         -- Discard lines until end of header (which is a blank line)
         Index := Hdr'First;
         loop

            Character'Read (Server, Hdr (Index));

            -- Check each header line to see if it's the Content-Length line
            if Hdr (Index) = Newline then
               if Index = Hdr'First or else Hdr (Hdr'First .. CRLF'Last) = CRLF then
                  -- Blank line means end of header, we're done
                  exit;
               else
                  -- Just another header line, discard it and keep going
                  Index := Hdr'First;
               end if;
            else
               Index := Index + 1;
            end if;

         end loop;

      end;

      -- Now we know the exact length of the real answer, so just fetch it
      declare
         Answer : String (1 .. Short_Len);
      begin
         String'Read (Server, Answer);
         Log.Info (Net_Name, "Got short URL """ & Answer & """");
         OutputQ.Say (Answer, Req.Destination);
      end;

      -- Clean up the server connection, which is probably closed already
      Finalize;

   exception
      when E : others =>
         Log.Err (Net_Name, "URL shortener exception:  " & Ada.Exceptions.Exception_Information (E));
         CommandQ.Crash (Net_Name);
   end Shorten_URL;

------------------------------------------------------------------------------
--
-- Public task
--
------------------------------------------------------------------------------

   task body Net_Task is

      use NetQ;

      Request : Request_Rec;

   begin  -- Net_Task

      -- Main task request loop
      loop

         -- Fetch next request from our request queue and handle it
         Requests.Dequeue (Request);
         case Request.Operation is
            when Shutdown_Operation =>
               exit;  -- exit the main loop, thus shutting down the task

            when Shorten_URL_Operation =>
               Shorten_URL (Request);
         end case;
      end loop;

   exception
      when E : others =>
         Log.Err (Net_Name, "Exception:  " & Ada.Exceptions.Exception_Information (E));
         CommandQ.Crash (Net_Name);
   end Net_Task;

   ---------------------------------------------------------------------------

begin  -- package Net initialization
   Init;
end Net;
