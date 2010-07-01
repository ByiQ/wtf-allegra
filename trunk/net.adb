
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

   -- Maximum length of a shortened URL
   Line_Max       : constant := 512;

   -- Rename one of our faves
   Newline        : constant Character := Ada.Characters.Latin_1.LF;

   -- First part of HTTP GET to fetch a shortened URL
   Shorten_GET_1  : constant String := "POST /s?Long_URL=";
   Shorten_GET_2  : constant String := " HTTP/1.1" & Newline & "Host: ";
   Shorten_GET_3  : constant String := "User-Agent: ";

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

      Handle  : Socket_Type;
      Address : Sock_Addr_Type;
      Answer  : Stream_Element_Array (1 .. Line_Max);
      Last    : Stream_Element_Offset;

      ------------------------------------------------------------------------

   begin  -- Shorten_URL

      -- Create a socket to use for the HTTP connection, then connect it
      Create_Socket (Handle);
      Set_Socket_Option (Handle, Socket_Level, (Reuse_Address, True));
      Address.Addr := Addresses (Get_Host_By_Name (S (URL_Shortener_Address)));
      Address.Port := Port_Type (Shortener_Port);
      Connect_Socket (Handle, Address);

      -- Assemble and send the request
      declare
         Get : String := Shorten_GET_1 & S (Req.Data) & Shorten_GET_2 & S (URL_Shortener_Address) & Newline &
                         Shorten_GET_3 & Identity.App_Name & " v" & Identity.App_Version & Newline &
                         Newline;
      begin
         String'Write (Stream (Handle), Get);
         Log.Info (Net_Name, "Sent HTTP GET request: " & Get);
      end;

      -- Get the answer
      Receive_Socket (Handle, Answer, Last, Wait_For_A_Full_Reception);

      -- Convert the answer to a string, and spit it out into the channel
      declare
         Response : String (1 .. Positive (Last));
         Index    : Positive := Response'First;
      begin
         for C in Answer'First .. Last loop
            if Character'Val (Answer (C)) = Newline then
               -- Discard all but the last line, and discard its newline if present
               if C < Last then
                  Index := Response'First;
               end if;
            else
               Response (Index) := Character'Val (Answer (C));
               Index := Index + 1;
            end if;
         end loop;

         OutputQ.Say (Response (Response'First .. Index - 1), Req.Destination);
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
