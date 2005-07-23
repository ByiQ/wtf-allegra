
--
-- Output -- IRC output task package for Allegra info-bot
--


--
-- Standard packages
with Ada.Exceptions;


--
-- Local library packages
with Strings;
use  Strings;


--
-- Application packages
with Command;
with IRC;
with Log;


package body Output is

------------------------------------------------------------------------------
--
-- Package constants
--
------------------------------------------------------------------------------

   -- This task's name, for logging purposes
   Output_Name : constant string := "Output";

------------------------------------------------------------------------------
--
-- Package subroutines
--
------------------------------------------------------------------------------

   -- Tell the command that we've had an exception, and the bot should crash

------------------------------------------------------------------------------
--
-- Public subroutines
--
------------------------------------------------------------------------------

   -- Simple interface to allow other tasks to write a message to the user
   procedure Say (Msg : in UString;  To : in UString) is

      Req : Request_Rec;

   begin  -- Say
      Req.Operation   := Message_Operation;
      Req.Destination := To;
      Req.Data        := Msg;
      Requests.Enqueue (Req);
   end Say;

   ---------------------------------------------------------------------------

   -- Simple interface to allow other tasks to write a message to the user
   procedure Say (Msg : in string;  To : in UString) is

      Req : Request_Rec;

   begin  -- Say
      Req.Operation   := Message_Operation;
      Req.Destination := To;
      Req.Data        := US (Msg);
      Requests.Enqueue (Req);
   end Say;

   ---------------------------------------------------------------------------

   -- Simple interface to allow other tasks to write a message to the user
   procedure Say (Msg : in UString;  To : in string) is

      Req : Request_Rec;

   begin  -- Say
      Req.Operation   := Message_Operation;
      Req.Destination := US (To);
      Req.Data        := Msg;
      Requests.Enqueue (Req);
   end Say;

   ---------------------------------------------------------------------------

   -- Simple interface to allow other tasks to write a message to the user
   procedure Say (Msg : in string;  To : in string) is

      Req : Request_Rec;

   begin  -- Say
      Req.Operation   := Message_Operation;
      Req.Destination := US (To);
      Req.Data        := US (Msg);
      Requests.Enqueue (Req);
   end Say;

------------------------------------------------------------------------------
--
-- Public task
--
------------------------------------------------------------------------------

   task body Output_Task is

      use Log;

      -- The current request being processed
      Request : Request_Rec;

   begin  -- Output_Task

      -- Main task request loop.  Note that exceptions are handled inside the
      -- main loop instead of breaking out of it as with most other tasks,
      -- since IRC connect errors aren't fatal; we just report them and wait
      -- to get reconnected by the other tasks.  Other exceptions will,
      -- however, lead to termination.
      loop

         -- Fetch next request from our request queue and handle it
         Requests.Dequeue (Request);
         begin
            case Request.Operation is
               when Shutdown_Operation =>
                  Dbg (Output_Name, "Writing QUIT """ & S (Request.Data) & """");
                  IRC.Write (IRC.Message_Rec'(IRC.Null_Field, US ("QUIT"), ":" & Request.Data));
                  exit;  -- exit the main loop, thus shutting down the task

               when Nick_Operation =>
                  Dbg (Output_Name, "Writing NICK " & S (Request.Data));
                  IRC.Write (IRC.Message_Rec'(IRC.Null_Field, US ("NICK"), Request.Data));

               when User_Operation =>
                  Dbg (Output_Name, "Writing USER " & S (Request.Data));
                  IRC.Write (IRC.Message_Rec'(IRC.Null_Field, US ("USER"), Request.Data));

               when Join_Operation =>
                  Dbg (Output_Name, "Writing JOIN " & S (Request.Data));
                  IRC.Write (IRC.Message_Rec'(IRC.Null_Field, US ("JOIN"), Request.Data));

               when Message_Operation =>
                  Dbg (Output_Name, "Writing PRIVMSG to " & S (Request.Destination) & ": " &
                       S (Request.Data));
                  IRC.Write (IRC.Message_Rec'(IRC.Null_Field, US ("PRIVMSG"),
                                              Request.Destination & " :" & Request.Data));

               when Ping_Operation =>
                  Dbg (Output_Name, "Writing PING: " & S (Request.Data));
                  IRC.Write (IRC.Message_Rec'(IRC.Null_Field, US ("PING"), Request.Data));

               when Pong_Operation =>
                  Dbg (Output_Name, "Writing PONG: " & S (Request.Data));
                  IRC.Write (IRC.Message_Rec'(IRC.Null_Field, US ("PONG"), Request.Data));
            end case;

         exception
            when IRC.Connect_Error =>
               Err (Output_Name, "Connect error during write in " & Output_Name);

            when E : others =>
               Err (Output_Name, "Exception " & Ada.Exceptions.Exception_Information (E) &
                    " during write in " & Output_Name);
               Command.Crash (Output_Name);
         end;
      end loop;
   end Output_Task;

   ---------------------------------------------------------------------------

end Output;
