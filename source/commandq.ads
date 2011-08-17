
--
-- CommandQ -- Command-processing task request queue for Allegra info-bot
--


--
-- Local library packages
with PQueue;
with Strings;


--
-- Application packages
with IRC;


package CommandQ is

------------------------------------------------------------------------------
--
-- Public types
--
------------------------------------------------------------------------------

   -- The command operations this task knows about
   type Operation_Type is
     (
      Login_Operation,
      Reply_Operation,
      Save_Operation,
      Message_Operation,
      Notice_Operation,
      Ping_Operation,
      Crash_Operation
     );

   -- The command request type
   type Request_Rec is record
      Operation : Operation_Type;
      Origin    : Strings.UString;
      Target    : Strings.UString;
      Data      : Strings.UString;
      Reply     : IRC.Server_Reply;
   end record;

------------------------------------------------------------------------------
--
-- Request queue
--
------------------------------------------------------------------------------

   -- Instantiate the protected-queue package with our request type, and
   -- create an instance of it to serve as this task's main input.
   package Command_Queue_Pkg is new PQueue (Request_Rec);
   Requests : Command_Queue_Pkg.Protected_Queue_Type;

------------------------------------------------------------------------------
--
-- Public subroutines
--
------------------------------------------------------------------------------

   -- Cause the bot to do an emergency shutdown, by telling the command task
   -- to crash it
   procedure Crash (Who : in string);

   ---------------------------------------------------------------------------

end CommandQ;
