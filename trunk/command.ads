
--
-- Command -- Command-processing task package for Allegra info-bot
--


--
-- Standard packages
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;


--
-- Local library packages
with PQueue;


--
-- Application packages
with IRC;


package Command is

------------------------------------------------------------------------------
--
-- Public types
--
------------------------------------------------------------------------------

   -- The command operations this package knows about
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

   -- The command request type, and the command queue, which is how other
   -- tasks communicate with this one
   type Request_Rec is record
      Operation : Operation_Type;
      Origin    : Unbounded_String;
      Target    : Unbounded_String;
      Data      : Unbounded_String;
      Reply     : IRC.Server_Reply;
   end record;
   package Command_Queue_Pkg is new PQueue (Request_Rec);
   Requests : Command_Queue_Pkg.Protected_Queue_Type;

   task Command_Task;

   ---------------------------------------------------------------------------

end Command;
