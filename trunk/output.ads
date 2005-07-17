
--
-- Output -- IRC output task package for Allegra info-bot
--


--
-- Local library packages
with PQueue;
with Strings;
use  Strings;


package Output is

   ---------------------------------------------------------------------------

   -- Define the types of requests other tasks can make of this task
   type Operation_Type is ( Shutdown_Operation, Nick_Operation, User_Operation, Join_Operation,
                            Message_Operation, Ping_Operation, Pong_Operation );
   type Request_Rec is record
      Operation   : Operation_Type;
      Destination : UString;
      Data        : UString;
   end record;

   -- Instantiate the protected-queue package with our request type, and
   -- create an instance of it to serve as this task's main input.
   package Output_Queue_Pkg is new PQueue (Request_Rec);
   Requests : Output_Queue_Pkg.Protected_Queue_Type;

   -- Declare the actual task
   task Output_Task;

   ---------------------------------------------------------------------------

   -- Simple interfaces to allow other tasks to write a message to the user
   procedure Say (Msg : in UString;  To : in UString);
   procedure Say (Msg : in string;   To : in UString);
   procedure Say (Msg : in UString;  To : in string);
   procedure Say (Msg : in string;   To : in string);

   ---------------------------------------------------------------------------

end Output;
