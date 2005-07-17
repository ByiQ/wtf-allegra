
--
-- File -- File manipulation task package for Allegra info-bot
--


--
-- Local library packages
with PQueue;
with Strings;
use  Strings;


package File is

   ---------------------------------------------------------------------------

   -- Define the types of requests other tasks can make of this task
   type Operation_Type is (Shutdown_Operation, RM_Operation, Help_Operation);
   type Request_Rec is record
      Operation   : Operation_Type;
      Destination : UString;
      Origin      : UString;
      Requestor   : UString;
      Data        : UString;
   end record;

   -- Instantiate the protected-queue package with our request type, and
   -- create an instance of it to serve as this task's main input.
   package File_Queue_Pkg is new PQueue (Request_Rec);
   Requests : File_Queue_Pkg.Protected_Queue_Type;

   -- Declare the actual task
   task File_Task;

   ---------------------------------------------------------------------------

end File;
