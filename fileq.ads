
--
-- FileQ -- File manipulation task request queue for Allegra info-bot
--


--
-- Local library packages
with PQueue;
with Strings;
use  Strings;


package FileQ is

------------------------------------------------------------------------------
--
-- Public types
--
------------------------------------------------------------------------------

   -- Define the types of requests other tasks can make of this task
   type Operation_Type is (Shutdown_Operation, RM_Operation, Help_Operation, Stats_Operation);
   type Request_Rec is record
      Operation   : Operation_Type;
      Destination : UString;
      Origin      : UString;
      Requestor   : UString;
      Data        : UString;
   end record;

------------------------------------------------------------------------------
--
-- Request queue
--
------------------------------------------------------------------------------

   -- Instantiate the protected-queue package with our request type, and
   -- create an instance of it to serve as this task's main input.
   package File_Queue_Pkg is new PQueue (Request_Rec);
   Requests : File_Queue_Pkg.Protected_Queue_Type;

   ---------------------------------------------------------------------------

end FileQ;
