
--
-- NetQ -- Non-IRC network connection handling task request queue for Allegra info-bot
--


--
-- Local library packages
with PQueue;
with Strings;
use  Strings;


package NetQ is

------------------------------------------------------------------------------
--
-- Public types
--
------------------------------------------------------------------------------

   -- Define the types of requests other tasks can make of this task
   type Operation_Type is (Shutdown_Operation, Shorten_URL_Operation);
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
   package Net_Queue_Pkg is new PQueue (Request_Rec);
   Requests : Net_Queue_Pkg.Protected_Queue_Type;

   ---------------------------------------------------------------------------

end NetQ;
