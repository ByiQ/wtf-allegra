
--
-- DatabaseQ -- Database task request queue for Allegra info-bot
--


--
-- Application packages
with PQueue;
with Strings;
use  Strings;


package DatabaseQ is

------------------------------------------------------------------------------
--
-- Public types
--
------------------------------------------------------------------------------

   -- Define the types of requests other tasks can make of this task
   type Operation_Type is ( Access_Operation, AddFactoid_Operation,
                            FactoidStats_Operation, Fetch_Operation,
                            Forget_Operation, List_Operation, Quip_Operation,
                            Quote_Operation, RE_Fetch_Operation,
                            RE_Tell_Operation, Rename_Operation,
                            ResetFactoid_Operation, SetAction_Operation,
                            SetFactoid_Operation, SetReply_Operation,
                            Shutdown_Operation, Snack_Operation, Stats_Operation,
                            Tell_Operation );

   type Request_Rec is record
      Operation   : Operation_Type;
      Destination : UString;  -- place to send output
      Origin      : UString;  -- nick of requesting user
      Requestor   : UString;  -- user ID of requesting user
      Key         : UString;  -- request data, usually factoid name
      Data        : UString;  -- request data, like factoid definition, etc.
   end record;

------------------------------------------------------------------------------
--
-- Request queue
--
------------------------------------------------------------------------------

   -- Instantiate the protected-queue package with our request type, and
   -- create an instance of it to serve as this task's main input.
   package Database_Queue_Pkg is new PQueue (Request_Rec);
   Requests : Database_Queue_Pkg.Protected_Queue_Type;

   ---------------------------------------------------------------------------

end DatabaseQ;
