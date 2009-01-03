
--
-- OutputQ -- IRC output task request queue for Allegra info-bot
--


--
-- Local library packages
with Strings;
use  Strings;


package body OutputQ is

------------------------------------------------------------------------------
--
-- Public subroutines
--
------------------------------------------------------------------------------

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

   ---------------------------------------------------------------------------

   -- Simple interface to allow other tasks to write a notice to the user
   procedure Note (Msg : in string;  To : in string) is

      Req : Request_Rec;

   begin  -- Note
      Req.Operation   := Notice_Operation;
      Req.Destination := US (To);
      Req.Data        := US (Msg);
      Requests.Enqueue (Req);
   end Note;

   ---------------------------------------------------------------------------

end OutputQ;
