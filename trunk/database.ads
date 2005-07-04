with
  Ada.Strings.Unbounded,
  PQueue;

use
  Ada.Strings.Unbounded;

package Database is

   Database_Name:  constant string := "Database";

   type Operation_Type is ( Access_Operation, AddFactoid_Operation,
                            FactoidStats_Operation, Fetch_Operation,
                            Forget_Operation, List_Operation, Quip_Operation,
                            Quote_Operation, RE_Fetch_Operation,
                            RE_Tell_Operation, Rename_Operation,
                            ResetFactoid_Operation, SetAction_Operation,
                            SetFactoid_Operation, SetReply_Operation,
                            Shutdown_Operation, Stats_Operation,
                            Tell_Operation );

   type Request_Rec is record
      Operation:    Operation_Type;
      Destination:  Unbounded_String;
      Origin:       Unbounded_String;
      Requestor:    Unbounded_String;
      Key:          Unbounded_String;
      Data:         Unbounded_String;
   end record;

   task Database_Task;

   package Database_Queue_Pkg is new PQueue (Request_Rec);

   Requests:  Database_Queue_Pkg.Protected_Queue_Type;

end Database;
