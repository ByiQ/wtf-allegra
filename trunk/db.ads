
-- DB -- Low-level database utility package for Allegra info-bot

-- Standard library packages
with Ada.Finalization;

-- Third-party library packages
with APQ.PostgreSQL.Client;


package DB is

------------------------------------------------------------------------------
--
-- Public types
--
------------------------------------------------------------------------------

   -- Database connection and query objects
   subtype DB_Handle is APQ.PostgreSQL.Client.Connection_Type;
   subtype DB_Result is APQ.PostgreSQL.Client.Query_Type;

------------------------------------------------------------------------------
--
-- Public exceptions
--
------------------------------------------------------------------------------

   -- All this package's routines raise this generic exception, since we don't
   -- really care *why* the connection failed, only that it did
   Connect_Error : exception;

------------------------------------------------------------------------------
--
-- Public subroutines
--
------------------------------------------------------------------------------

   -- Establish a connection to the given database on the given host.  May
   -- raise Connect_Error.
   procedure Connect (Handle : in out DB_Handle;
                      Host   : in     String;
                      DB     : in     String);

   -- Terminate the database connection
   procedure Disconnect (Handle : in out DB_Handle);

   -- These value-returning functions require a bit of thunking
   function Get_Value (Result : DB_Result;
                       Col    : Positive)
   return String;

   function Get_Value (Result : DB_Result;
                       Col    : Positive)
   return Integer;

   function Get_Value (Result : DB_Result;
                       Field  : String)
   return String;

   function Get_Value (Result : DB_Result;
                       Field  : String)
   return Integer;

   -- Re-export these library routines so that users don't need to "with" the
   -- PGAda packages
   procedure Prepare (Query : in out DB_Result;
                      Start : in     String;
                      After : in     String := APQ.Line_Feed)
   renames APQ.PostgreSQL.Client.Prepare;

   procedure Append (Query : in out DB_Result;
                     Start : in     String;
                     After : in     String := "")
   renames APQ.PostgreSQL.Client.Append;

   procedure Append_Quoted (Query  : in out DB_Result;
                            Handle : in     DB_Handle;
                            Str    : in     String);

   procedure Execute (Query  : in out DB_Result;
                      Handle : in out DB_Handle);

---   procedure Fetch (Query : in out DB_Result) renames APQ.PostgreSQL.Client.Fetch;
   procedure Fetch (Query : in out DB_Result);

   procedure Rewind (Query : in out DB_Result) renames APQ.PostgreSQL.Client.Rewind;

   function Rows (Result : DB_Result) return Natural;

   function Cols (Result : DB_Result) return Natural;

   ---------------------------------------------------------------------------

end DB;
