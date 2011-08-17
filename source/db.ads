
--
-- DB -- Low-level database utility package for Allegra info-bot
--


--
-- Third-party library packages
with PGAda.Connections;
with PGAda.Database;
with PGAda.Results;

package DB is

------------------------------------------------------------------------------
--
-- Public types
--
------------------------------------------------------------------------------

   -- Rename these to shorter and (for me) more familiar names
   subtype DB_Handle is PGAda.Connections.Connection;
   subtype DB_Result is PGAda.Results.Result;

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
   procedure Connect (Handle : out DB_Handle;
                      Host   : in  String;
                      DB     : in  String);

   -- Terminate the database connection
   procedure Disconnect (Handle : in out DB_Handle);

   -- Perform a SQL "select" query to retrieve data from the database.  The
   -- query is constructed thusly:
   --
   -- select Fields from Table Clause
   procedure Fetch (Handle : in  DB_Handle;
                    Fields : in  String;
                    Table  : in  String;
                    Clause : in  String;
                    Result : out DB_Result);

   -- Perform an arbitrary SQL statement, useful for things like insert,
   -- delete, and update
   procedure Statement (Handle : in DB_Handle;
                        Stmt   : in String);

   -- Re-export these library routines so that users don't need to "with" the
   -- PGAda packages

   function Get_Value (Result : in DB_Result;
                       Row    : in Positive;
                       Col    : in Positive)
                      return String renames PGAda.Database.Get_Value;

   function Get_Value (Result : in DB_Result;
                        Row    : in Positive;
                        Col    : in Positive)
                       return integer;

   function Get_Value (Result : in DB_Result;
                       Row    : in Positive;
                       Field  : in String)
                      return String renames PGAda.Database.Get_Value;

   function Get_Value (Result : in DB_Result;
                        Row    : in Positive;
                        Field  : in String)
                       return Integer;

   function Escape (S : String) return String;

   function Rows (Result : in DB_Result) return natural    renames PGAda.Results.Number_Of_Tuples;

   function Cols (Result : in DB_Result) return natural    renames PGAda.Results.Number_Of_Fields;

   ---------------------------------------------------------------------------

end DB;
