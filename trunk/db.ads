
--
-- DB -- Low-level database utility package for Allegra info-bot
--


--
-- Third-party library packages
with PGAda.Database;
with PGAda.Syntax;


package DB is

------------------------------------------------------------------------------
--
-- Public types
--
------------------------------------------------------------------------------

   -- Rename these to shorter and (for me) more familiar names
   subtype DB_Handle is PGAda.Database.Connection_t;
   subtype DB_Result is PGAda.Database.Result_t;

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
                      Host   : in  string;
                      DB     : in  string);

   -- Terminate the database connection
   procedure Disconnect (Handle : in out DB_Handle);

   -- Perform a SQL "select" query to retrieve data from the database.  The
   -- query is constructed thusly:
   --
   -- select Fields from Table Clause
   procedure Fetch (Handle : in  DB_Handle;
                    Fields : in  string;
                    Table  : in  string;
                    Clause : in  string;
                    Result : out DB_Result);

   -- Perform an arbitrary SQL statement, useful for things like insert,
   -- delete, and update
   procedure Statement (Handle : in DB_Handle;
                        Stmt   : in string);

   -- Re-export these library routines so that users don't need to "with" the
   -- PGAda packages
   function Escape (S : string) return string renames PGAda.Syntax.Escape;

   function Get_Value (Result : in DB_Result;
                       Row    : in positive;
                       Col    : in positive)
                      return string renames PGAda.Database.Get_Value;

   function Get_Value (Result : in DB_Result;
                       Row    : in positive;
                       Col    : in positive)
                      return integer renames PGAda.Database.Get_Value;

   function Get_Value (Result : in DB_Result;
                       Row    : in positive;
                       Field  : in string)
                      return string renames PGAda.Database.Get_Value;

   function Get_Value (Result : in DB_Result;
                       Row    : in positive;
                       Field  : in string)
                      return integer renames PGAda.Database.Get_Value;

   function Rows (Result : in DB_Result) return natural    renames PGAda.Database.Nbr_Tuples;

   function Cols (Result : in DB_Result) return natural    renames PGAda.Database.Nbr_Fields;

   ---------------------------------------------------------------------------

end DB;
