
--
-- DB -- Low-level database utility package for Allegra info-bot
--


--
-- Third-party library packages
with PGAda.Database;


package body DB is

------------------------------------------------------------------------------
--
-- Public subroutines
--
------------------------------------------------------------------------------

   -- Establish a connection to the given database on the given host.  May
   -- raise Connect_Error.
   procedure Connect (Handle : out DB_Handle;
                      Host   : in  string;
                      DB     : in  string) is
   begin  -- Connect
      PGAda.Database.Set_DB_Login (Handle, Host => Host, DB_Name => DB);

   exception
      -- Map PGAda exceptions into our local generic exception
      when others =>
         raise Connect_Error;
   end Connect;

   ---------------------------------------------------------------------------

   -- Terminate the database connection
   procedure Disconnect (Handle : in out DB_Handle) is
   begin  -- Disconnect
      PGAda.Database.Finish (Handle);
   end Disconnect;

   ---------------------------------------------------------------------------

   -- Perform a SQL "select" query to retrieve data from the database.  The
   -- query is constructed thusly:
   --
   -- select Fields from Table Clause
   procedure Fetch (Handle : in  DB_Handle;
                    Fields : in  string;
                    Table  : in  string;
                    Clause : in  string;
                    Result : out DB_Result) is
   begin  -- Fetch
      PGAda.Database.Exec (Handle, "SELECT " & Fields & " FROM " & Table & " " & Clause, Result);
   end Fetch;

   ---------------------------------------------------------------------------

   -- Perform an arbitrary SQL statement, useful for things like insert,
   -- delete, and update
   procedure Statement (Handle : in DB_Handle;
                        Stmt   : in string) is
   begin  -- Statement
      PGAda.Database.Exec (Handle, Stmt);
   end Statement;

   ---------------------------------------------------------------------------

end DB;
