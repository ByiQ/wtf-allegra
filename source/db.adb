
--
-- DB -- Low-level database utility package for Allegra info-bot
--

package body DB is

------------------------------------------------------------------------------
--
-- Public subroutines
--
------------------------------------------------------------------------------

   -- Establish a connection to the given database on the given host.  May
   -- raise Connect_Error.
   procedure Connect (Handle   : out DB_Handle;
                      Host     : in String;
                      Port     : in Natural := 0;
                      Options  : in String  := "";
                      TTY      : in String  := "";
                      DB       : in String  := "";
                      Login    : in String  := "";
                      Password : in String  := "") is
   begin  -- Connect
      PGAda.Connections.Connect
         (Connection => Handle,
          Host       => Host,
          Port       => Port,
          Options    => Options,
          TTY        => TTY,
          DB_Name    => DB ,
          Login      => Login,
          Password   => Password);
   exception
      -- Map PGAda exceptions into our local generic exception
      when others =>
         raise Connect_Error;
   end Connect;

   ---------------------------------------------------------------------------

   -- Terminate the database connection
   procedure Disconnect (Handle : in out DB_Handle) is
   begin  -- Disconnect
      PGAda.Connections.Finish (Handle);
   end Disconnect;

   ---------------------------------------------------------------------------

   -- Perform a SQL "select" query to retrieve data from the database.  The
   -- query is constructed thusly:
   --
   -- select Fields from Table Clause
   procedure Fetch (Handle : in  DB_Handle;
                    Fields : in  String;
                    Table  : in  String;
                    Clause : in  String;
                    Result : out DB_Result) is
   begin  -- Fetch
      PGAda.Database.Exec (Handle, "SELECT " & Fields & " FROM " & Table & " " & Clause, Result);
   end Fetch;

   ---------------------------------------------------------------------------

   -- Perform an arbitrary SQL statement, useful for things like insert,
   -- delete, and update
   procedure Statement (Handle : in DB_Handle;
                        Stmt   : in String) is
   begin  -- Statement
      PGAda.Database.Exec (Handle, Stmt);
   end Statement;

   ---------------------------------------------------------------------------

   function Get_Value (Result : in DB_Result;
                       Row    : in Positive;
                       Field  : in String)
   return Integer
   is
   begin
      return Integer'Value (Get_Value (Result, Row, Field) );
   end Get_Value;

   function Get_Value (Result : in DB_Result;
                       Row    : in Positive;
                       Col    : in Positive)
   return Integer
   is
   begin
      return Integer'Value (Get_Value (Result, Row, Col) );
   end Get_Value;

  function Escape (S : String) return String is
    Result : String (1 .. S'Length * 2);
    Last   : Natural := 0;
  begin
    for I in S'Range loop
      if S (I) = ''' then
        Last := Last + 1;
        Result (Last) := ''';
      end if;
      Last := Last + 1;
      Result (Last) := S (I);
    end loop;
    return ''' & Result (1 .. Last) & ''';
  end Escape;
end DB;

