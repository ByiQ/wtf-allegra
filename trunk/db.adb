with
  PGAda.Database;

package body DB is

   procedure Connect (Handle:  out DB_Handle;
                      Host:    in  string;
                      DB:      in  string) is
   begin  -- Connect
      PGAda.Database.Set_DB_Login (Handle, Host => Host, DB_Name => DB);

   exception
      when others =>
         raise Connect_Error;
   end Connect;

   procedure Disconnect (Handle:  in out DB_Handle) is
   begin  -- Disconnect
      PGAda.Database.Finish (Handle);
   end Disconnect;

   procedure Fetch (Handle:  in  DB_Handle;
                    Fields:  in  string;
                    Table:   in  string;
                    Clause:  in  string;
                    Result:  out DB_Result) is
   begin  -- Fetch
      PGAda.Database.Exec (Handle, "SELECT " & Fields & " FROM " & Table & " " & Clause, Result);
   end Fetch;

   procedure Statement (Handle:  in DB_Handle;
                        Stmt:    in string) is
   begin  -- Statement
      PGAda.Database.Exec (Handle, Stmt);
   end Statement;

end DB;
