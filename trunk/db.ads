
with
  PGAda.Database,
  PGAda.Syntax;

package DB is

   subtype DB_Handle is PGAda.Database.Connection_Type;

   subtype DB_Result is PGAda.Database.Result_Type;

   Connect_Error:  exception;

   procedure Connect (Handle:  out DB_Handle;
                      Host:    in  string;
                      DB:      in  string);

   function Escape (S : string) return string renames PGAda.Syntax.Escape;

   procedure Disconnect (Handle:  in out DB_Handle);

   procedure Fetch (Handle:  in  DB_Handle;
                    Fields:  in  string;
                    Table:   in  string;
                    Clause:  in  string;
                    Result:  out DB_Result);

   function Get_Value (Result:  in DB_Result;
                       Row:     in positive;
                       Col:     in positive)
                      return string renames PGAda.Database.Get_Value;

   function Get_Value (Result:  in DB_Result;
                       Row:     in positive;
                       Col:     in positive)
                      return integer renames PGAda.Database.Get_Value;

   function Get_Value (Result:  in DB_Result;
                       Row:     in positive;
                       Field:   in string)
                      return string renames PGAda.Database.Get_Value;

   function Get_Value (Result:  in DB_Result;
                       Row:     in positive;
                       Field:   in string)
                      return integer renames PGAda.Database.Get_Value;

   function Rows (Result:  in DB_Result) return natural    renames PGAda.Database.Nbr_Tuples;

   function Cols (Result:  in DB_Result) return natural    renames PGAda.Database.Nbr_Fields;

   procedure Statement (Handle:  in DB_Handle;
                        Stmt:    in string);

end DB;
