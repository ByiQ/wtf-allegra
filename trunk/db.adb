
-- DB -- Low-level database utility package for Allegra info-bot


-- Standard library packages
with Ada.Unchecked_Deallocation;


package body DB is

------------------------------------------------------------------------------
--
-- Public subroutines
--
------------------------------------------------------------------------------

   -- Establish a connection to the given database on the given host.  May
   -- raise Connect_Error.
   procedure Connect (Handle : in out DB_Handle;
                      Host   : in     String;
                      DB     : in     String) is
   begin  -- Connect
      APQ.PostgreSQL.Client.Set_Host_Name (Handle, Host);
      APQ.PostgreSQL.Client.Set_DB_Name (Handle, DB);
      APQ.PostgreSQL.Client.Connect (Handle);

   exception
      -- Map APQ exceptions into our local generic exception
      when others =>
         raise Connect_Error;
   end Connect;

   ---------------------------------------------------------------------------

   -- Terminate the database connection
   procedure Disconnect (Handle : in out DB_Handle) is
   begin  -- Disconnect
      APQ.PostgreSQL.Client.Disconnect (Handle);
   end Disconnect;

   ---------------------------------------------------------------------------

   procedure Append_Quoted (Query  : in out DB_Result;
                            Handle : in     DB_Handle;
                            Str    : in     String) is
   begin  -- Append_Quoted
      APQ.PostgreSQL.Client.Append_Quoted (Query, Handle, Str);
   end Append_Quoted;

   ---------------------------------------------------------------------------

   procedure Execute (Query  : in out DB_Result;
                      Handle : in out DB_Handle) is
   begin  -- Execute
      APQ.PostgreSQL.Client.Execute (Query, Handle);
   end Execute;

   ---------------------------------------------------------------------------

   procedure Fetch (Query : in out DB_Result) is
   begin  -- Fetch
      APQ.PostgreSQL.Client.Fetch (Query);

   exception
      when APQ.No_Tuple =>
         null;
   end Fetch;

   ---------------------------------------------------------------------------

   function Rows (Result : DB_Result) return Natural is
   begin  -- Rows
      return Natural (APQ.PostgreSQL.Client.Tuples (Result));

   exception
      when APQ.No_Tuple =>
         return 0;
   end Rows;

   ---------------------------------------------------------------------------

   function Cols (Result : DB_Result) return Natural is
   begin  -- Cols
      return APQ.PostgreSQL.Client.Columns (Result);

   exception
      when APQ.No_Tuple =>
         return 0;
   end Cols;

   ---------------------------------------------------------------------------

   function Get_Value (Result : DB_Result;
                       Col    : Positive)
   return String is
   begin  -- Get_Value
      return APQ.PostgreSQL.Client.Value (Result, APQ.Column_Index_Type (Col));
   end Get_Value;

   ---------------------------------------------------------------------------

   function Get_Value (Result : DB_Result;
                       Field  : String)
   return String is
   begin  -- Get_Value
      return APQ.PostgreSQL.Client.Value (Result, APQ.PostgreSQL.Client.Column_Index (Result, Field));
   end Get_Value;

   ---------------------------------------------------------------------------

   function Get_Int_Value is new APQ.Integer_Value (Val_Type => Integer);

   ---------------------------------------------------------------------------

   function Get_Value (Result : DB_Result;
                       Col    : Positive)
   return Integer is
   begin  -- Get_Value
      return Get_Int_Value (Result, APQ.Column_Index_Type (Col));
   end Get_Value;

   ---------------------------------------------------------------------------

   function Get_Value (Result : DB_Result;
                       Field  : String)
   return Integer is
   begin  -- Get_Value
      return Get_Int_Value (Result, APQ.PostgreSQL.Client.Column_Index (Result, Field));
   end Get_Value;

   ---------------------------------------------------------------------------

end DB;
