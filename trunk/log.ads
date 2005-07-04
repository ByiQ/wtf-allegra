package Log is

   -- Logging procedures
   procedure Err  (From:  string;   Msg:  string);
   procedure Warn (From:  string;   Msg:  string);
   procedure Info (From:  string;   Msg:  string);
   procedure Dbg  (From:  string;   Msg:  string);

   -- Initialization procedure
   procedure Init;
   procedure WrapUp;

end Log;
