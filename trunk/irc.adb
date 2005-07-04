with
  Ada.Text_IO,
  Ada.Strings.Unbounded,
  Sockets;

use
  Ada.Text_IO,
  Ada.Strings.Unbounded;

package body IRC is

   Space: constant character := Ada.Strings.Space;

   Handle:  Sockets.Socket_FD;

   procedure Open_Server (Name:  in string;
                          Port:  in positive) is
   begin  -- Open_Server
      Sockets.Socket (Sockets.Socket_FD (Handle));
      Sockets.Connect (Sockets.Socket_FD (Handle), Name, Port);

   exception
      when others =>
         raise Connect_Error;
   end Open_Server;

   procedure Close_Server is
   begin  -- Close_Server
      Sockets.Shutdown (Sockets.Socket_FD (Handle));
   end Close_Server;

   procedure Read (Message: out Message_Rec) is

      Scan:   positive;
      Start:  positive;
      Len:    natural;

   begin  -- Read
      declare
         Input_Line:  string := Sockets.Get_Line (Handle);
      begin
         Scan := Input_Line'First;
         Len  := Input_Line'Last;
         Message.Prefix := Null_Field;
         if Scan <= Len and then Input_Line (Scan) = ':' then
            Scan := Scan + 1;
            Start := Scan;
            while Scan <= Len and then Input_Line (Scan) /= Space loop
               Scan := Scan + 1;
            end loop;
            if Scan > Start then
               Message.Prefix := To_Unbounded_String (Input_Line (Start .. Scan - 1));
            end if;
         end if;
         Message.Command := Null_Field;
         if Scan <= Len then
            if Input_Line (Scan) = Space then
               Scan := Scan + 1;
            end if;
            Start := Scan;
            while Scan <= Len and then Input_Line (Scan) /= Space loop
               Scan := Scan + 1;
            end loop;
            if Scan > Start then
               Message.Command := To_Unbounded_String (Input_Line (Start .. Scan - 1));
            end if;
         end if;
         Message.Params := Null_Field;
         if Scan <= Len then
            if Input_Line (Scan) = Space then
               Scan := Scan + 1;
            end if;
            if Scan <= Len then
               Message.Params := To_Unbounded_String (Input_Line (Scan .. Len));
            end if;
         end if;
      end;

   exception
      when others =>
         raise Connect_Error;
   end Read;

   procedure Write (Message: in Message_Rec) is
   begin  -- Write
      if Message.Prefix /= Null_Field then
         Sockets.Put_Line (Handle, ":" & To_String (Message.Prefix) & Space &
                           To_String (Message.Command) & Space &
                           To_String (Message.Params));
      else
         Sockets.Put_Line (Handle, To_String (Message.Command) & Space &
                           To_String (Message.Params));
      end if;

   exception
      when others =>
         raise Connect_Error;
   end Write;

   procedure Parse_Params (Param_Str:  in  Unbounded_String;
                           Params:     out Param_Arr;
                           Count:      out Param_Count) is

      Scan:   positive;
      Start:  positive;
      Len:    natural;

   begin  -- Parse_Params
      Count := 0;
      Scan := 1;
      Len := Length (Param_Str);
      loop
         while Scan <= Len and then Element (Param_Str, Scan) = Space loop
            Scan := Scan + 1;
         end loop;
         exit when Scan > Len;
         Start := Scan;
         if Element (Param_Str, Start) = ':' then
            Start := Start + 1;
            Scan := Len + 1;
         elsif Count >= Max_Params - 1 then
            Scan := Len + 1;
         else
            while Scan <= Len and then Element (Param_Str, Scan) /= Space loop
               Scan := Scan + 1;
            end loop;
         end if;
         if Start <= Len then
            Count := Count + 1;
            Params (Count) := Trim (To_Unbounded_String (Slice (Param_Str, Start, Scan - 1)), Side => Ada.Strings.Both);
         end if;
      end loop;
   end Parse_Params;

   procedure Parse_MsgTo (Str:   in  Unbounded_String;
                          MsgTo: out MsgTo_Rec) is

      Scan:   positive;
      Start:  positive;
      Len:    natural;

   begin  -- Parse_MsgTo
      Len := Length (Str);
      MsgTo.Nick := Null_Field;
      MsgTo.User := Null_Field;
      MsgTo.Host := Null_Field;
      Scan := 1;
      Start := Scan;
      while Scan <= Len and then Element (Str, Scan) /= '!' loop
         Scan := Scan + 1;
      end loop;
      if Scan <= Len and then Element (Str, Scan) = '!' then
         MsgTo.Nick := Trim (To_Unbounded_String (Slice (Str, Start, Scan - 1)), Side => Ada.Strings.Both);
         Scan := Scan + 1;
      else
         Scan := 1;
      end if;
      Start := Scan;
      if Start <= Len then
         while Scan <= Len and then Element (Str, Scan) /= '@' loop
            Scan := Scan + 1;
         end loop;
         if Scan <= Len and then Element (Str, Scan) = '@' then
            MsgTo.User := Trim (To_Unbounded_String (Slice (Str, Start, Scan - 1)), Side => Ada.Strings.Both);
            Scan := Scan + 1;
         else
            if MsgTo.Nick /= Null_Field then
               MsgTo.User := Trim (To_Unbounded_String (Slice (Str, Start, Len)), Side => Ada.Strings.Both);
            else
               MsgTo.Nick := Trim (Str, Side => Ada.Strings.Both);
            end if;
         end if;
      end if;
      Start := Scan;
      if Start <= Len then
         MsgTo.Host := Trim (To_Unbounded_String (Slice (Str, Start, Len)), Side => Ada.Strings.Both);
      end if;
      if MsgTo.Nick /= Null_Field and then Length (MsgTo.Nick) = 0 then
         MsgTo.Nick := Null_Field;
      end if;
      if MsgTo.User /= Null_Field and then Length (MsgTo.User) = 0 then
         MsgTo.User := Null_Field;
      end if;
      if MsgTo.Host /= Null_Field and then Length (MsgTo.Host) = 0 then
         MsgTo.Host := Null_Field;
      end if;
   end Parse_MsgTo;

end IRC;
