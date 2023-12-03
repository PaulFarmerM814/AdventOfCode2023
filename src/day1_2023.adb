with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

package body day1_2023 is

   function Get_Input (S : Ada.Text_IO.File_Type)
                       return Integer
   is

      USV : constant Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Text_IO.Get_Line (S);

      SV : constant String := Ada.Strings.Unbounded.To_String (USV);

      first_digit : Integer := 0;
      first_found : Boolean := False;
      second_digit : Integer := 0;
      second_found : Boolean := False;

      ret_val : Integer;

   begin

      string_scan :
      for X in SV'Range loop

         if SV (X) in '0' .. '9' then
            if not first_found then
               first_found := True;
               first_digit := Integer'Value ("" & SV (X));
            end if;
         end if;

         if SV (SV'Length - X + 1) in '0' .. '9' then
            if not second_found then
               second_found := True;
               second_digit := Integer'Value ("" & SV (SV'Length - X + 1));
            end if;
         end if;

         exit string_scan when first_found and second_found;

      end loop string_scan;

      ret_val := (10 * first_digit) + second_digit;

      Ada.Text_IO.Put_Line (Item => SV & "   " & ret_val'img);

      return ret_val;

   end Get_Input;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;
      --  Input_Stream : Ada.Streams.Stream_IO.Stream_Access;
      running_total : Integer := 0;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day1_2023.txt");

      --  Input_Stream := Ada.Streams.Stream_IO.Stream (Input_File);

      while not Ada.Text_IO.End_Of_File (Input_File) loop

         running_total := running_total + Get_Input (Input_File);

      end loop;

      Ada.Text_IO.Put_Line ("total " & running_total'Image);

      Ada.Text_IO.Close(Input_File);

   end run;

end day1_2023;
