with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

package body day14_2023 is


   type rocks_t is array(Long_Long_Integer range 1..100) of string(1..100);

   rocks : rocks_t := [others => [others => '.']];

   current_row : Long_Long_Integer := 0;

   --  constants for determining cycle - increase if no cycle found
   cycle_rounds : constant := 1000;
   max_load : constant := 120_000;

   --  used for detecting cycles
   saved_load : array (Long_Long_Integer range 1 .. max_load) of string(1..10000) := [others => [others => '.']];

   step : Long_Long_Integer := 0;
   cycle : Long_Long_Integer;

   total_run : constant := 1_000_000_000;
   remaining_cycles : Long_Long_Integer;

   procedure Get_Input (S   : Ada.Text_IO.File_Type;
                        row : Long_Long_Integer;
                        row_length : out Integer)
   is
      USV : constant Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Text_IO.Get_Line (S);

      SV : constant String := Ada.Strings.Unbounded.To_String (USV);

   begin

      row_length := SV'Length;

      if SV'Length = 100
      then
         rocks(row) := SV;
      else
         rocks(row)(1..SV'Length) := SV;
      end if;

   end Get_Input;

   function Get_Load return Long_Long_Integer
   is

      total_load : Long_Long_Integer := 0;

   begin
      for x in 1..current_row
      loop
         --  ada.text_io.put_line(rocks(x));

         for y in 1..100
         loop
            if rocks(x)(y) = 'O'
            then
               total_load := total_load + current_row - x + 1;
            end if;
         end loop;
      end loop;

      return total_load;

   end Get_Load;

   function Check_For_Cycle return Boolean
   is
      found : boolean := False;

      saved : string(1..10_000);
   begin

      step := step + 1;

      for x in 0..99
      loop
         saved((x * 100) + 1..(x * 100) + 100) := rocks(Long_Long_Integer(x) + 1);
      end loop;

      for x in reverse 1..step - 1
      loop
         if saved_load (x) = saved
         then

            found := True;
            cycle := step - x;

            Ada.Text_IO.Put_line
              ("  Step: " & step'Img & "  repeat: " & saved_load (x)'Img & " cycle " & cycle'Img);

            exit;
         end if;
      end loop;

      saved_load (step) := saved;

      return found;

   end Check_For_Cycle;


   procedure run is

      Input_File   : Ada.Text_IO.File_Type;

      cycle_found : Boolean;

      row_length : Integer;

      procedure north
      is
      begin
         for x in 2..current_row
         loop
            for y in 1..100
            loop
               if rocks(x)(y) = 'O'
               then
                  for z in reverse 1..x - 1
                  loop
                     if rocks(z)(y) = '.'
                     then
                        rocks(z)(y) := 'O';
                        rocks(z + 1)(y) := '.';
                     else
                        exit;
                     end if;
                  end loop;
               end if;
            end loop;
         end loop;
      end north;

      procedure west
      is
      begin
         for y in 2..row_length
         loop
            for x in 1..current_row
            loop
               if rocks(x)(y) = 'O'
               then
                  for z in reverse 1..y - 1
                  loop
                     if rocks(x)(z) = '.'
                     then
                        rocks(x)(z) := 'O';
                        rocks(x)(z + 1) := '.';
                     else
                        exit;
                     end if;
                  end loop;
               end if;
            end loop;
         end loop;
      end west;

      procedure south
      is
      begin
         for x in reverse 1..current_row - 1
         loop
            for y in 1..100
            loop
               if rocks(x)(y) = 'O'
               then
                  for z in x + 1..current_row
                  loop
                     if rocks(z)(y) = '.'
                     then
                        rocks(z)(y) := 'O';
                        rocks(z - 1)(y) := '.';
                     else
                        exit;
                     end if;
                  end loop;
               end if;
            end loop;
         end loop;
      end south;

      procedure east
      is
      begin
         for y in reverse 1..row_length - 1
         loop
            for x in 1..current_row
            loop
               if rocks(x)(y) = 'O'
               then
                  for z in y + 1..row_length
                  loop
                     if rocks(x)(z) = '.'
                     then
                        rocks(x)(z) := 'O';
                        rocks(x)(z - 1) := '.';
                     else
                        exit;
                     end if;
                  end loop;
               end if;
            end loop;
         end loop;
      end east;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day14_2023.txt");

      while not Ada.Text_IO.End_Of_File (Input_File) loop

         current_row := current_row + 1;

         Get_Input (Input_File,
                    current_row,
                    row_length);

      end loop;

      for a in 1 .. cycle_rounds
      loop

         north;
         west;
         south;
         east;

         cycle_found := check_for_cycle;

         exit when cycle_found;

      end loop;

      remaining_cycles := total_run - step;

      remaining_cycles := remaining_cycles mod cycle;

      for a in 1 .. remaining_cycles
      loop

         north;
         west;
         south;
         east;

      end loop;

      ada.text_io.put_line("Total Load " & Get_Load'Img);

      Ada.Text_IO.Close (Input_File);

   end run;

end day14_2023;
