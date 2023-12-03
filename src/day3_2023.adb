with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with GNAT.Regpat;

package body day3_2023 is

   type line_t is (previous, current, next);

   type parsing_lines_t is array (line_t) of Ada.Strings.Unbounded.Unbounded_String;

   parsing_lines : parsing_lines_t := (others => Ada.Strings.Unbounded.Null_Unbounded_String);

   type part_array_t is array (1 .. 2) of Integer;

   type gear_id_t is record
      valid : Boolean := False;
      column : Integer := 0;
      parts : part_array_t;
   end record;

   type gear_array_t is array (1 .. 150, 1 ..150) of gear_id_t;

   gear_store : gear_array_t;

   Re_Part_id      : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("([[:digit:]]+)");

   procedure Get_Input (lines : parsing_lines_t;
                        row   : Integer;
                        parts_on_line_sum : out Integer)
   is
      use type GNAT.Regpat.Match_Location;
      use type Ada.Strings.Unbounded.Unbounded_String;

      Matches : GNAT.Regpat.Match_Array (0 .. 0);

      current_index : Integer := 1;

      current_part : Integer;

      parsing_lines_as_string : constant String := Ada.Strings.Unbounded.To_String (lines (current));

      part_valid : Boolean;

      current_char : Character;

      gear_found : Boolean := False;

      start_point : Integer;
      end_point   : Integer;

   begin

      parts_on_line_sum := 0;

      pattern_match :
      loop
         GNAT.Regpat.Match (Re_Part_id, parsing_lines_as_string, Matches, current_index);

         exit pattern_match when Matches (0) = GNAT.Regpat.No_Match;

         part_valid := False;
         gear_found := False;

         Ada.Text_IO.Put_Line (Item => parsing_lines_as_string (Matches (0).First .. Matches (0).Last));

         current_part := Integer'Value (parsing_lines_as_string (Matches (0).First .. Matches (0).Last));

         if Matches (0).First /= parsing_lines_as_string'First
         then
            if parsing_lines_as_string (Matches (0).First - 1) /= '.' and then parsing_lines_as_string (Matches (0).First - 1) not in '0' .. '9'
            then
               parts_on_line_sum := parts_on_line_sum + current_part;
               part_valid := True;
               Ada.Text_IO.Put_Line (Item => "Symbol Match - previous character");

               if parsing_lines_as_string (Matches (0).First - 1) = '*'
               then
                  if gear_store (row, Matches (0).First - 1).column = Matches (0).First - 1
                  then
                     if gear_store (row, Matches (0).First - 1).parts (1) /= 0
                     then
                        gear_store (row, Matches (0).First - 1).parts (2) := current_part;
                     else
                        gear_store (row, Matches (0).First - 1).parts (1) := current_part;
                     end if;
                  end if;

                  if gear_store (row, Matches (0).First - 1).column = 0
                  then
                     gear_store (row, Matches (0).First - 1).parts (1) := current_part;
                     gear_store (row, Matches (0).First - 1).column := Matches (0).First - 1;
                  end if;
               end if;
            end if;
         end if;

         if Matches (0).Last /= parsing_lines_as_string'Last and then not part_valid
         then
            if parsing_lines_as_string (Matches (0).Last + 1) /= '.' and then parsing_lines_as_string (Matches (0).Last + 1) not in '0' .. '9'
            then
               parts_on_line_sum := parts_on_line_sum + current_part;
               part_valid := True;
               Ada.Text_IO.Put_Line (Item => "Symbol Match - next character");

               if parsing_lines_as_string (Matches (0).Last + 1) = '*'
               then
                  if gear_store (row, Matches (0).Last + 1).column = Matches (0).Last + 1
                  then
                     if gear_store (row, Matches (0).Last + 1).parts (1) /= 0
                     then
                        gear_store (row, Matches (0).Last + 1).parts (2) := current_part;
                     else
                        gear_store (row, Matches (0).Last + 1).parts (1) := current_part;
                     end if;
                  end if;

                  if gear_store (row, Matches (0).Last + 1).column = 0
                  then
                     gear_store (row, Matches (0).Last + 1).parts (1) := current_part;
                     gear_store (row, Matches (0).Last + 1).column := Matches (0).Last + 1;
                  end if;
               end if;
            end if;
         end if;

         if lines (previous) /= Ada.Strings.Unbounded.Null_Unbounded_String and then not part_valid
         then
            start_point := Integer'Max (Matches (0).First - 1, parsing_lines_as_string'First);
            end_point := Integer'Min (Matches (0).Last + 1, parsing_lines_as_string'Last);

            PREV_LINE :
            for x in start_point .. end_point
            loop
               current_char := Ada.Strings.Unbounded.Element (lines (previous), x);
               if current_char /= '.' and then current_char not in '0' .. '9' and then not part_valid
               then
                  parts_on_line_sum := parts_on_line_sum + current_part;
                  part_valid := True;
                  Ada.Text_IO.Put_Line (Item => "Symbol Match - previous line");
               end if;

               if current_char = '*'
               then
                  if gear_store (row - 1, x).column = x
                  then
                     if gear_store (row - 1, x).parts (1) /= 0
                     then
                        gear_store (row - 1, x).parts (2) := current_part;
                     else
                        gear_store (row - 1, x).parts (1) := current_part;
                     end if;
                  end if;

                  if gear_store (row - 1, x).column = 0
                  then
                     gear_store (row - 1, x).parts (1) := current_part;
                     gear_store (row - 1, x).column := x;
                  end if;
               end if;
            end loop PREV_LINE;
         end if;

         if lines (next) /= Ada.Strings.Unbounded.Null_Unbounded_String and then not part_valid
         then
            start_point := Integer'Max (Matches (0).First - 1, parsing_lines_as_string'First);
            end_point := Integer'Min (Matches (0).Last + 1, parsing_lines_as_string'Last);

            NEXT_LINE :
            for x in start_point .. end_point
            loop
               current_char := Ada.Strings.Unbounded.Element (lines (next), x);
               if current_char /= '.' and then current_char not in '0' .. '9' and then not part_valid
               then
                  parts_on_line_sum := parts_on_line_sum + current_part;
                  part_valid := True;
                  Ada.Text_IO.Put_Line (Item => "Symbol Match - next line");
               end if;

               if current_char = '*'
               then
                  if gear_store (row + 1, x).column = x
                  then
                     if gear_store (row + 1, x).parts (1) /= 0
                     then
                        gear_store (row + 1, x).parts (2) := current_part;
                     else
                        gear_store (row + 1, x).parts (1) := current_part;
                     end if;
                  end if;

                  if gear_store (row + 1, x).column = 0
                  then
                     gear_store (row + 1, x).parts (1) := current_part;
                     gear_store (row + 1, x).column := x;
                  end if;
               end if;
            end loop NEXT_LINE;
         end if;

         current_index := Matches (0).Last + 1;

      end loop pattern_match;

   end Get_Input;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;
      --  Input_Stream : Ada.Streams.Stream_IO.Stream_Access;
      running_total_parts : Integer := 0;
      parts_on_line_sum : Integer := 0;
      current_row : Integer := 0;

      ratio_total : Integer := 0;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day3_2023.txt");

      --  Input_Stream := Ada.Streams.Stream_IO.Stream (Input_File);

      parsing_lines (next) := Ada.Strings.Unbounded.Text_IO.Get_Line (Input_File);

      while not Ada.Text_IO.End_Of_File (Input_File) loop

         parsing_lines (previous) := parsing_lines (current);
         parsing_lines (current)  := parsing_lines (next);
         parsing_lines (next)     := Ada.Strings.Unbounded.Text_IO.Get_Line (Input_File);

         current_row := current_row + 1;

         Get_Input (parsing_lines,
                    current_row,
                    parts_on_line_sum);

         running_total_parts := running_total_parts + parts_on_line_sum;

      end loop;

      parsing_lines (previous) := parsing_lines (current);
      parsing_lines (current)  := parsing_lines (next);
      parsing_lines (next)     := Ada.Strings.Unbounded.Null_Unbounded_String;

      current_row := current_row + 1;

      Get_Input (parsing_lines,
                 current_row,
                 parts_on_line_sum);

      running_total_parts := running_total_parts + parts_on_line_sum;

      Ada.Text_IO.Put_Line ("total " & running_total_parts'Image);

      for x in gear_store'Range(1)
      loop
         for y in gear_store'Range(2)
         loop
            if gear_store (x,y).column /= 0
            then
               if gear_store (x,y).parts(1) /= 0 and then gear_store(x,y).parts(2) /= 0
               then
                  ratio_total := ratio_total + (gear_store (x,y).parts(1) * gear_store(x,y).parts(2));
               end if;
            end if;
         end loop;
      end loop;

      Ada.Text_IO.Put_Line ("ratio " & ratio_total'Image);

      Ada.Text_IO.Close (Input_File);

   end run;

end day3_2023;
