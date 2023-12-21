with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

package body day13_2023 is

   --  To find the reflection in each pattern, you need to find a perfect reflection
   --  across either a horizontal line between two rows or across a vertical line between two columns.
   --
   --  In the first pattern, the reflection is across a vertical line between two columns;
   --  arrows on each of the two columns point at the line between the columns:
   --
   --  123456789
   --      ><
   --  #.##..##.
   --  ..#.##.#.
   --  ##......#
   --  ##......#
   --  ..#.##.#.
   --  ..##..##.
   --  #.#.##.#.
   --      ><
   --  123456789
   --  In this pattern, the line of reflection is the vertical line between columns 5 and 6.
   --  Because the vertical line is not perfectly in the middle of the pattern, part of the pattern (column 1) has nowhere to reflect onto and can be ignored; every other column has a reflected column within the pattern and must match exactly: column 2 matches column 9, column 3 matches 8, 4 matches 7, and 5 matches 6.
   --
   --  The second pattern reflects across a horizontal line instead:
   --
   --  1 #...##..# 1
   --  2 #....#..# 2
   --  3 ..##..### 3
   --  4v#####.##.v4
   --  5^#####.##.^5
   --  6 ..##..### 6
   --  7 #....#..# 7
   --  This pattern reflects across the horizontal line between rows 4 and 5.
   --  Row 1 would reflect with a hypothetical row 8, but since that's not in the pattern, row 1 doesn't need to match anything.
   --  The remaining rows match: row 2 matches row 7, row 3 matches row 6, and row 4 matches row 5.
   --
   --  To summarize your pattern notes, add up the number of columns to the
   --  left of each vertical line of reflection; to that,
   --  also add 100 multiplied by the number of rows above each horizontal line of reflection.
   --  In the above example, the first pattern's vertical line has 5 columns to its left and the second pattern's
   --  horizontal line has 4 rows above it, a total of 405.
   --
   --  Find the line of reflection in each of the patterns in your notes.
   --  What number do you get after summarizing all of your notes?

   type rocks_t is array(Integer range 1..50) of Ada.Strings.Unbounded.Unbounded_String;

   rocks_horiz : rocks_t := [others => Ada.Strings.Unbounded.Null_Unbounded_String];
   rocks_vert : rocks_t := [others => Ada.Strings.Unbounded.Null_Unbounded_String];

   procedure Get_Input (S : Ada.Text_IO.File_Type;
                       subtotal : out Integer)
   is

      row_found : Boolean := True;

      current_row : Integer := 0;

      horiz_match_found : Boolean := False;
      vert_match_found : Boolean := False;
      current_horiz_match_found : Boolean := False;
      current_vert_match_found : Boolean := False;

      new_row : Ada.Strings.Unbounded.Unbounded_String;

      row_length : Integer;

      horiz_row : Integer;
      vert_row : Integer;
      current_horiz_row : Integer;
      current_vert_row : Integer;

      no_match : Boolean := False;

      check : Integer := 0;

      horiz_inconsistencies : Natural := 0;
      vert_inconsistencies  : Natural := 0;
      inconsistencies1 : Natural := 0;

      function inconsistency_count(row1 : Ada.Strings.Unbounded.Unbounded_String;
                                   row2 : Ada.Strings.Unbounded.Unbounded_String) return Natural
      is

         count : Natural := 0;

      begin
         for x in 1..Ada.Strings.Unbounded.Length(row1)
         loop
            if Ada.Strings.Unbounded.Slice(row1,x,x) /= Ada.Strings.Unbounded.Slice(row2,x,x)
            then
               count := count + 1;
            end if;
            exit when count > 1;
         end loop;
         return count;
      end inconsistency_count;

   begin

      while not Ada.Text_IO.End_Of_File (S) and then row_found
      loop

         declare
         USV : constant Ada.Strings.Unbounded.Unbounded_String :=
           Ada.Strings.Unbounded.Text_IO.Get_Line (S);
         begin
            if Ada.Strings.Unbounded.Length(USV) /= 0
            then
               row_length := Ada.Strings.Unbounded.Length(USV);
               current_row := current_row + 1;
               rocks_horiz(current_row) := USV;
            else
               row_found := false;
            end if;
         end;
      end loop;

      if current_row = 0
      then
         subtotal := 0;
         return;
      end if;

      --  check for horizontal reflection
      --  look for 2 matching rows
      --  and then back too the edge
      horiz_scan:
      for y in 1 .. current_row - 1
      loop
         horiz_inconsistencies := inconsistency_count(rocks_horiz(y), rocks_horiz(y + 1));
         if horiz_inconsistencies <= 1
         then
            current_horiz_row := y;
            current_horiz_match_found := True;
            inconsistencies1 := 0;

            check := Integer'Min(y - 1, current_row - y - 1);

            for z in 1 .. check
            loop
               inconsistencies1 := inconsistencies1 + inconsistency_count(rocks_horiz(y - z), rocks_horiz(y + 1 + z));
               if (horiz_inconsistencies + inconsistencies1 > 1)
               then
                  current_horiz_match_found := False;
                  exit;
               end if;
            end loop;

            if current_horiz_match_found
            then
               horiz_match_found := current_horiz_match_found;
               horiz_row := current_horiz_row;
               horiz_inconsistencies := horiz_inconsistencies + inconsistencies1;
               exit horiz_scan when horiz_inconsistencies = 1;
            end if;
         end if;
      end loop horiz_scan;

      for x in 1 .. row_length
      loop
         new_row := Ada.Strings.Unbounded.Null_Unbounded_String;

         for y in 1 .. current_row
         loop
            Ada.Strings.Unbounded.Append(new_row, Ada.Strings.Unbounded.Slice(rocks_horiz(y),x,x));
         end loop;

         rocks_vert(x) := new_row;
      end loop;

      --  check for vertical reflection
      --  look for 2 matching rows
      --  and then back too the edge
      vert_scan:
      for y in 1 .. row_length - 1
      loop
         vert_inconsistencies := inconsistency_count(rocks_vert(y), rocks_vert(y + 1));
         if vert_inconsistencies <= 1
         then
            current_vert_row := y;
            current_vert_match_found := True;
            inconsistencies1 := 0;

            check := Integer'Min(y - 1, row_length - y - 1);

            for z in 1 .. check
            loop
               inconsistencies1 := inconsistencies1 + inconsistency_count(rocks_vert(y - z), rocks_vert(y + 1 + z));
               if (vert_inconsistencies + inconsistencies1 > 1)
               then
                  current_vert_match_found := False;
                  exit;
               end if;
            end loop;

            if current_vert_match_found
            then
               vert_match_found := current_vert_match_found;
               vert_row := current_vert_row;
               vert_inconsistencies := vert_inconsistencies + inconsistencies1;
               exit vert_scan when vert_inconsistencies = 1;
            end if;
         end if;
      end loop vert_scan;

      if horiz_match_found and then horiz_inconsistencies = 1
      then
         subtotal := horiz_row * 100;
      elsif vert_match_found and then vert_inconsistencies = 1
      then
         subtotal := vert_row;
      else
         subtotal := 0;
         no_match := True;
      end if;

      if horiz_match_found and then horiz_inconsistencies = 1 and then vert_match_found and then vert_inconsistencies = 1
      then
         Ada.Text_IO.Put_Line("dual ");
      end if;

   end Get_Input;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;

      subtotal : Integer;
      running_total : Long_Integer := 0;

   begin

      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day13_2023.txt");

      while not Ada.Text_IO.End_Of_File (Input_File) loop

         Get_Input (Input_File,
                    subtotal);

        running_total := running_total + Long_Integer(subtotal);

         Ada.Text_IO.Put_Line("subtotal " & subtotal'Img & " " & running_total'Img);

       end loop;

      Ada.Text_IO.Put_Line("total " & running_total'Img);

      Ada.Text_IO.Close (Input_File);

   end run;

end day13_2023;
