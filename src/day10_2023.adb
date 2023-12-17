with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

package body day10_2023 is

   type pipe_enum_t is (start,
                        vertical,
                        horizontal,
                        ninety_ne,
                        ninety_nw,
                        ninety_sw,
                        ninety_se,
                        ground);

   type movement_t is (n, s, e, w);

   type coord_t is
      record
         north : Integer;
         east : Integer;
      end record;

   type pipe_t;

   type pipe_t is
      record
         pipe_char : Character;
         pipe_enum : pipe_enum_t;
         path_1_count : Integer;
         path_2_count : Integer;
         my_coord : coord_t;
         next_coord_path_1 : coord_t;
         next_coord_path_2 : coord_t;
         pragma Warnings (Off, "literal * is not referenced", Reason => "TBD");
      end record;

   type char_fill_t is ('I', 'O', '#', 'U', 'P');
   pragma Warnings (On, "literal * is not referenced");

   type land_t is array (Integer range 1 .. 142, Integer range 1 .. 142) of pipe_t;
   type land_expanded_t is array (Integer range 1 .. 142 * 3, Integer range 1 .. 142 * 3) of char_fill_t;

   null_pipe : constant pipe_t := (pipe_char => '.',
                                   pipe_enum => ground,
                                   path_1_count => 0,
                                   path_2_count => 0,
                                   my_coord => (0, 0),
                                   next_coord_path_1 => (0, 0),
                                   next_coord_path_2 => (0, 0));

   land : land_t := (others => (others => null_pipe));
   land_expanded : land_expanded_t := (others => (others => 'U'));

   start_point : coord_t;

   procedure Get_Input (S : Ada.Text_IO.File_Type;
                        file_row : Integer)
   is
      USV : constant Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Text_IO.Get_Line (S);

      SV : constant String := Ada.Strings.Unbounded.To_String (USV);

      row : constant Integer := file_row + 1;

      x : Integer;

   begin

      for input in Integer range 1 .. Integer (SV'Last)
      loop
         x := input + 1;

         land (row, x) := (pipe_char => SV (input),
                          pipe_enum => start,
                          path_1_count => 0,
                          path_2_count => 0,
                          my_coord => (row, x),
                          next_coord_path_1 => (0, 0),
                          next_coord_path_2 => (0, 0));

         if SV (input) = 'S'
         then
            start_point := (row, x);
         end if;

         if SV (input) = 'S'
         then
            land (row, x).pipe_enum := start;
            land_expanded (row * 3 + 2, x * 3 + 1) := 'P';
            land_expanded (row * 3 + 2, x * 3 + 2) := 'P';
            land_expanded (row * 3 + 2, x * 3 + 3) := 'P';
            land_expanded (row * 3 + 1, x * 3 + 2) := 'P';
            land_expanded (row * 3 + 3, x * 3 + 2) := 'P';
         elsif SV (input) = '|'
         then
            land (row, x).pipe_enum := vertical;
            land_expanded (row * 3 + 1, x * 3 + 2) := '#';
            land_expanded (row * 3 + 2, x * 3 + 2) := '#';
            land_expanded (row * 3 + 3, x * 3 + 2) := '#';
         elsif SV (input) = '-'
         then
            land (row, x).pipe_enum := horizontal;
            land_expanded (row * 3 + 2, x * 3 + 1) := '#';
            land_expanded (row * 3 + 2, x * 3 + 2) := '#';
            land_expanded (row * 3 + 2, x * 3 + 3) := '#';
         elsif SV (input) = 'L'
         then
            land (row, x).pipe_enum := ninety_ne;
            land_expanded (row * 3 + 1, x * 3 + 2) := '#';
            land_expanded (row * 3 + 2, x * 3 + 2) := '#';
            land_expanded (row * 3 + 2, x * 3 + 3) := '#';
         elsif SV (input) = 'J'
         then
            land (row, x).pipe_enum :=  ninety_nw;
            land_expanded (row * 3 + 1, x * 3 + 2) := '#';
            land_expanded (row * 3 + 2, x * 3 + 2) := '#';
            land_expanded (row * 3 + 2, x * 3 + 1) := '#';
         elsif SV (input) = '7'
         then
            land (row, x).pipe_enum :=  ninety_sw;
            land_expanded (row * 3 + 2, x * 3 + 1) := '#';
            land_expanded (row * 3 + 2, x * 3 + 2) := '#';
            land_expanded (row * 3 + 3, x * 3 + 2) := '#';
         elsif SV (input) = 'F'
         then
            land (row, x).pipe_enum :=  ninety_se;
            land_expanded (row * 3 + 3, x * 3 + 2) := '#';
            land_expanded (row * 3 + 2, x * 3 + 2) := '#';
            land_expanded (row * 3 + 2, x * 3 + 3) := '#';
         elsif SV (input) = '.'
         then
            land (row, x).pipe_enum :=  ground;
         end if;
      end loop;

   end Get_Input;

   procedure flood_fill (north : Integer; east : Integer)
   is
   begin

      if land_expanded (north, east) = 'U'
      then
         land_expanded (north, east) := 'O';

         if east /= land_expanded'Last (2)
         then
            flood_fill (north, east + 1);
         end if;
         if east /= land_expanded'First (2)
         then
            flood_fill (north, east - 1);
         end if;
         if north /= land_expanded'Last (1)
         then
            flood_fill (north + 1, east);
         end if;
         if north /= land_expanded'First (1)
         then
            flood_fill (north - 1, east);
         end if;
      end if;

   end flood_fill;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;

      current_row : Integer := 0;

      path_count : Integer := 1;

      current_coord_path_1 : coord_t := start_point;
      current_coord_path_2 : coord_t := start_point;

      returning : Boolean := False;

      movement_1 : movement_t;
      movement_2 : movement_t;

      inside : Integer := 0;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day10_2023.txt");

      while not Ada.Text_IO.End_Of_File (Input_File) loop

         current_row := current_row + 1;

         Get_Input (Input_File,
                    current_row);

      end loop;

      --  find the 2 paths from the starting point
      if start_point.north /= land'First (1)
      then
         if land (start_point.north - 1, start_point.east).pipe_char /= '.' and then
           land (start_point.north - 1, start_point.east).pipe_char /= '-' and then
           land (start_point.north - 1, start_point.east).pipe_char /= 'L' and then
           land (start_point.north - 1, start_point.east).pipe_char /= 'J'
         then
            current_coord_path_1 := (start_point.north - 1, start_point.east);
            movement_1 := n;
         end if;
      end if;

      if start_point.north /= land'Last (1)
      then
         if land (start_point.north + 1, start_point.east).pipe_char /= '.' and then
           land (start_point.north + 1, start_point.east).pipe_char /= '-' and then
           land (start_point.north + 1, start_point.east).pipe_char /= 'F' and then
           land (start_point.north + 1, start_point.east).pipe_char /= '7'
         then
            if current_coord_path_1 = (0, 0)
            then
               current_coord_path_1 := (start_point.north + 1, start_point.east);
               movement_1 := s;
            else
               current_coord_path_2 := (start_point.north + 1, start_point.east);
               movement_2 := s;
            end if;
         end if;
      end if;

      if start_point.east /= land'First (2)
      then
         if land (start_point.north, start_point.east - 1).pipe_char /= '.' and then
           land (start_point.north, start_point.east - 1).pipe_char /= '|' and then
           land (start_point.north, start_point.east - 1).pipe_char /= 'J' and then
           land (start_point.north, start_point.east - 1).pipe_char /= '7'
         then
            if current_coord_path_1 = (0, 0)
            then
               current_coord_path_1 := (start_point.north, start_point.east - 1);
               movement_1 := w;
            else
               current_coord_path_2 := (start_point.north, start_point.east - 1);
               movement_2 := w;
            end if;
         end if;
      end if;

      if start_point.east /= land'Last (2)
      then
         if land (start_point.north, start_point.east + 1).pipe_char /= '.' and then
           land (start_point.north, start_point.east + 1).pipe_char /= '|' and then
           land (start_point.north, start_point.east + 1).pipe_char /= 'F' and then
           land (start_point.north, start_point.east + 1).pipe_char /= 'L'
         then
            if current_coord_path_1 = (0, 0)
            then
               current_coord_path_1 := (start_point.north, start_point.east + 1);
               movement_1 := e;
            else
               current_coord_path_2 := (start_point.north, start_point.east + 1);
               movement_2 := e;
            end if;
         end if;
      end if;

      while not returning
      loop

         path_count := path_count + 1;

         Ada.Text_IO.Put_Line ("current_coord_path_1 " & movement_1'Img & " " & land (current_coord_path_1.north, current_coord_path_1.east).pipe_enum'Img & " " & current_coord_path_1.north'Img & " " & current_coord_path_1.east'Img);
         Ada.Text_IO.Put_Line ("current_coord_path_2 " & movement_2'Img & " " & land (current_coord_path_2.north, current_coord_path_2.east).pipe_enum'Img & " " & current_coord_path_2.north'Img & " " & current_coord_path_2.east'Img);

         case land (current_coord_path_1.north, current_coord_path_1.east).pipe_enum is
            when start =>
               returning := True;
            when vertical =>
               land_expanded (current_coord_path_1.north * 3 + 1, current_coord_path_1.east * 3 + 2) := 'P';
               land_expanded (current_coord_path_1.north * 3 + 2, current_coord_path_1.east * 3 + 2) := 'P';
               land_expanded (current_coord_path_1.north * 3 + 3, current_coord_path_1.east * 3 + 2) := 'P';
               if movement_1 = n
               then
                  current_coord_path_1.north := current_coord_path_1.north - 1;
               elsif movement_1 = s
               then
                  current_coord_path_1.north := current_coord_path_1.north + 1;
               end if;
            when horizontal =>
               land_expanded (current_coord_path_1.north * 3 + 2, current_coord_path_1.east * 3 + 1) := 'P';
               land_expanded (current_coord_path_1.north * 3 + 2, current_coord_path_1.east * 3 + 2) := 'P';
               land_expanded (current_coord_path_1.north * 3 + 2, current_coord_path_1.east * 3 + 3) := 'P';
               if movement_1 = e
               then
                  current_coord_path_1.east := current_coord_path_1.east + 1;
               elsif movement_1 = w
               then
                  current_coord_path_1.east := current_coord_path_1.east - 1;
               end if;
            when ninety_ne =>
               land_expanded (current_coord_path_1.north * 3 + 1, current_coord_path_1.east * 3 + 2) := 'P';
               land_expanded (current_coord_path_1.north * 3 + 2, current_coord_path_1.east * 3 + 2) := 'P';
               land_expanded (current_coord_path_1.north * 3 + 2, current_coord_path_1.east * 3 + 3) := 'P';
               if movement_1 = s
               then
                  current_coord_path_1.east := current_coord_path_1.east + 1;
                  movement_1 := e;
               elsif movement_1 = w
               then
                  current_coord_path_1.north := current_coord_path_1.north - 1;
                  movement_1 := n;
               end if;
            when ninety_nw =>
               land_expanded (current_coord_path_1.north * 3 + 1, current_coord_path_1.east * 3 + 2) := 'P';
               land_expanded (current_coord_path_1.north * 3 + 2, current_coord_path_1.east * 3 + 2) := 'P';
               land_expanded (current_coord_path_1.north * 3 + 2, current_coord_path_1.east * 3 + 1) := 'P';
               if movement_1 = s
               then
                  current_coord_path_1.east := current_coord_path_1.east - 1;
                  movement_1 := w;
               elsif movement_1 = e
               then
                  current_coord_path_1.north := current_coord_path_1.north - 1;
                  movement_1 := n;
               end if;
            when ninety_sw =>
               land_expanded (current_coord_path_1.north * 3 + 2, current_coord_path_1.east * 3 + 1) := 'P';
               land_expanded (current_coord_path_1.north * 3 + 2, current_coord_path_1.east * 3 + 2) := 'P';
               land_expanded (current_coord_path_1.north * 3 + 3, current_coord_path_1.east * 3 + 2) := 'P';
               if movement_1 = n
               then
                  current_coord_path_1.east := current_coord_path_1.east - 1;
                  movement_1 := w;
               elsif movement_1 = e
               then
                  current_coord_path_1.north := current_coord_path_1.north + 1;
                  movement_1 := s;
               end if;
            when ninety_se =>
               land_expanded (current_coord_path_1.north * 3 + 3, current_coord_path_1.east * 3 + 2) := 'P';
               land_expanded (current_coord_path_1.north * 3 + 2, current_coord_path_1.east * 3 + 2) := 'P';
               land_expanded (current_coord_path_1.north * 3 + 2, current_coord_path_1.east * 3 + 3) := 'P';
               if movement_1 = n
               then
                  current_coord_path_1.east := current_coord_path_1.east + 1;
                  movement_1 := e;
               elsif movement_1 = w
               then
                  current_coord_path_1.north := current_coord_path_1.north + 1;
                  movement_1 := s;
               end if;
            when ground =>
               returning := True;
         end case;

         case land (current_coord_path_2.north, current_coord_path_2.east).pipe_enum is
            when start =>
               returning := True;
            when vertical =>
               if movement_2 = n
               then
                  current_coord_path_2.north := current_coord_path_2.north - 1;
               elsif movement_2 = s
               then
                  current_coord_path_2.north := current_coord_path_2.north + 1;
               end if;
            when horizontal =>
               if movement_2 = e
               then
                  current_coord_path_2.east := current_coord_path_2.east + 1;
               elsif movement_2 = w
               then
                  current_coord_path_2.east := current_coord_path_2.east - 1;
               end if;
            when ninety_ne =>
               if movement_2 = s
               then
                  current_coord_path_2.east := current_coord_path_2.east + 1;
                  movement_2 := e;
               elsif movement_2 = w
               then
                  current_coord_path_2.north := current_coord_path_2.north - 1;
                  movement_2 := n;
               end if;
            when ninety_nw =>
               if movement_2 = s
               then
                  current_coord_path_2.east := current_coord_path_2.east - 1;
                  movement_2 := w;
               elsif movement_2 = e
               then
                  current_coord_path_2.north := current_coord_path_2.north - 1;
                  movement_2 := n;
               end if;
            when ninety_sw =>
               if movement_2 = n
               then
                  current_coord_path_2.east := current_coord_path_2.east - 1;
                  movement_2 := w;
               elsif movement_2 = e
               then
                  current_coord_path_2.north := current_coord_path_2.north + 1;
                  movement_2 := s;
               end if;
            when ninety_se =>
               if movement_2 = n
               then
                  current_coord_path_2.east := current_coord_path_2.east + 1;
                  movement_2 := e;
               elsif movement_2 = w
               then
                  current_coord_path_2.north := current_coord_path_2.north + 1;
                  movement_2 := s;
               end if;
            when ground =>
               returning := True;
         end case;

         if not returning and then current_coord_path_1 = current_coord_path_2
         then
            returning := True;
         end if;

      end loop;

      for x in 4 .. land_expanded'Last (1) / 4 - 3
      loop
         for y in 4 .. land_expanded'Last (2) / 4 - 3
         loop
            Ada.Text_IO. Put (land_expanded (x, y)'Img (2));
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
               Ada.Text_IO.New_Line;

               Ada.Text_IO.New_Line;

               Ada.Text_IO.New_Line;

               Ada.Text_IO.New_Line;

      flood_fill (1, 1);

            for x in land_expanded'range(1)
      loop
         for y in land_expanded'range(2)
         loop
            if land_expanded (x, y) = '#'
            then
               land_expanded (x, y) := 'U';
            end if;
         end loop;
      end loop;

      for x in 4 .. land_expanded'Last (1) / 4 - 3
      loop
         for y in 4 .. land_expanded'Last (2) / 4 - 3
         loop
            Ada.Text_IO. Put (land_expanded (x, y)'Img(2));
         end loop;
         Ada.Text_IO.New_Line;
      end loop;

      for x in 2 .. land'Last (1) - 1
      loop
         for y in 2 .. land'Last (2) - 1
         loop
            if land_expanded (x * 3 + 1, y * 3 + 1) = 'U' and then land_expanded (x * 3 + 1, y * 3 + 2) = 'U' and then land_expanded (x * 3 + 1, y * 3 + 3) = 'U' and then
              land_expanded (x * 3 + 2, y * 3 + 1) = 'U' and then land_expanded (x * 3 + 2, y * 3 + 2) = 'U' and then land_expanded (x * 3 + 2, y * 3 + 3) = 'U' and then
              land_expanded (x * 3 + 3, y * 3 + 1) = 'U' and then land_expanded (x * 3 + 3, y * 3 + 2) = 'U' and then land_expanded (x * 3 + 3, y * 3 + 3) = 'U'
            then
               inside := inside + 1;
            end if;
         end loop;
      end loop;

      Ada.Text_IO.Put_Line ("path_count " & path_count'Img & " Inside " & inside'Img);

      Ada.Text_IO.Close (Input_File);

   end run;

end day10_2023;
