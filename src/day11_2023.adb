with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with GNAT.Regpat;
with GNAT.Lists;

with Ada.Unchecked_Deallocation;

with Ada.Containers.Vectors;

package body day11_2023 is

   package Empty_Rows_Vector is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Long_Long_Integer);
   package Empty_Col_Vector is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Long_Long_Integer);

   type galaxy_t is
      record
         galaxy_id     : Natural;
         x_pos         : Long_Long_Integer;
         y_pos         : Long_Long_Integer;
         new_x_pos     : Long_Long_Integer;
         new_y_pos     : Long_Long_Integer;
         shortest_path : Natural;
      end record;

   type galaxy_access_t is access all galaxy_t;

   procedure deallocate is new Ada.Unchecked_Deallocation (galaxy_t, galaxy_access_t);

   package linked_list is

      function "="
        (Left  : galaxy_access_t;
         Right : galaxy_access_t) return Boolean;

      procedure Destroy_Element (Elem : in out galaxy_access_t);

   end linked_list;

   package body linked_list is

      function "="
        (Left  : galaxy_access_t;
         Right : galaxy_access_t) return Boolean
      is
      begin

         return Left.all = Right.all;

      end "=";

      procedure Destroy_Element (Elem : in out galaxy_access_t)
      is
      begin

         deallocate (Elem);

         Elem := null;

      end Destroy_Element;

   end linked_list;

   package galaxy_list is new GNAT.Lists.Doubly_Linked_Lists
     (Element_Type    => galaxy_access_t,
      "="             => linked_list."=",
      Destroy_Element => linked_list.Destroy_Element);

   Re_Int : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("(.|#)");

   Empty_Rows : Empty_Rows_Vector.Vector;
   Empty_Cols : Empty_Col_Vector.Vector;

   procedure Get_Input (S : Ada.Text_IO.File_Type;
                        row : Integer;
                        galaxy_id : in out Integer;
                        galaxies : galaxy_list.Doubly_Linked_List)
   is
      use type GNAT.Regpat.Match_Location;

      Matches : GNAT.Regpat.Match_Array (0 .. 1);

      current : Integer := 0;

      new_galaxy : galaxy_access_t;

   begin

      declare
         USV : constant Ada.Strings.Unbounded.Unbounded_String :=
           Ada.Strings.Unbounded.Text_IO.Get_Line (S);

         SV : constant String := Ada.Strings.Unbounded.To_String (USV);

      begin

         Ada.Text_IO.Put_Line ("new row : " & SV);

         Empty_Rows_Vector.Append (Empty_Rows,  Long_Long_Integer (row));

         if row = 1
         then
            for x in SV'Range
            loop
               Empty_Col_Vector.Append (Empty_Cols,  Long_Long_Integer (x));
            end loop;
         end if;

         match_loop :
         loop
            GNAT.Regpat.Match (Re_Int, SV, Matches, current);

            exit match_loop when Matches (0) = GNAT.Regpat.No_Match;

            if SV (Matches (1).First) = '#'
            then
               Empty_Col_Vector.Replace_Element
                 (Container => Empty_Cols,
                  Index     => Matches (1).First,
                  New_Item  => 0);

               Empty_Rows_Vector.Replace_Element
                 (Container => Empty_Rows,
                  Index     => row,
                  New_Item  => 0);

               new_galaxy := new galaxy_t'(galaxy_id    => galaxy_id,
                                           x_pos         =>  Long_Long_Integer (Matches (1).First),
                                           y_pos         =>  Long_Long_Integer (row),
                                           new_x_pos     =>  Long_Long_Integer (Matches (1).First),
                                           new_y_pos     =>  Long_Long_Integer (row),
                                           shortest_path => 0);

               galaxy_id := galaxy_id + 1;

               galaxy_list.Append (galaxies, new_galaxy);
            end if;

            current := Matches (0).Last + 1;

         end loop match_loop;
      end;

   end Get_Input;

   procedure expand_rows
     (galaxies : galaxy_list.Doubly_Linked_List;
     expansion : Long_Long_Integer)
   is

      row : Long_Long_Integer;

      iter : galaxy_list.Iterator;

      galaxy_access : galaxy_access_t;

      procedure row_iterator (cursor : Empty_Rows_Vector.Cursor)
      is
      begin

         row := Empty_Rows_Vector.Element (cursor);

         if row /= 0
         then
            iter := galaxy_list.Iterate (galaxies);

            while galaxy_list.Has_Next (iter)
            loop
               galaxy_list.Next (iter, galaxy_access);

               if galaxy_access.y_pos > row
               then
                  galaxy_access.new_y_pos := galaxy_access.new_y_pos + expansion - 1;
               end if;
            end loop;
         end if;

      end row_iterator;

   begin

      Empty_Rows_Vector.Iterate (Empty_Rows, row_iterator'Access);

   end expand_rows;

   procedure expand_cols
     (galaxies : galaxy_list.Doubly_Linked_List;
     expansion : Long_Long_Integer)
   is

      col : Long_Long_Integer;

      iter : galaxy_list.Iterator;

      galaxy_access : galaxy_access_t;

      procedure col_iterator (cursor : Empty_Col_Vector.Cursor)
      is
      begin

         col := Empty_Col_Vector.Element (cursor);

         if col /= 0
         then
            iter := galaxy_list.Iterate (galaxies);

            while galaxy_list.Has_Next (iter)
            loop
               galaxy_list.Next (iter, galaxy_access);

               if galaxy_access.x_pos > col
               then
                  galaxy_access.new_x_pos := galaxy_access.new_x_pos + expansion - 1;
               end if;
            end loop;
         end if;

      end col_iterator;

   begin

      Empty_Col_Vector.Iterate (Empty_Cols, col_iterator'Access);

   end expand_cols;

   function calc_distances
     (galaxies : galaxy_list.Doubly_Linked_List)
     return Long_Long_Integer
   is

      sum : Long_Long_Integer := 0;

      iter1 : galaxy_list.Iterator;
      iter2 : galaxy_list.Iterator;

      galaxy_access : galaxy_access_t;
      galaxy_access2 : galaxy_access_t;

   begin

      iter1 := galaxy_list.Iterate (galaxies);

      while galaxy_list.Has_Next (iter1)
      loop
         galaxy_list.Next (iter1, galaxy_access);

         Ada.Text_IO.Put_Line ("outer " & galaxy_access.galaxy_id'Img);

         exit when galaxy_access = galaxy_list.Last (galaxies);

         iter2 := galaxy_list.Iterate (galaxies);

         if galaxy_list.Has_Next (iter2)
         then
            galaxy_list.Next (iter2, galaxy_access2);

            while galaxy_access2 /= galaxy_access
            loop
               galaxy_list.Next (iter2, galaxy_access2);
            end loop;

            while galaxy_list.Has_Next (iter2)
            loop
               galaxy_list.Next (iter2, galaxy_access2);

               Ada.Text_IO.Put_Line ("inner " & galaxy_access2.galaxy_id'Img);
               sum := sum +  Long_Long_Integer (abs (galaxy_access.new_y_pos - galaxy_access2.new_y_pos) +
               abs (galaxy_access.new_x_pos - galaxy_access2.new_x_pos));
            end loop;
         end if;
      end loop;

      return sum;

   end calc_distances;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;

      current_row : Integer := 0;

      galaxy_id : Integer := 0;

      list : constant galaxy_list.Doubly_Linked_List := galaxy_list.Create;

      total : Long_Long_Integer;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day11_2023.txt");

      while not Ada.Text_IO.End_Of_File (Input_File)
      loop

         current_row := current_row + 1;

         Get_Input (Input_File,
                    current_row,
                    galaxy_id,
                    list);

      end loop;

      Ada.Text_IO.Put_Line ("expand_rows");
      expand_rows (list, 1e6);
      Ada.Text_IO.Put_Line ("expand_cols");
      expand_cols (list, 1e6);

      Ada.Text_IO.Put_Line ("calc_distances");
      total := calc_distances (list);

      Ada.Text_IO.Put_Line ("Total " & total'Img);

      Ada.Text_IO.Close (Input_File);

   end run;

end day11_2023;
