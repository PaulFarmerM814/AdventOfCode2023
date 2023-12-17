with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with GNAT.Regpat;
with GNAT.Lists;

with Ada.Unchecked_Deallocation;

package body day9_2023 is

   type sensor_val_t is array (Natural range <>) of Long_Integer;

   type sensor_val_access_t is access all sensor_val_t;

   procedure deallocate is new Ada.Unchecked_Deallocation (sensor_val_t, sensor_val_access_t);

   package linked_list is

      function "="
        (Left  : sensor_val_access_t;
         Right : sensor_val_access_t) return Boolean;

      procedure Destroy_Element (Elem : in out sensor_val_access_t);

   end linked_list;

   package body linked_list is

      function "="
        (Left  : sensor_val_access_t;
         Right : sensor_val_access_t) return Boolean
      is
      begin

         return Left.all = Right.all;

      end "=";

      procedure Destroy_Element (Elem : in out sensor_val_access_t)
      is
      begin

         deallocate (Elem);

         Elem := null;

      end Destroy_Element;

   end linked_list;

   package value_list is new GNAT.Lists.Doubly_Linked_Lists
     (Element_Type    => sensor_val_access_t,
      "="             => linked_list."=",
      Destroy_Element => linked_list.Destroy_Element);

   Re_Int : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("([[:digit:]]+|-[[:digit:]]+)");

   procedure Get_Input (S : Ada.Text_IO.File_Type;
                        row : Integer;
                        increase : out Long_Integer)
   is
      pragma Unreferenced (row);
      use type GNAT.Regpat.Match_Location;

      Matches : GNAT.Regpat.Match_Array (0 .. 1);

      current : Integer := 0;

      value_count : Integer := 0;
      scan_value_count : Integer := 0;
      pragma Unreferenced (scan_value_count);

      list : constant value_list.Doubly_Linked_List := value_list.Create;

      iter : value_list.Iterator;

      val_array_access : sensor_val_access_t;
      prev_val_array_access : sensor_val_access_t;

      diff_non_zero : Boolean := True;

   begin

      declare
         USV : constant Ada.Strings.Unbounded.Unbounded_String :=
           Ada.Strings.Unbounded.Text_IO.Get_Line (S);

         SV : constant String := Ada.Strings.Unbounded.To_String (USV);

      begin

         Ada.Text_IO.Put_Line ("new row : " & SV);

         match_loop :
         loop
            GNAT.Regpat.Match (Re_Int, SV, Matches, current);

            exit match_loop when Matches (0) = GNAT.Regpat.No_Match;

            value_count := value_count + 1;

            current := Matches (0).Last + 1;

         end loop match_loop;

         val_array_access := new sensor_val_t (0 .. value_count + 1);

         value_list.Append (list, val_array_access);

         current := 0;
         value_count := 0;

         match_loop1 :
         loop
            GNAT.Regpat.Match (Re_Int, SV, Matches, current);

            exit match_loop1 when Matches (0) = GNAT.Regpat.No_Match;

            value_count := value_count + 1;

            val_array_access (value_count) := Long_Integer'Value (SV (Matches (1).First .. Matches (1).Last));

            current := Matches (0).Last + 1;

         end loop match_loop1;
      end;

      scan_value_count := 0;

      while diff_non_zero
      loop
         diff_non_zero := False;

         prev_val_array_access := value_list.First (list);
         val_array_access := new sensor_val_t (0 .. prev_val_array_access.all'Last - 1);

         value_list.Prepend (list,
                             val_array_access);

         for x in 2 .. prev_val_array_access.all'Last - 1
         loop
            val_array_access (x - 1) := prev_val_array_access (x) - prev_val_array_access (x - 1);

            if val_array_access (x - 1) /= 0
            then
               diff_non_zero := True;
            end if;

            Ada.Text_IO.Put_Line (val_array_access (x - 1)'Image);
         end loop;

         Ada.Text_IO.Put_Line ("new row");
      end loop;

      iter := value_list.Iterate (list);

      while value_list.Has_Next (iter)
      loop

         value_list.Next (iter, val_array_access);

         if value_list.First (list) = val_array_access
         then
            val_array_access.all (val_array_access.all'Last) := 0;
            val_array_access.all (val_array_access.all'First) := 0;
         else
            Ada.Text_IO.Put_Line
              (val_array_access.all (val_array_access.all'Last)'Img & " " &
                 val_array_access.all (val_array_access.all'Last - 1)'Img & " " &
                 prev_val_array_access.all (prev_val_array_access.all'Last)'Img & " ");

            val_array_access.all (val_array_access.all'Last) :=
              prev_val_array_access.all (prev_val_array_access.all'Last) +
              val_array_access.all (val_array_access.all'Last - 1);

            val_array_access.all (val_array_access.all'First) :=
              val_array_access.all (val_array_access.all'First + 1) -
              prev_val_array_access.all (prev_val_array_access.all'First);

            Ada.Text_IO.Put_Line
              (val_array_access.all (val_array_access.all'First)'Img & " " &
                 val_array_access.all (val_array_access.all'First + 1)'Img & " " &
                 prev_val_array_access.all (prev_val_array_access.all'First)'Img & " ");

         end if;

         prev_val_array_access := val_array_access;

      end loop;

      increase := value_list.Last (list).all (value_list.Last (list).all'First);

      Ada.Text_IO.Put_Line ("increase " & increase'Image);

   end Get_Input;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;

      current_row : Integer := 0;

      increase : Long_Integer := 0;
      running_total : Long_Integer := 0;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day9_2023.txt");

      while not Ada.Text_IO.End_Of_File (Input_File)
      loop

         current_row := current_row + 1;

         Get_Input (Input_File,
                    current_row,
                    increase);

         running_total := running_total + increase;

      end loop;

      Ada.Text_IO.Put_Line ("total " & running_total'Image);

      Ada.Text_IO.Close (Input_File);

   end run;

end day9_2023;
