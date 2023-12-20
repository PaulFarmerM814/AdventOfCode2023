with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with GNAT.Regpat;
with GNAT.Lists;

with Ada.Containers.Vectors;

package body day12_2023 is

   package Spring_Vector is new Ada.Containers.Vectors (Positive, Long_Integer);

   package linked_list is

      function "="
        (Left  : Ada.Strings.Unbounded.Unbounded_String;
         Right : Ada.Strings.Unbounded.Unbounded_String) return Boolean;

      procedure Destroy_Element (Elem : in out Ada.Strings.Unbounded.Unbounded_String);

   end linked_list;

   package body linked_list is

      function "="
        (Left  : Ada.Strings.Unbounded.Unbounded_String;
         Right : Ada.Strings.Unbounded.Unbounded_String) return Boolean
      is
      begin

         return ada.strings.unbounded."="(Left, Right);

      end "=";

      procedure Destroy_Element (Elem : in out Ada.Strings.Unbounded.Unbounded_String)
      is
      begin

         null;

      end Destroy_Element;

   end linked_list;

   package spring_list is new GNAT.Lists.Doubly_Linked_Lists
     (Element_Type    => Ada.Strings.Unbounded.Unbounded_String,
      "="             => linked_list."=",
      Destroy_Element => linked_list.Destroy_Element);

   Re_Spring : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("[#|\.|\?]+");

   Re_Int : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("([[:digit:]]+)");

   procedure parse_springs (springs : String;
                          values : Spring_Vector.Vector;
                           alternatives : in out Long_Long_Integer; --  spring_list.Doubly_Linked_List;
                          current_element : in Integer)
   is

      use type Spring_Vector.Cursor;
      use type ada.Containers.count_type;
      use Spring_Vector;

      result : Boolean := True;

      continue : Boolean := True;

      altered_springs : String := springs;

      corrupt_found : Boolean := False;

      contiguous_found_count : Long_Integer := 0;

      spring_count : Integer := 0;

      cursor : Spring_Vector.Cursor := Spring_Vector.First (values);
      spring_vector_len : constant ada.Containers.count_type := Spring_Vector.Length (values);

      function parse (springs : String) return Boolean
      is

         cursor : Spring_Vector.Cursor := Spring_Vector.First (values);
         contiguous_found_count : Long_Integer := 0;

         spring_count : Integer := 0;

      begin

         for y in springs'Range
         loop
            if springs(y) = '#'
            then
               if contiguous_found_count = 0
               then
                  spring_count := spring_count + 1;
               end if;

               contiguous_found_count := contiguous_found_count + 1;

               if cursor = Spring_Vector.No_Element
                 or else
                   contiguous_found_count > Spring_Vector.Element (cursor)
               then
                  return False;
               end if;

            elsif springs(y) = '.'
            then
               if contiguous_found_count /= 0 and then
                 cursor /= Spring_Vector.No_Element and then
                 contiguous_found_count /= Spring_Vector.Element (cursor)
               then
                  return False;
               elsif contiguous_found_count /= 0
               then
                  Spring_Vector.Next (cursor);
               end if;
               contiguous_found_count := 0;
            else
               return True;
            end if;

            if ada.Containers.count_type(spring_count) > spring_vector_len
            then
               return False;
            end if;
         end loop;

         return true;

      end parse;

   begin

      scan_loop:
      for x in current_element + 1 .. altered_springs'Length
      loop
         if altered_springs(x) = '?'
         then

            --  check if failed up to this point,
            --  no point in parsing further
            result := true;

            altered_springs(x) := '#';
            continue := parse(altered_springs(1..x));
            if continue
            then
               --  result :=
               parse_springs (altered_springs, values, alternatives, x);
            end if;

            altered_springs(x) := '.';
            continue := parse(altered_springs(1..x));
            if continue
            then
               --  result :=
               parse_springs (altered_springs, values, alternatives, x);
            end if;

            corrupt_found := True;
            exit scan_loop;
         end if;
      end loop scan_loop;

      if not corrupt_found
      then
         --  ada.Text_IO.Put_Line(altered_springs);
         spring_count := 0;
         result := true;

         for x in altered_springs'Range
         loop
            if altered_springs(x) = '#'
            then
               if contiguous_found_count = 0
               then
                  spring_count := spring_count + 1;
               end if;

               contiguous_found_count := contiguous_found_count + 1;

               if cursor = Spring_Vector.No_Element
                 or else
                   contiguous_found_count > Spring_Vector.Element (cursor)
                 or else
                   (x = altered_springs'last
                      and then
                    contiguous_found_count /= Spring_Vector.Element (cursor))
               then
                  result := False;
                  --  ada.Text_IO.Put_Line("xxx1" & altered_springs);

               end if;
            else
               if contiguous_found_count /= 0 and then
                 cursor /= Spring_Vector.No_Element and then
                 contiguous_found_count /= Spring_Vector.Element (cursor)
               then
                  result := False;
                  --  ada.Text_IO.Put_Line("xxx2" & altered_springs);

                  exit;
               elsif contiguous_found_count /= 0
               then
                  Spring_Vector.Next (cursor);
               end if;
               contiguous_found_count := 0;
            end if;

            if ada.Containers.count_type(spring_count) > spring_vector_len
            then
               result := false;
               --  ada.Text_IO.Put_Line("xxx3" & altered_springs);

               exit;
            end if;
         end loop;

         if result and then ada.Containers.count_type(spring_count) = spring_vector_len
         then
            --  .Text_IO.Put_Line("xxx" & altered_springs & alternatives'img);

            alternatives := alternatives + 1;
            --  spring_list.Append (alternatives, altered_springs);
         end if;

      end if;

      --  return result;

   end parse_springs;

   procedure Get_Input (S : Ada.Text_IO.File_Type;
                        row : Integer;
                       alternatives : out Long_Long_Integer)
   is
      pragma Unreferenced (row);
      use type GNAT.Regpat.Match_Location;

      use Spring_Vector;

      Matches : GNAT.Regpat.Match_Array (0 .. 1);

      current : Integer := 0;
      new_start : Integer := 0;

      count_vector : Spring_Vector.Vector;
      orig_count_vector : Spring_Vector.Vector;

      springs : constant spring_list.Doubly_Linked_List := spring_list.Create;

      result : Boolean;

      USV_Springs : Ada.Strings.Unbounded.Unbounded_String;
      Orig_USV_Springs : Ada.Strings.Unbounded.Unbounded_String;

      current_element : Integer;

   begin

      declare
         USV : constant Ada.Strings.Unbounded.Unbounded_String :=
           Ada.Strings.Unbounded.Text_IO.Get_Line (S);

         SV : constant String := Ada.Strings.Unbounded.To_String (USV);

      begin

         Ada.Text_IO.Put_Line ("new row : " & SV);

         match_loop :
         loop
            GNAT.Regpat.Match (Re_Spring, SV, Matches, current);

            exit match_loop when Matches (0) = GNAT.Regpat.No_Match;

            current := Matches (0).Last + 1;

            USV_Springs := Ada.Strings.Unbounded.Unbounded_Slice (USV, 1, Matches (0).Last);

            new_start := Matches (0).Last;

         end loop match_loop;

         current := new_start + 2;

         match_loop1 :
         loop
            GNAT.Regpat.Match (Re_Int, SV, Matches, current);

            exit match_loop1 when Matches (0) = GNAT.Regpat.No_Match;

            Spring_Vector. Append (count_vector, Long_Integer'Value(SV(Matches(1).First..Matches(1).Last)));
            current := Matches (0).Last + 1;

         end loop match_loop1;
      end;

      Orig_USV_Springs := USV_Springs;
      orig_count_vector := count_vector;

      ada.Text_IO.Put_Line("Appending");

      for x in 1..4
      loop
         Ada.Strings.Unbounded.append(USV_Springs, "?");
         Ada.Strings.Unbounded.append(USV_Springs, Orig_USV_Springs);

         count_vector := count_vector & orig_count_vector;
      end loop;

      ada.Text_IO.Put_Line("Appending finished " & ada.Strings.Unbounded.To_String(USV_Springs));

      --  result :=
      current_element := 0;
      --  parse_springs (USV_Springs, count_vector, springs, current_element);
      parse_springs (ada.Strings.Unbounded.To_String(USV_Springs), count_vector, alternatives, current_element);

      ada.Text_IO.Put_Line("row, alternatives " & row'Img & " " & alternatives'Img);

      --  alternatives := Long_Integer(spring_list.Size(springs));

   end Get_Input;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;

      current_row : Integer := 0;

      running_total : Long_Long_Integer := 0;

      alternatives : Long_Long_Integer := 0;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day12_2023.txt");

      while not Ada.Text_IO.End_Of_File (Input_File)
      loop

         current_row := current_row + 1;

         Get_Input (Input_File,
                    current_row,
                    alternatives);

         running_total := running_total + alternatives;

      end loop;

      Ada.Text_IO.Put_Line ("total " & running_total'Image);

      Ada.Text_IO.Close (Input_File);

   end run;

end day12_2023;
