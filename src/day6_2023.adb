with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with GNAT.Regpat;

package body day6_2023 is

   type map_t;

   type map_access_t is access all map_t;

   type map_t is
      record
         map_time     : Long_Long_Integer;
         map_distance : Long_Long_Integer;
         wins : Long_Long_Integer;
         next_map     : map_access_t;
      end record;

   map : map_access_t := null;

   race_index : Integer := 1;

   Re_Int : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("([[:digit:]]+)");

   procedure Get_Input (S : Ada.Text_IO.File_Type;
                        row : Integer)
   is
      use type GNAT.Regpat.Match_Location;

      Matches : GNAT.Regpat.Match_Array (0 .. 1);

      current : Integer := 6;

      new_map : map_access_t;
      last_map : map_access_t;

      whole_time : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;

      whole_distance : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;

   begin

      declare
         USV : constant Ada.Strings.Unbounded.Unbounded_String :=
           Ada.Strings.Unbounded.Text_IO.Get_Line (S);

         SV : constant String := Ada.Strings.Unbounded.To_String (USV);

      begin

         map := new map_t'(map_time => 0,
                           map_distance  => 0,
                           wins => 0,
                           next_map => null
                          );

         time_loop :
         loop
            GNAT.Regpat.Match (Re_Int, SV, Matches, current);

            exit time_loop when Matches (0) = GNAT.Regpat.No_Match;

            whole_time := whole_time & Ada.Strings.Unbounded.To_Unbounded_String (SV (Matches (1).First .. Matches (1).Last));

            Ada.Text_IO.Put_Line ("whole_time " & Ada.Strings.Unbounded.To_String (whole_time));

            current := Matches (0).Last + 1;

         end loop time_loop;
      end;

      map.map_time := Long_Long_Integer'Value (Ada.Strings.Unbounded.To_String (whole_time));

      current := 10;

      new_map := map;

      declare
         USV : constant Ada.Strings.Unbounded.Unbounded_String :=
           Ada.Strings.Unbounded.Text_IO.Get_Line (S);

         SV : constant String := Ada.Strings.Unbounded.To_String (USV);
      begin
         distance_loop :
         loop
            GNAT.Regpat.Match (Re_Int, SV, Matches, current);

            exit distance_loop when Matches (0) = GNAT.Regpat.No_Match;

            whole_distance := whole_distance & Ada.Strings.Unbounded.To_Unbounded_String (SV (Matches (1).First .. Matches (1).Last));

            Ada.Text_IO.Put_Line ("whole_distance " & Ada.Strings.Unbounded.To_String (whole_distance));

            current := Matches (0).Last + 1;

         end loop distance_loop;
      end;

      map.map_distance := Long_Long_Integer'Value (Ada.Strings.Unbounded.To_String (whole_distance));

   end Get_Input;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;

      current_row : Integer := 0;

      current_ptr : map_access_t;

      distance : Long_Long_Integer := 0;

      x : Integer := 1;

      all_records : Long_Long_Integer := 1;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day6_2023.txt");

      Get_Input (Input_File,
                 current_row);

      current_ptr := map;

      while current_ptr /= null
      loop

         --  brute force, but what the hell......
         Ada.Text_IO.Put_Line ("time,distance " & current_ptr.map_time'Img & "," & current_ptr.map_distance'Img);

         current_ptr.wins := 0;

         for x in 1 .. current_ptr.map_time
         loop
            distance := (current_ptr.map_time - x) * x;
            if distance > current_ptr.map_distance
            then
               current_ptr.wins := current_ptr.wins + 1;
            end if;
         end loop;

         current_ptr := current_ptr.next_map;

      end loop;

      current_ptr := map;

      while current_ptr /= null
      loop
         all_records := all_records * current_ptr.wins;

         current_ptr := current_ptr.next_map;
      end loop;

      Ada.Text_IO.Put_Line ("records " & all_records'Img);

      Ada.Text_IO.Close (Input_File);

   end run;

end day6_2023;
