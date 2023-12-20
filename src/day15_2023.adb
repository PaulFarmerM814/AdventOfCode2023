with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with GNAT.Regpat;

package body day15_2023 is

   Re_Hash_id      : constant GNAT.Regpat.Pattern_Matcher :=
   GNAT.Regpat.Compile ("([[:alpha:]]+)([=-])+([[:digit:]]*)");

   type lens_entry_t is
      record
         lens_label : Ada.Strings.Unbounded.Unbounded_String;
         focal_length : integer;
      end record;

   type lens_entry_array_t is array(1..255) of lens_entry_t;

   type box_entry_t is
      record
         lens_count : natural := 0;
         lenses : lens_entry_array_t;
      end record;

   type boxes_t is array(0..255) of box_entry_t;

   boxes  : boxes_t;

   procedure Get_Input (S : Ada.Text_IO.File_Type;
                        row : Integer;
                        current_hash_total : out Long_Long_Integer)
   is
      use type GNAT.Regpat.Match_Location;

      USV : constant Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Text_IO.Get_Line (S);

      SV : constant String := Ada.Strings.Unbounded.To_String (USV);

      Matches : GNAT.Regpat.Match_Array (0 .. 4);

      current : Integer := 1;

      local_hash_total : Integer;

      found : Boolean;

      current_label : Ada.Strings.Unbounded.Unbounded_String;

      focal_length : Integer;

   begin

      --  Determine the ASCII code for the current character of the string.
      --  Increase the current value by the ASCII code you just determined.
      --  Set the current value to itself multiplied by 17.
      --  Set the current value to the remainder of dividing itself by 256.

      current_hash_total := 0;

      hash_loop :
      loop
         GNAT.Regpat.Match (Re_Hash_id, SV, Matches, current);

         exit hash_loop when Matches (0) = GNAT.Regpat.No_Match;

         local_hash_total := 0;

         for current_hash in Matches(1).first..Matches(1).last
         loop
            local_hash_total := local_hash_total + Character'Enum_Rep(SV(current_hash));
            local_hash_total := (local_hash_total * 17) mod 256;
         end loop;

         current_label := Ada.Strings.Unbounded.Unbounded_Slice(USV,Matches(1).first,Matches(1).last);

         if SV(Matches(2).first) = '='
         then

            focal_length := Integer'Value(SV(Matches(3).first..Matches(3).last));

            if boxes(local_hash_total).lens_count = 0
            then
               boxes(local_hash_total).lens_count := 1;
               boxes(local_hash_total).lenses(boxes(local_hash_total).lens_count) :=
                 (lens_label => current_label,
                  focal_length => focal_length);
            else

               found := False;

               for x in 1..boxes(local_hash_total).lens_count
               loop
                  if boxes(local_hash_total).lenses(x).lens_label = current_label
                  then
                     found := True;
                     boxes(local_hash_total).lenses(x) :=
                       (lens_label => current_label,
                        focal_length => focal_length);
                     exit;
                  end if;
               end loop;
               if not found
               then
                  boxes(local_hash_total).lens_count := boxes(local_hash_total).lens_count + 1;
                  boxes(local_hash_total).lenses(boxes(local_hash_total).lens_count) :=
                    (lens_label => current_label,
                     focal_length => focal_length);
               end if;
            end if;
         else
            for x in 1..boxes(local_hash_total).lens_count
            loop
               if boxes(local_hash_total).lenses(x).lens_label = current_label
               then
                  if x /= boxes(local_hash_total).lens_count
                  then
                     boxes(local_hash_total).lenses(x .. boxes(local_hash_total).lens_count - 1) :=
                       boxes(local_hash_total).lenses(x + 1 .. boxes(local_hash_total).lens_count);
                  end if;
                  boxes(local_hash_total).lens_count := boxes(local_hash_total).lens_count - 1;
                  exit;
               end if;
            end loop;
         end if;

         current := Matches (0).Last + 1;

      end loop hash_loop;

      for x in boxes'Range
      loop
         for y in 1..boxes(x).lens_count
         loop
            current_hash_total := current_hash_total + Long_Long_Integer(((x + 1) * y * boxes(x).lenses(y).focal_length));
         end loop;
      end loop;

   end Get_Input;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;

      hash_total : Long_Long_Integer;

      current_row : integer := 0;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day15_2023.txt");

      while not Ada.Text_IO.End_Of_File (Input_File) loop

         current_row := current_row + 1;

         Get_Input (Input_File,
                    current_row,
                    hash_total);

      end loop;

      Ada.Text_IO.Put_Line ("total " & hash_total'Image);

      Ada.Text_IO.Close (Input_File);

   end run;

end day15_2023;
