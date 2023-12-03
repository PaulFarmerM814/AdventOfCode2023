with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with GNAT.Regpat;

package body day1_2023_pt2 is

   Re      : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("one|two|three|four|five|six|seven|eight|nine");

   type nums is (one,
                two,
                three,
                four,
                five,
                six,
                seven,
                eight,
                nine);
   for nums use (one => 1,
                 two => 2,
                 three => 3,
                 four => 4,
                 five => 5,
                 six => 6,
                 seven => 7,
                 eight => 8,
                 nine => 9);

   function Get_Input (S : Ada.Text_IO.File_Type)
                       return Integer
   is
      use type GNAT.Regpat.Match_Location;

      USV : constant Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Text_IO.Get_Line (S);

      SV : constant String := Ada.Strings.Unbounded.To_String (USV);

      Matches : GNAT.Regpat.Match_Array (0 .. 0);

      first_digit : Integer := 0;
      first_found : Boolean := False;
      first_found_pos : Integer := 0;
      second_digit : Integer := 0;
      second_found : Boolean := False;
      second_found_pos : Integer := 0;

      first_match_digit : Integer := 0;
      first_match_found : Boolean := False;
      first_match_found_pos : Integer := 0;
      second_match_digit : Integer := 0;
      second_match_found : Boolean := False;
      second_match_found_pos : Integer := 0;

      using_first_match : Boolean := False;
      using_second_match : Boolean := False;

      Current    : Natural := SV'First;

      ret_val : Integer;

   begin

      pattern_match :
      loop
         GNAT.Regpat.Match (Re, SV, Matches, Current);
         exit pattern_match when Matches (0) = GNAT.Regpat.No_Match;

         --  Process the match at position Matches (0).First
         if not first_match_found then
            first_match_found := True;
            first_match_found_pos := Matches (0).First;
            first_match_digit :=
              nums'Enum_Rep
                (nums'Value (SV (Matches (0).First .. Matches (0).Last)));
         end if;

         second_match_found := True;
         second_match_found_pos := Matches (0).First;
         second_match_digit :=
           nums'Enum_Rep
             (nums'Value (SV (Matches (0). First .. Matches (0).Last)));

         Current := Matches (0).Last;
      end loop pattern_match;

      string_scan :
      for X in SV'Range loop

         if SV (X) in '0' .. '9' then
            if not first_found then
               first_found := True;
               first_found_pos := X;
               first_digit := Integer'Value ("" & SV (X));
            end if;
         end if;

         if SV (SV'Length - X + 1) in '0' .. '9' then
            if not second_found then
               second_found := True;
               second_found_pos := SV'Length - X + 1;
               second_digit := Integer'Value ("" & SV (SV'Length - X + 1));
            end if;
         end if;

         exit string_scan when first_found and second_found;

      end loop string_scan;

      if first_match_found and then
        (not first_found or else
        first_found_pos > first_match_found_pos)
      then
         first_digit := first_match_digit;
         using_first_match := True;
      end if;

      if second_match_found and then
        (not second_found or else
        second_found_pos < second_match_found_pos)
      then
         second_digit := second_match_digit;
         using_second_match := True;
      end if;

      ret_val := (10 * first_digit) + second_digit;

      if using_first_match or using_second_match
      then
         Ada.Text_IO.Put_Line (Item => first_found_pos'Img & "   " & second_found_pos'img & "   " & first_match_found_pos'img & "   " & second_match_found_pos'img & "   ");
         Ada.Text_IO.Put_Line (Item => SV & "   " & ret_val'Img);
      end if;

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

end day1_2023_pt2;
