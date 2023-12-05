with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with GNAT.Regpat;

package body day4_2023 is

   Re_Game      : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile (".*:");
   Re_Game_id      : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("([[:alpha:]]* *)([[:digit:]]*)");

   Re_Game_Win       : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("([[:digit:]]+\s+)(\|)*");

   Re_Game_Scratched : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("([[:digit:]]+)");

   card_count : array (1 .. 201) of Integer := (others => 0);

   type win_repeat_t is array (1 .. 100) of Integer;

   procedure Get_Input (S : Ada.Text_IO.File_Type;
                        row : Integer;
                        winning_points : out Integer)
   is
      use type GNAT.Regpat.Match_Location;

      USV : constant Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Text_IO.Get_Line (S);

      SV : constant String := Ada.Strings.Unbounded.To_String (USV);

      Matches : GNAT.Regpat.Match_Array (0 .. 3);

      current_win_card : Integer;

      current : Integer := 0;

      power : Integer := -1;

      game : Integer;
      pragma Unreferenced (game);

      --  could create a string of just the winning numbers and then check
      --  for substring of the picked numbers, but might need to use the data
      --  so save it anyway
      winners : array (1 .. 99) of Boolean := (others => False);

   begin

      GNAT.Regpat.Match (Re_Game, SV, Matches, current);
      Ada.Text_IO.Put_Line (Item => SV (Matches (0).First .. Matches (0).Last));

      GNAT.Regpat.Match (Re_Game_id, SV, Matches, Matches (0).First, Matches (0).Last);

      game := Integer'Value (SV (Matches (2).First .. Matches (2).Last));

      current := Matches (2).Last + 1;

      game_win_pattern_match :
      loop
         GNAT.Regpat.Match (Re_Game_Win, SV, Matches, current);

         --  Ada.Text_IO.Put_Line (Item => Matches (1).Last'img & " " & Matches (2).First'img);
         Ada.Text_IO.Put_Line (Item => SV (Matches (1).First .. Matches (1).Last));

         current_win_card := Integer'Value (SV (Matches (1).First .. Matches (1).Last));

         winners (current_win_card) := True;

         current := Matches (0).Last + 1;

         exit game_win_pattern_match when Matches (2) /= GNAT.Regpat.No_Match;

      end loop game_win_pattern_match;

      game_played_pattern_match :
      loop
         GNAT.Regpat.Match (Re_Game_Scratched, SV, Matches, current);

         exit game_played_pattern_match when Matches (0) = GNAT.Regpat.No_Match;

         --  Ada.Text_IO.Put_Line (Item => Matches (1).Last'img & " " & Matches (2).First'img);
         Ada.Text_IO.Put_Line (Item => SV (Matches (1).First .. Matches (1).Last));

         current_win_card := Integer'Value (SV (Matches (1).First .. Matches (1).Last));

         if winners (current_win_card)
         then
            power := power + 1;
         end if;

         current := Matches (0).Last + 1;

      end loop game_played_pattern_match;

      card_count (row) := card_count(row) + 1;

      if power >= 0
      then
         winning_points := 2**power;

         if row < card_count'Last
         then
            for x in row + 1 .. Integer'Min (card_count'last, row + 1 + power)
            loop
               card_count (x) := card_count (x) + card_count (row);
            end loop;
         end if;
      else
         winning_points := 0;
      end if;

   end Get_Input;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;
      running_total : Integer := 0;

      game_points : Integer;

      total_cards : Integer := 0;

      current_row : Integer := 0;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day4_2023.txt");

      while not Ada.Text_IO.End_Of_File (Input_File) loop

         current_row := current_row + 1;

         Get_Input (Input_File,
                    current_row,
                    game_points);

         Ada.Text_IO.Put_Line (game_points'Img);

         running_total := running_total + game_points;

      end loop;

      for x in card_count'range
      loop
               Ada.Text_IO.Put_Line ("total " & card_count(x)'img);

         total_cards := card_count(x) + total_cards;
      end loop;

      Ada.Text_IO.Put_Line ("total " & running_total'Image & " " & total_cards'img);

      Ada.Text_IO.Close (Input_File);

   end run;

end day4_2023;
