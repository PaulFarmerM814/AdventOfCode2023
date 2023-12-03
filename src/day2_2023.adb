with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with GNAT.Regpat;

package body day2_2023 is

   Re_Game      : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile (".*:");
   Re_Game_id      : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("([[:alpha:]]*) ([[:digit:]]*)");

   Re_Game_Colours      : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("([[:digit:]]+) ([[:alpha:]]+)(,|;|\r*)");
   --Re_Game_Colours      : constant GNAT.Regpat.Pattern_Matcher :=
   --  GNAT.Regpat.Compile ("([[:digit:]]+) ([[:alpha:]]+)(,|;|\s)");

   type colours_t is (green, blue, red);

   procedure Get_Input (S : Ada.Text_IO.File_Type;
                        game : out Integer;
                        valid : out Boolean;
                       power : out Integer)
   is
      use type GNAT.Regpat.Match_Location;

      USV : constant Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Text_IO.Get_Line (S);

      SV : constant String := Ada.Strings.Unbounded.To_String (USV);

      Matches : GNAT.Regpat.Match_Array (0 .. 3);

      current_colour : colours_t;
      current_colour_count : array (colours_t) of Integer := (others => 0);
      max_colour_count : array (colours_t) of Integer := (others => 0);

      current : Integer := 0;

   begin

      GNAT.Regpat.Match (Re_Game, SV, Matches, current);
      Ada.Text_IO.Put_Line (Item => SV (Matches (0).First .. Matches (0).Last));

      GNAT.Regpat.Match (Re_Game_id, SV, Matches, Matches (0).First, Matches (0).Last);

      --  Ada.Text_IO.Put_Line (Item => SV (Matches (1).First .. Matches (1).Last));
      --  Ada.Text_IO.Put_Line (Item => SV (Matches (2).First .. Matches (2).Last));

      game := Integer'Value (SV (Matches (2).First .. Matches (2).Last));

      current := Matches (2).Last + 1;

      valid := True;

      pattern_match :
      loop
         GNAT.Regpat.Match (Re_Game_Colours, SV, Matches, current);

         exit pattern_match when Matches (0) = GNAT.Regpat.No_Match;

         Ada.Text_IO.Put_Line (Item => SV (Matches (2).First .. Matches (2).Last) & " " & SV (Matches (1).First .. Matches (1).Last));

         current_colour := colours_t'Value (SV (Matches (2).First .. Matches (2).Last));
         current_colour_count (current_colour) := Integer'Value (SV (Matches (1).First .. Matches (1).Last));

         case current_colour is
            when red =>
               if current_colour_count (red) > 12
               then
                  Ada.Text_IO.Put_Line (Item => ">>>>>>>>> " & SV (Matches (2).First .. Matches (2).Last) & " " & SV (Matches (1).First .. Matches (1).Last));

                  valid := False;
               end if;

            when green =>
               if current_colour_count (green) > 13
               then
                  Ada.Text_IO.Put_Line (Item => ">>>>>>>>> " & SV (Matches (2).First .. Matches (2).Last) & " " & SV (Matches (1).First .. Matches (1).Last));

                  valid := False;
               end if;

            when blue =>
               if current_colour_count (blue) > 14
               then
                  Ada.Text_IO.Put_Line (Item => ">>>>>>>>> " & SV (Matches (2).First .. Matches (2).Last) & " " & SV (Matches (1).First .. Matches (1).Last));

                  valid := False;
               end if;
         end case;

         if current_colour_count (current_colour) > max_colour_count(current_colour)
         then
               max_colour_count(current_colour) := current_colour_count (current_colour);
         end if;

         current := Matches (0).Last + 1;

      end loop pattern_match;

      power := 1;

      for X in colours_t
      loop
         power := power * max_colour_count(X);
      end loop;


   end Get_Input;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;
      --  Input_Stream : Ada.Streams.Stream_IO.Stream_Access;
      running_total : Integer := 0;
      running_total_power : Integer := 0;

      valid : Boolean := False;
      game : Integer := 0;
      power : Integer := 0;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day2_2023.txt");

      --  Input_Stream := Ada.Streams.Stream_IO.Stream (Input_File);

      while not Ada.Text_IO.End_Of_File (Input_File) loop

         Get_Input (Input_File,
                    game,
                    valid,
                    power);

         running_total_power := running_total_power + power;

         if valid
         then
            Ada.Text_IO.Put_Line (game'img);

            running_total := running_total + game;
         end if;

      end loop;

      Ada.Text_IO.Put_Line ("total " & running_total'Image & " power " & running_total_power'image);

      Ada.Text_IO.Close (Input_File);

   end run;

end day2_2023;
