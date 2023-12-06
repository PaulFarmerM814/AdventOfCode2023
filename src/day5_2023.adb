with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with GNAT.Regpat;

package body day5_2023 is

   type map_t;

   type map_access_t is access all map_t;

   type map_t is
      record
         map_start : Long_Long_Integer;
         map_end : Long_Long_Integer;
         map_range : Long_Long_Integer;
         next_map : map_access_t;
      end record;

   seeds_index : Integer := 1;

   seeds_to_be_planted : array (1 .. 100) of Long_Long_Integer := (others => 0);

   type state_t is (starting_point,
                    seeds,
                    seed_to_soil,
                    soil_to_fertilizer,
                    fertilizer_to_water,
                    water_to_light,
                    light_to_temperature,
                    temperature_to_humidity,
                    humidity_to_location);

   subtype active_state_t is state_t range seeds .. humidity_to_location;

   type maps_t is array (active_state_t) of map_access_t;

   keys : constant array (active_state_t) of Ada.Strings.Unbounded.Unbounded_String :=
     (seeds => Ada.Strings.Unbounded.To_Unbounded_String ("seeds:"),
      seed_to_soil => Ada.Strings.Unbounded.To_Unbounded_String ("seed-to-soil map:"),
      soil_to_fertilizer => Ada.Strings.Unbounded.To_Unbounded_String ("soil-to-fertilizer map:"),
      fertilizer_to_water => Ada.Strings.Unbounded.To_Unbounded_String ("fertilizer-to-water map:"),
      water_to_light => Ada.Strings.Unbounded.To_Unbounded_String ("water-to-light map:"),
      light_to_temperature => Ada.Strings.Unbounded.To_Unbounded_String ("light-to-temperature map:"),
      temperature_to_humidity => Ada.Strings.Unbounded.To_Unbounded_String ("temperature-to-humidity map:"),
      humidity_to_location => Ada.Strings.Unbounded.To_Unbounded_String ("humidity-to-location map:"));

   maps : maps_t := (others => null);

   current_state : state_t := starting_point;

   seed_map : array (active_state_t) of Long_Long_Integer;

   Re_Map : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("([[:digit:]]+) +([[:digit:]]+) +([[:digit:]]+)");

   Re_Seed : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("([[:digit:]]+)");

   procedure Get_Input (S : Ada.Text_IO.File_Type;
                        row : Integer)
   is
      use type GNAT.Regpat.Match_Location;

      USV : constant Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Text_IO.Get_Line (S);

      SV : constant String := Ada.Strings.Unbounded.To_String (USV);

      Matches : GNAT.Regpat.Match_Array (0 .. 3);

      current : Integer := 0;

      procedure save_map is

         found : Boolean := False;

         next_map : map_access_t;

      begin

         current := 0;

         map_loop :
         loop
            GNAT.Regpat.Match (Re_Map, SV, Matches, current);

            exit map_loop when Matches (0) = GNAT.Regpat.No_Match;

            if maps (current_state) = null
            then
               maps (current_state) := new map_t'(map_start => Long_Long_Integer'Value (SV (Matches (1).First .. Matches (1).Last)),
                                                  map_end  => Long_Long_Integer'Value (SV (Matches (2).First .. Matches (2).Last)),
                                                  map_range  => Long_Long_Integer'Value (SV (Matches (3).First .. Matches (3).Last)),
                                                  next_map => null
                                                 );
            else
               next_map := maps (current_state);
               maps (current_state) := new map_t'(map_start => Long_Long_Integer'Value (SV (Matches (1).First .. Matches (1).Last)),
                                                  map_end  => Long_Long_Integer'Value (SV (Matches (2).First .. Matches (2).Last)),
                                                  map_range  => Long_Long_Integer'Value (SV (Matches (3).First .. Matches (3).Last)),
                                                  next_map => next_map
                                                 );
            end if;

            current := Matches (0).Last + 1;

            found := True;

         end loop map_loop;

         -- if found
         -- then
         --    Ada.Text_IO.Put_Line (Item => maps (current_state).map_start'Img & " " & maps (current_state).map_end'Img & " " & maps (current_state).map_range'Img);
         -- end if;

      end save_map;

   begin

      case current_state is
         when starting_point =>
            if SV (1 .. Ada.Strings.Unbounded.Length (keys (seeds))) = keys (seeds)
            then
               current_state := state_t'Succ (current_state);

               current := Ada.Strings.Unbounded.Length (keys (seeds)) + 1;

               seed_loop :
               loop
                  GNAT.Regpat.Match (Re_Seed, SV, Matches, current);

                  exit seed_loop when Matches (0) = GNAT.Regpat.No_Match;

                  -- Ada.Text_IO.Put_Line (Item => SV (Matches (1).First .. Matches (1).Last));

                  seeds_to_be_planted (seeds_index) := Long_Long_Integer'Value (SV (Matches (1).First .. Matches (1).Last));
                  seeds_index := seeds_index + 1;

                  current := Matches (0).Last + 1;

               end loop seed_loop;
            end if;

         when seeds =>

            if SV'Length >= Ada.Strings.Unbounded.Length (keys (state_t'Succ (current_state)))
              and then
                SV (1 .. Ada.Strings.Unbounded.Length (keys (state_t'Succ (current_state)))) = keys (state_t'Succ (current_state))
            then
               current_state := state_t'Succ (current_state);
               -- Ada.Text_IO.Put_Line ("changing state " & current_state'Img);
            end if;

         when seed_to_soil |
              soil_to_fertilizer |
              fertilizer_to_water |
              water_to_light |
              light_to_temperature |
              temperature_to_humidity =>

            if SV'Length >= Ada.Strings.Unbounded.Length (keys (state_t'Succ (current_state)))
              and then
                SV (1 .. Ada.Strings.Unbounded.Length (keys (state_t'Succ (current_state)))) = keys (state_t'Succ (current_state))
            then
               current_state := state_t'Succ (current_state);
               -- Ada.Text_IO.Put_Line ("changing state " & current_state'Img);
            end if;

            save_map;

         when humidity_to_location =>

            save_map;

      end case;

   end Get_Input;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;

      current_row : Integer := 0;

      current_ptr : map_access_t;

      match_state : state_t := seed_to_soil;
      prev_state  : state_t := seeds;

      lowest : Long_Long_Integer := Long_Long_Integer'Last;

      x : Integer := 1;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day5_2023.txt");

      while not Ada.Text_IO.End_Of_File (Input_File) loop

         current_row := current_row + 1;

         Get_Input (Input_File,
                    current_row);

      end loop;

      while seeds_to_be_planted (x) /= 0
      loop

         seed_map := (others => -1);

         for y in seeds_to_be_planted (x) .. seeds_to_be_planted (x) + seeds_to_be_planted (x + 1) - 1
         loop
            seed_map (seeds) := y;

            match_state := seed_to_soil;
            prev_state  := seeds;

            loop
               current_ptr := maps (match_state);

               while current_ptr /= null
               loop
                  -- Ada.Text_IO.Put_Line (match_state'Img & " testing " & current_ptr.map_start'Img);

                  if seed_map (prev_state) in current_ptr.map_end .. current_ptr.map_end + current_ptr.map_range - 1
                  then
                     seed_map (match_state) := current_ptr.map_start + seed_map (prev_state) - current_ptr.map_end;
                     -- Ada.Text_IO.Put_Line (match_state'Img & " in range " & current_ptr.map_start'Img);

                     exit;
                  end if;
                  current_ptr := current_ptr.next_map;
               end loop;

               if seed_map (match_state) = -1
               then
                  seed_map (match_state) := seed_map (prev_state);
               end if;

               -- Ada.Text_IO.Put_Line (match_state'Img & " " & seed_map (match_state)'Img);

               exit when match_state = state_t'Last;
               prev_state := match_state;
               match_state := state_t'Succ (match_state);

            end loop;

            lowest := Long_Long_Integer'Min (seed_map (humidity_to_location), lowest);

            -- Ada.Text_IO.Put_Line ("<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>");
         end loop;

         x := x + 2;

      end loop;

      Ada.Text_IO.Put_Line ("lowest " & lowest'Img);

      Ada.Text_IO.Close (Input_File);

   end run;

end day5_2023;
