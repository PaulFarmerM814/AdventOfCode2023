with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with GNAT.Regpat;
with GNAT.Bubble_Sort;

package body day7_2023 is

   type map_t;

   type map_access_t is access all map_t;

--  Five of a kind, where all five cards have the same label: AAAAA
--  Four of a kind, where four cards have the same label and one card has a different label: AA8AA
--  Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
--  Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
--  Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
--  One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
--  High card, where all cards' labels are distinct: 23456

   type hand_enum_t is (five_of_a_kind,
                        four_of_a_kind,
                        full_house,
                        three_of_a_kind,
                        two_pair,
                        one_pair,
                        high_card);

   type card_t is (c_A, c_K, c_Q, c_T, c_9, c_8, c_7, c_6, c_5, c_4, c_3, c_2, c_J);

   type bit_rep_t is array (card_t) of  String (1 .. 4);

   --  must be a way of using bit patterns to decide best hand, but haven't got the time to work on it
   card_bits : constant bit_rep_t :=
     (c_A => "1110",
      c_K => "1101",
      c_Q => "1100",
      c_J => "1011",
      c_T => "1010",
      c_9 => "1001",
      c_8 => "1000",
      c_7 => "0111",
      c_6 => "0110",
      c_5 => "0101",
      c_4 => "0100",
      c_3 => "0011",
      c_2 => "0010");

   subtype hand_t is  String (1 .. 5);

   type map_t is
      record
         cards    : hand_t;
         cards_unsorted : hand_t;
         bit_rep  : Long_Long_Integer;
         bid      : Long_Long_Integer;
         hand     : hand_enum_t;
         index    : Integer;
         next_map : map_access_t;
         prev_map : map_access_t;
      end record;

   Re_Hand : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("([[:alnum:]]{5})( *[[:digit:]]*)");

   package sort is

      procedure hand_sort (sort_hand : in out hand_t);

   end sort;

   package body sort is

      hand : hand_t;

      procedure Xchg_Procedure (Op1, Op2 : Natural)
      is

         char_store : Character;

      begin

         char_store := hand (Op1);
         hand (Op1) := hand (Op2);
         hand (Op2) := char_store;
      end Xchg_Procedure;

      function Lt_Function (Op1, Op2 : Natural) return Boolean
      is

      begin

         return card_t'Pos (card_t'Value ("c_" & hand (Op1))) < card_t'Pos (card_t'Value ("c_" & hand (Op2)));

      end Lt_Function;

      procedure hand_sort (sort_hand : in out hand_t) is
      begin

         hand := sort_hand;

         GNAT.Bubble_Sort.Sort (5, Xchg_Procedure'Access, Lt_Function'Access);

         sort_hand := hand;

      end hand_sort;

   end sort;

   procedure Get_Input (S : Ada.Text_IO.File_Type;
                        map : in out map_access_t;
                        row : Integer)
   is
      use type GNAT.Regpat.Match_Location;

      USV : constant Ada.Strings.Unbounded.Unbounded_String :=
           Ada.Strings.Unbounded.Text_IO.Get_Line (S);

      SV : constant String := Ada.Strings.Unbounded.To_String (USV);

      Matches : GNAT.Regpat.Match_Array (0 .. 2);

      in_pair_count : Integer := 0;
      first_pair_count : Integer := 0;
      second_pair_count : Integer := 0;

      mismatch : Boolean := True;
      inserted : Boolean := False;

      new_map : map_access_t;
      current_map : map_access_t;
      prev_map : map_access_t;

      current : Integer := 0;

      j_count : Integer := 0;

   begin

      match_loop :
      loop
         GNAT.Regpat.Match (Re_Hand, SV, Matches, current);

         exit match_loop when Matches (0) = GNAT.Regpat.No_Match;

         new_map := new map_t'(cards => SV (Matches (1).First .. Matches (1).Last),
                               cards_unsorted => SV (Matches (1).First .. Matches (1).Last),
                               bid  => Long_Long_Integer'Value (SV (Matches (2).First .. Matches (2).Last)),
                               bit_rep => 0,
                               hand => hand_enum_t'First,
                               index => row,
                               next_map => null,
                               prev_map => null
                              );

         sort.hand_sort (new_map.cards);

         in_pair_count := 0;

         --  decide how many pairs and jokers are in the string
         for x in 1 .. new_map.cards'Length - 1
         loop
            if new_map.cards (x) = 'J'
            then
               j_count := j_count + 1;
               mismatch := True;
            else
               if new_map.cards (x) = new_map.cards (x + 1)
               then
                  if mismatch
                  then
                     in_pair_count := in_pair_count + 1;
                  end if;

                  mismatch := False;

                  if in_pair_count = 1
                  then
                     first_pair_count := first_pair_count + 1;
                  elsif in_pair_count = 2
                  then
                     second_pair_count := second_pair_count + 1;
                  else
                     in_pair_count := 1;
                  end if;
               else
                  mismatch := True;
               end if;
            end if;
         end loop;

         if new_map.cards (new_map.cards'Length) = 'J'
         then
            j_count := j_count + 1;
         end if;

         --  this is all a bit naff, there must be a better way of promoting hands
         if j_count = 5
         then -- special case - all 'j's
            new_map.hand := five_of_a_kind;
         elsif first_pair_count = 4
         then
            new_map.hand := five_of_a_kind;
         elsif first_pair_count = 3 or else second_pair_count = 3
         then
            new_map.hand := four_of_a_kind;
            if j_count = 1
            then
               new_map.hand := five_of_a_kind;
            end if;
         elsif (first_pair_count = 2 and then second_pair_count = 1) or else (first_pair_count = 1 and then second_pair_count = 2)
         then
            new_map.hand := full_house;
            --  impossible if there are 'j's
         elsif (first_pair_count = 2 and then second_pair_count = 0) or else (first_pair_count = 0 and then second_pair_count = 2)
         then
            new_map.hand := three_of_a_kind;
            if j_count = 1
            then
               new_map.hand := four_of_a_kind;
            elsif j_count = 2
            then
               new_map.hand := five_of_a_kind;
            end if;
         elsif first_pair_count = 1 and then second_pair_count = 1
         then
            new_map.hand := two_pair;
            if j_count = 1
            then
               new_map.hand := full_house;
            end if;
         elsif first_pair_count = 1 or else second_pair_count = 1
         then
            new_map.hand := one_pair;
            if j_count = 1
            then
               new_map.hand := three_of_a_kind;
            elsif j_count = 2
            then
               new_map.hand := four_of_a_kind;
            elsif j_count = 3
            then
               new_map.hand := five_of_a_kind;
            end if;
         else
            new_map.hand := high_card;
            if j_count = 1
            then
               new_map.hand := one_pair;
            elsif j_count = 2
            then
               new_map.hand := three_of_a_kind;
            elsif j_count = 3
            then
               new_map.hand := four_of_a_kind;
            elsif j_count = 4
            then
               new_map.hand := five_of_a_kind;
            end if;
         end if;

         Ada.Text_IO.Put_Line ("cards, hand" & new_map.cards & "," & new_map.hand'Img & " " & first_pair_count'Img & " " & second_pair_count'Img);

         --  provide a sorted list. Would be better to have this an abstract and have comparison routines
         --  as likely to reuse in later puzzles
         if map = null
         then
            map := new_map;
         else
            current_map := map;

            while not inserted and then current_map /= null
            loop
               if new_map.hand > current_map.hand
               then
                  Ada.Text_IO. Put_Line ("inserting1 " & new_map.cards & new_map.hand'Img & " " & current_map.cards & current_map.hand'Img);

                  new_map.next_map := current_map;
                  new_map.prev_map := current_map.prev_map;
                  current_map.prev_map := new_map;

                  if new_map.prev_map = null
                  then
                     map := new_map;
                  else
                     new_map.prev_map.next_map := new_map;
                  end if;

                  inserted := True;
               elsif new_map.hand = current_map.hand
               then
                  for x in 1 .. current_map.cards'Length
                  loop
                     if card_t'Value ("c_" & new_map.cards_unsorted (x)) > card_t'Value ("c_" & current_map.cards_unsorted (x))
                     then
                        Ada.Text_IO. Put_Line ("inserting2 " & new_map.cards & new_map.hand'Img & " " & current_map.cards & current_map.hand'Img);

                        --  Ada.Text_IO. Put_Line ("swapping1 " & current.cards & " " & current.next_map.cards);

                        new_map.next_map := current_map;
                        new_map.prev_map := current_map.prev_map;
                        current_map.prev_map := new_map;

                        if new_map.prev_map = null
                        then
                           map := new_map;
                        else
                           new_map.prev_map.next_map := new_map;
                        end if;

                        inserted := True;
                        exit;
                     elsif card_t'Value ("c_" & new_map.cards_unsorted (x)) < card_t'Value ("c_" & current_map.cards_unsorted (x))
                     then
                        exit;
                     end if;
                  end loop;
               end if;
               prev_map := current_map;
               current_map := current_map.next_map;
            end loop;

            --  not inserted, so becomes the end
            if not inserted
            then
               Ada.Text_IO. Put_Line ("inserting3 " & new_map.cards & new_map.hand'Img & " " & prev_map.cards & prev_map.hand'Img);

               prev_map.next_map := new_map;
               new_map.prev_map := prev_map;
            end if;
         end if;

         new_map.bit_rep := Long_Long_Integer'Value ("2#" &
                                                  card_bits (card_t'Value ("c_" & new_map.cards (1))) &
                                                  card_bits (card_t'Value ("c_" & new_map.cards (2))) &
                                                  card_bits (card_t'Value ("c_" & new_map.cards (3))) &
                                                  card_bits (card_t'Value ("c_" & new_map.cards (4))) &
                                                  card_bits (card_t'Value ("c_" & new_map.cards (5))) &
                                                  "#"
                                               );

         current := Matches (0).Last + 1;

      end loop match_loop;

   end Get_Input;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;

      current_row : Integer := 0;

      current_ptr : map_access_t := null;

      running_total : Long_Long_Integer := 0;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day7_2023.txt");

      while not Ada.Text_IO.End_Of_File (Input_File) loop

         current_row := current_row + 1;

         Get_Input (Input_File,
                    current_ptr,
                    current_row);

      end loop;

      current_row := 0;

      while current_ptr /= null
      loop

         Ada.Text_IO.Put_Line ("hand,bid " & current_ptr.cards_unsorted & "," & current_ptr.cards & "," & current_ptr.bid'Img);

         current_row := current_row + 1;
         running_total := running_total + (Long_Long_Integer (current_row) * current_ptr.bid);

         current_ptr := current_ptr.next_map;

      end loop;

      Ada.Text_IO.Put_Line ("running_total " & running_total'Img);

      Ada.Text_IO.Close (Input_File);

   end run;

end day7_2023;
