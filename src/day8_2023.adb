with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with GNAT.Regpat;

with GNAT.Dynamic_HTables;

package body day8_2023 is

   subtype hash_key_t is Long_Integer range 1 .. (50 * 30 * 26) + (30 * 26) + 26;

   subtype key_t is  String (1 .. 3);

   type element_t is
      record
         key   : key_t;
         left  : key_t;
         right : key_t;
      end record;

   no_element : constant element_t := (key   => "   ",
                                       left  => "   ",
                                       right => "   ");

   type node_t;

   type node_access_t is access all node_t;

   type node_t is
      record
         key             : key_t;
         current_element : element_t;
         next_element    : element_t;
         complete        : Boolean;
         complete_count  : Long_Long_Long_Integer;
         next_node       : node_access_t;
      end record;

   a_nodes : node_access_t := null;

   package hash_store is
      function hash (f : key_t) return hash_key_t;
      function equal (F1 : key_t; F2 : key_t) return Boolean;
   end hash_store;

   package body hash_store is
      function hash (f : key_t) return hash_key_t
      is
      begin

         return
           ((Character'Pos (f (1)) - Character'Pos ('A') + 1) * 50 * 30) +
           ((Character'Pos (f (2)) - Character'Pos ('A') + 1) * 30) +
           ((Character'Pos (f (3)) - Character'Pos ('A') + 1));
      end hash;

      function equal (F1 : key_t; F2 : key_t) return Boolean
      is
      begin
         return F1 = F2;
      end equal;

   end hash_store;

   package hash_table is new GNAT.Dynamic_HTables.Simple_HTable
     (Header_Num => hash_key_t,
      Element    => element_t,
      No_Element => no_element,
      Key        => key_t,
      Hash       => hash_store.hash,
      Equal      => hash_store.equal);

   instance : hash_table.Instance := hash_table.Nil;

   --  AAA = (BBB, BBB)
   Re_Instruction      : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("([[:alnum:]]{3}) = \(([[:alnum:]]{3}), ([[:alnum:]]{3})\)");

   procedure Get_Input (S : Ada.Text_IO.File_Type;
                        row : Integer;
                        key : out key_t)
   is

      USV : constant Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Text_IO.Get_Line (S);

      SV : constant String := Ada.Strings.Unbounded.To_String (USV);

      Matches : GNAT.Regpat.Match_Array (0 .. 3);

      current : Integer := 0;

   begin

      GNAT.Regpat.Match (Re_Instruction, SV, Matches, current);
      Ada.Text_IO.Put_Line (Item => SV (Matches (0).First .. Matches (0).Last));

      hash_table.Set
        (T => instance,
         K => SV (Matches (1).First .. Matches (1).Last),
         E => element_t'
                (key   => SV (Matches (1).First .. Matches (1).Last),
                 left  => SV (Matches (2).First .. Matches (2).Last),
                 right => SV (Matches (3).First .. Matches (3).Last)));

      key := SV (Matches (1).First .. Matches (1).Last);

      --  Ada.Text_IO.Put_Line (hash_store.hash (SV (Matches (1).First .. Matches (1).Last))'Img);

      if key (3) = 'A'
      then
         Ada.Text_IO.Put_Line ("key " & key);

         if a_nodes = null
         then
            a_nodes := new node_t'(key             => key,
                                   current_element => no_element,
                                   next_element    => no_element,
                                   complete => False,
                                   complete_count => 0,
                                   next_node       => null
                                  );
         else
            a_nodes := new node_t'(key            => key,
                                   current_element => no_element,
                                   next_element    => no_element,
                                   complete => False,
                                   complete_count => 0,
                                   next_node       => a_nodes
                                  );
         end if;
      end if;

   end Get_Input;

   function gcd (num1 : Long_Long_Long_Integer;
                num2 : Long_Long_Long_Integer)
                return Long_Long_Long_Integer
   is

      num1_work : Long_Long_Long_Integer := num1;
      num2_work : Long_Long_Long_Integer := num2;

   begin

      --  Loop till both numbers are equal
      while num1_work /= num2_work
      loop

         if num1_work > num2_work
         then
            num1_work := num1_work - num2_work;
         else
            num2_work := num2_work - num1_work;
         end if;

      end loop;

      return num2_work;
   end gcd;

   function lcm (num1 : Long_Long_Long_Integer;
                num2 : Long_Long_Long_Integer)
                return Long_Long_Long_Integer
   is

      num1_work : constant Long_Long_Long_Integer := num1;
      num2_work : constant Long_Long_Long_Integer := num2;

      gcd_val : constant Long_Long_Long_Integer := gcd (num1_work, num2_work);

   begin

      return (num1_work * num2_work) / gcd_val;

   end lcm;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;
      running_total : Long_Long_Long_Integer := 0;
      lcm_value : Long_Long_Long_Integer := 0;

      current_row : Integer := 0;

      directions : Ada.Strings.Unbounded.Unbounded_String;
      blank : Ada.Strings.Unbounded.Unbounded_String;
      pragma Unreferenced (blank);

      journey_complete : Boolean := False;

      key : key_t;

      current_node : node_access_t := null;

      non_z_found : Boolean := False;

      found_incomplete : Boolean := False;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day8_2023.txt");

      directions := Ada.Strings.Unbounded.Text_IO.Get_Line (Input_File);
      blank := Ada.Strings.Unbounded.Text_IO.Get_Line (Input_File);

      while not Ada.Text_IO.End_Of_File (Input_File) loop

         current_row := current_row + 1;

         Get_Input (Input_File,
                    current_row,
                    key);

      end loop;

      Ada.Text_IO.Put_Line ("key " & key);

      current_node := a_nodes;
      non_z_found := False;
      found_incomplete := False;

      while current_node /= null
      loop
         current_node.current_element := hash_table.Get (instance, current_node.key);

         current_node := current_node.next_node;
      end loop;

      declare
         directions_str : constant String := Ada.Strings.Unbounded.To_String (directions);
      begin
         while not journey_complete
         loop

            for x in directions_str'Range
            loop

               current_node := a_nodes;
               non_z_found := False;
               found_incomplete := False;

               running_total := running_total + 1;

               while current_node /= null
               loop

                  if not current_node.complete
                  then
                     if directions_str (x) = 'L'
                     then
                        current_node.next_element := hash_table.Get (instance, current_node.current_element.left);
                     else
                        current_node.next_element := hash_table.Get (instance, current_node.current_element.right);
                     end if;

                     --  Ada.Text_IO.Put_Line (directions_str (x) & "," & next_element.key);

                     if current_node.next_element.key (3) /= 'Z'
                     then
                        non_z_found := True;
                        found_incomplete := True;
                     else
                        current_node.complete_count := running_total;
                        current_node.complete := True;
                     end if;

                     current_node.current_element := current_node.next_element;
                  end if;

                  current_node := current_node.next_node;

               end loop;

               exit when not found_incomplete;

            end loop;

            journey_complete := not found_incomplete;

            --  Ada.Text_IO.Put_Line ("repeat " & running_total'Image);

         end loop;
      end;

      current_node := a_nodes;

      lcm_value := current_node.complete_count;

      current_node := current_node.next_node;

      while current_node /= null
      loop

         lcm_value := lcm (lcm_value, current_node.complete_count);

         Ada.Text_IO.Put_Line (lcm_value'Img & " " & current_node.complete_count'img & current_node.complete'img);

         current_node := current_node.next_node;

      end loop;

      Ada.Text_IO.Put_Line ("total " & running_total'Image & " " & lcm_value'Img);

      Ada.Text_IO.Close (Input_File);

   end run;

end day8_2023;
