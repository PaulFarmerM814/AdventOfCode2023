package body day1_2015 is

   procedure run is
      Input_File   : Ada.Streams.Stream_IO.File_Type;
      Input_Stream : Ada.Streams.Stream_IO.Stream_Access;
      movement     : Character;

      current_floor : Integer := 0;
      current_pos : Integer := 0;
      basement_found : boolean := False;

   begin
      Ada.Streams.Stream_IO.Open
        (File => Input_File, Mode => Ada.Streams.Stream_IO.In_File,
         Name => "input.txt");

      Input_Stream := Ada.Streams.Stream_IO.Stream (Input_File);

      while not Ada.Streams.Stream_IO.End_Of_File (Input_File) loop

         Character'Read (Input_Stream, movement);

         current_pos := integer'Succ(current_pos);

         if movement = '(' then -- going up
            current_floor := current_floor + 1;
         elsif movement = ')' then -- going down
            current_floor := current_floor - 1;
         end if;

         if not basement_found and then current_floor < 0 then
            Ada.Text_IO.Put_Line ("basement " & current_pos'Image);
            basement_found := True;
         end if;

      end loop;

      Ada.Text_IO.Put_Line ("floor " & current_floor'Image);
   end run;

end day1_2015;
