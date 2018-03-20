with ClosedLoop;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

procedure MainDriver is
		-- x : Integer := 1;
begin
	ClosedLoop.Init;
	for I in Integer range 1..300 loop
		ClosedLoop.Tick;
	end loop;

end MainDriver;