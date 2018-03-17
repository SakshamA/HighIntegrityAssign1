with ClosedLoop;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

procedure MainDriver is
		-- x : Integer := 1;
begin
	ClosedLoop.Init;
	for I in Integer range 1..150 loop
		ClosedLoop.Tick;
		-- Put("I: ");
		-- Put(Integer(x));
		-- x := i+ 1;
		-- New_line;
	end loop;

end MainDriver;