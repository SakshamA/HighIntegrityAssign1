with HRM;
with ImpulseGenerator;
with Measures; use Measures;
with Network;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body ICD is

	INIT_TACHYBOUND : constant Integer := 100;
	INIT_JOULESTODELIVER_VENTFIBRIL : constant Joules := 30;
	JOULESTODELIVER_TACHY : constant Joules := 2;
	NUMSIGNAL_TACHY : constant Integer := 10;
	TACHY_IMPULSERATECHANGE : constant BPM := 15;
	NUMDECISONDS_PERMINUTE : constant Float := 600.0;

	procedure Init(Icd : out ICDType) is
	begin
		Icd.IsModeOn := False;
		Icd.IsTachy := False;
		Icd.IsVentFibril := False;	
		Icd.TachyBound := INIT_TACHYBOUND;
		Icd.VentFibrilJoulesToDeliver := INIT_JOULESTODELIVER_VENTFIBRIL;
		Icd.NumTachySignals := 0;
		Icd.NumBeatsPerTick := 0;
		Icd.TachyImpulseBPM := 0;
		Icd.NumTicksPassed := 0;
		Icd.SendTachySignal := False;
	end Init;

	procedure On(Icd : out ICDType) is
	begin
		Icd.IsModeOn := True;
	end On;

	procedure Off(Icd : out ICDType) is
	begin
		Icd.IsModeOn := False;
	end Off;

	function IsOn(Icd : in ICDType) return Boolean is
   	begin
      return Icd.IsModeOn;
   	end IsOn;

	-- A procedure to detect tachycardia
	procedure CheckTachy(Icd : in out ICDType; Monitor : in HRM.HRMType) is
		Rate : Measures.BPM;
		Tmp_NumBeatsPerTick : Float;
		Tmp_TachyImpulseBPM : Float;
	begin
		Icd.SendTachySignal := False;
		HRM.GetRate(Monitor, Rate);
		if Icd.IsTachy then
		-- Tachycardia has already been previously detected;
		-- Continue sending signals until max number.
		Put("Passed 1");
			if (Icd.NumTachySignals < NUMSIGNAL_TACHY) then
			-- Leave tachycardia flag as true
			-- Check if it is time to send the next impulse
			Put("Passed 2");
				if Icd.NumTicksPassed = Icd.NumBeatsPerTick  then
					Put("Passed 4");
					Icd.SendTachySignal := True;
					Icd.NumTachySignals := Icd.NumTachySignals + 1;
					-- Reset tick counter
					Icd.NumTicksPassed := 0;
				else
					Put("Passed 3");
					Icd.NumTicksPassed := Icd.NumTicksPassed + 1;
				end if;
			else 
			-- Finished sending all signals
			-- Check if patient is still experiencing tachycardia.
				Icd.IsTachy := False;
				Icd.NumTachySignals := 0;
				Icd.NumTicksPassed := 0;
				CheckTachy(Icd, Monitor);
			end if;
		elsif (Rate > Icd.TachyBound) then
		-- First time tachycarda has been detected
			Icd.IsTachy := True;
			Icd.TachyImpulseBPM := (Rate + TACHY_IMPULSERATECHANGE);

		-- Some conversions to integers
			Tmp_TachyImpulseBPM := Float(Icd.TachyImpulseBPM);
			Tmp_NumBeatsPerTick := NUMDECISONDS_PERMINUTE / Tmp_TachyImpulseBPM;

			Icd.NumBeatsPerTick := Integer(Float'Floor(Tmp_NumBeatsPerTick));
			Icd.SendTachySignal := True;
			Icd.NumTachySignals := Icd.NumTachySignals + 1;
			Icd.NumTicksPassed := Icd.NumTicksPassed + 1;
		else
		-- Either have finished sending all tachycardia signals
		-- Or patient is no longer experiencing tachycardia
			Icd.IsTachy := False;
		end if;
	end CheckTachy;

	function HasTachy(Icd : in ICDType) return Boolean is
	begin
		return Icd.IsTachy;
	end HasTachy;

	function HasVentFibril(Icd : in ICDType) return Boolean is
	begin
		return Icd.IsVentFibril;
	end HasVentFibril;

	-- A procedure to detect ventricle fibrillation
	procedure CheckVentFibril(Icd : in out ICDType; Monitor : in HRM.HRMType; History : in Network.RateHistory) is
	Avg : Integer;
	begin
		Avg := 0;
		-- Put(History(1).Rate);
		-- New_line;
		if History(History'Last).Rate /= 0 then	
			Avg := AverageChange(Icd, Monitor, History);
			if (Avg >= 2) then
				Icd.IsVentFibril := True;
			else 
				Icd.IsVentFibril := False;
			end if;
		end if;
	end CheckVentFibril;

	-- A function to calculate average change in heart rate per reading over
	-- the previous six readings
	function AverageChange(Icd : in ICDType; Monitor : in HRM.HRMType; History : in Network.RateHistory)
						   return Integer is
		Sum : Integer;
		Rate : Measures.BPM;
	begin
		Sum := 0;
		--Sum differences in historical values
			for I in Integer range 1..(History'Last-1) loop
				Sum := Sum + abs (History(I+1).Rate - History(I).Rate);
			end loop;
		-- Change from most recent Tick
		HRM.GetRate(Monitor, Rate);
		Sum := Sum + (Rate - History(History'Last).Rate);	
	return (Sum/6);
	end AverageChange;

	procedure ChangeSettingsResponse(Icd in out ICDType; NewTachyBound in BPM; NewJoulesToDeliver in BPM) is
	begin
		Icd.TachyBound := NewTachyBound;
		Icd.VentFibrilJoulesToDeliver := NewJoulesToDeliver;
	end ChangeSettingsResponse;

	procedure ReadSettingsResponse(Icd in ICDType; ReadTachyBound in out BPM; ReadJoulesToDeliver in out Joules) is
		ReadTachyBound := Icd.TachyBound;
		ReadJoulesToDeliver := Icd.VentFibrilJoulesToDeliver;
	end ReadSettingsResponse;

	procedure 

	procedure Tick(Icd : in out ICDType; Gen : in out ImpulseGenerator.GeneratorType; Monitor : in HRM.HRMType; History : in Network.RateHistory) is
	begin
		-- Calculate impulse if ICD system is on
		if Icd.IsModeOn then
			CheckTachy(Icd, Monitor);
			CheckVentFibril(Icd, Monitor, History);
			if 	Icd.SendTachySignal then
				ImpulseGenerator.SetImpulse(Gen, JOULESTODELIVER_TACHY);
			-- end if;
			elsif Icd.IsVentFibril then
				ImpulseGenerator.SetImpulse(Gen, Icd.VentFibrilJoulesToDeliver);
			end if;
			if (Icd.IsTachy = False and Icd.IsVentFibril = False) then
				ImpulseGenerator.SetImpulse(Gen, 0);
			end if;
		end if;
	end Tick;

end ICD;