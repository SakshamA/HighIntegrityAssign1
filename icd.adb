with HRM;
with ImpulseGenerator;
with Measures; use Measures;
with Network;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Task_Identification; use Ada.Task_Identification;

package body ICD is

	-- Initial parameters for tachychardia and ventricle fibrillation 
	INIT_TACHYBOUND : constant Integer := 100;
	INIT_JOULESTODELIVER_VENTFIBRIL : constant Joules := 30;

	-- Joules to deliver when tachycardia is detected
	JOULESTODELIVER_TACHY : constant Joules := 2;

	-- Number of impulses to send when tachycardia is detected
	NUMSIGNAL_TACHY : constant Integer := 10;

	-- Amount of BPM above current heart rate
	TACHY_IMPULSERATECHANGE : constant BPM := 15;

	-- Upper limit for average change in heart rate
	MAX_AVG : constant Integer := 10;

	-- Number of heart rate readings required to detect ventricle 
	-- vibrillation
	NUM_READINGS : constant Integer := 6;

	-- Conversion from decisonds to minute
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
			if (Icd.NumTachySignals < NUMSIGNAL_TACHY) then
			-- Leave tachycardia flag as true
			-- Check if it is time to send the next impulse
			Icd.NumTicksPassed := Icd.NumTicksPassed + 1;
				if Icd.NumTicksPassed = Icd.NumBeatsPerTick  then
					Icd.SendTachySignal := True;
					Icd.NumTachySignals := Icd.NumTachySignals + 1;
					-- Reset tick counter
					Icd.NumTicksPassed := 0;
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
			-- First time tachycardia has been detected
			Icd.IsTachy := True;
			Icd.TachyImpulseBPM := (Rate + TACHY_IMPULSERATECHANGE);

			-- Some calculations to convert ticks to BPM
			Tmp_TachyImpulseBPM := Float(Icd.TachyImpulseBPM);
			Tmp_NumBeatsPerTick := NUMDECISONDS_PERMINUTE/
								   Tmp_TachyImpulseBPM;

			Icd.NumBeatsPerTick := Integer(Float'Floor
										  (Tmp_NumBeatsPerTick));
			Icd.SendTachySignal := True;
			Icd.NumTachySignals := Icd.NumTachySignals + 1;
		else
		-- Either have finished sending all tachycardia signals
		-- Or patient is no longer experiencing tachycardia
			Icd.IsTachy := False;
		end if;

	end CheckTachy;

	procedure CheckVentFibril(Icd : in out ICDType; Monitor : in HRM.HRMType;
							  VentFibrilHistory : in VentFibrilHistoryType) is
		Avg : Integer;
	begin
		Icd.IsVentFibril := False;
		Avg := 0;
		-- Will only calculate average difference if there is sufficient values
		if VentFibrilHistory(NUM_READINGS).Rate /= 0 then	
			Avg := AverageChange(Icd, Monitor, VentFibrilHistory);

			if (Avg >= MAX_AVG) then
				Icd.IsVentFibril := True;
			end if;

		end if;

	end CheckVentFibril;

	function AverageChange(Icd : in ICDType; Monitor : in HRM.HRMType; 
						   VentFibrilHistory : in VentFibrilHistoryType) 
		return Integer is
		Sum : Integer;
	begin
		Sum := 0;

		--Sum differences in the last NUM_READINGS values 
		for I in Integer range (VentFibrilHistory'Last-NUM_READINGS+1)
								..(VentFibrilHistory'Last-1) loop
			Sum := Sum + abs (VentFibrilHistory(I+1).Rate - 
							  VentFibrilHistory(I).Rate);
		end loop;

		return (Sum/NUM_READINGS);

	end AverageChange;

	procedure ChangeSettingsRequest(Icd : in out ICDType; 
		                            NewTachyBound : in BPM; 
		                            NewJoulesToDeliver : in BPM) is
	begin
		Icd.TachyBound := NewTachyBound;
		Icd.VentFibrilJoulesToDeliver := NewJoulesToDeliver;
	end ChangeSettingsRequest;

	procedure ReadSettingsRequest(Icd : in ICDType; 
								  ReadTachyBound : in out BPM; 
								  ReadJoulesToDeliver : in out Joules) is
	begin
		ReadTachyBound := Icd.TachyBound;
		ReadJoulesToDeliver := Icd.VentFibrilJoulesToDeliver;
	end ReadSettingsRequest;

	procedure Tick(Icd : in out ICDType; 
				   Gen : in out ImpulseGenerator.GeneratorType; 
				   Monitor : in HRM.HRMType; 
				   VentFibrilHistory : in VentFibrilHistoryType) is
	begin
		CheckTachy(Icd, Monitor);
		CheckVentFibril(Icd, Monitor, VentFibrilHistory);

		if 	Icd.SendTachySignal and (Icd.IsVentFibril = false) then
			-- Only tachycardia is detected; send signals when required
			Put("Impulse generated for tachycardia"); 
			New_line;
			ImpulseGenerator.SetImpulse(Gen, JOULESTODELIVER_TACHY);

		elsif Icd.IsVentFibril and (Icd.IsTachy = false) then
			-- Only ventricle firbrillation is detected
			Put("Impulse generated for ventricle fibrillation"); 
			New_line;
			ImpulseGenerator.SetImpulse(Gen, Icd.VentFibrilJoulesToDeliver);

		elsif Icd.IsTachy and Icd.IsVentFibril then
			-- Both tachycardia and ventricle fibrillation is detected
			-- Only treat ventricle fibrillation
			Put("Impulse generated for ventricle fibrillation"); 
			New_line;
			ImpulseGenerator.SetImpulse(Gen, Icd.VentFibrilJoulesToDeliver);

			-- Reset counters for tachycardia
			Icd.IsTachy := false;
			Icd.NumTicksPassed := 0;
			Icd.NumTachySignals := 0;
		else
			-- Neither tachycardia nor ventricle fibrillation is detected
			-- Set impulse joules to zero
			ImpulseGenerator.SetImpulse(Gen, 0);
		end if;

	end Tick;

end ICD;