with HRM;
with ImpulseGenerator;
with Measures; use Measures;
with Network;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body ICD is

	-- Parameters for tachychardia and ventricle fibrillation 
	INIT_TACHYBOUND : constant Integer := 100;
	INIT_JOULESTODELIVER_VENTFIBRIL : constant Joules := 30;
	JOULESTODELIVER_TACHY : constant Joules := 2;
	NUMSIGNAL_TACHY : constant Integer := 10;

	-- Upper limit for average change in heart rate
	MAX_AVG : constant Integer := 10;

	-- Number of heart rate readings required to detect ventricle 
	-- vibrillation
	NUM_READINGS : constant Integer := 5;

	-- Amount of BPM above current heart rate when tachycardia is detected
	TACHY_IMPULSERATECHANGE : constant BPM := 15;

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
				if Icd.NumTicksPassed = Icd.NumBeatsPerTick  then
					Icd.SendTachySignal := True;
					Icd.NumTachySignals := Icd.NumTachySignals + 1;
					-- Reset tick counter
					Icd.NumTicksPassed := 0;
				else
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

			-- Some calculations to convert ticks to BPM
			Tmp_TachyImpulseBPM := Float(Icd.TachyImpulseBPM);
			Tmp_NumBeatsPerTick := NUMDECISONDS_PERMINUTE/Tmp_TachyImpulseBPM;

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

	procedure CheckVentFibril(Icd : in out ICDType; Monitor : in HRM.HRMType;
							  HistoryNew : in closedloop.RateHistoryNew) is
		Avg : Integer;
	begin
		Avg := 0;
		-- Will only calculate average difference if there is sufficient values
		if HistoryNew(NUM_READINGS).Rate /= 0 then	
			Avg := AverageChange(Icd, Monitor, HistoryNew);
			if (Avg >= MAX_AVG) then
				Icd.IsVentFibril := True;
			else 
				Icd.IsVentFibril := False;
			end if;
		end if;
	end CheckVentFibril;

	function AverageChange(Icd : in ICDType; Monitor : in HRM.HRMType; 
						   HistoryNew : in closedloop.RateHistoryNew) return Integer is
		Sum : Integer;
	begin
		Sum := 0;
		--Sum differences in the last NUM_READINGS values 
			for I in Integer range (HistoryNew'Last-NUM_READINGS+1)
									..(HistoryNew'Last-1) loop
				Sum := Sum + abs (HistoryNew(I+1).Rate - HistoryNew(I).Rate);
			end loop;	
	return (Sum/6);
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
				   HistoryNew : in closedloop.RateHistoryNew) is
	begin
		CheckTachy(Icd, Monitor);
		CheckVentFibril(Icd, Monitor, HistoryNew);
		if 	Icd.SendTachySignal then
			ImpulseGenerator.SetImpulse(Gen, JOULESTODELIVER_TACHY);
		elsif Icd.IsVentFibril then
			ImpulseGenerator.SetImpulse(Gen, Icd.VentFibrilJoulesToDeliver);
		end if;
		if (Icd.IsTachy = False and Icd.IsVentFibril = False) then
			ImpulseGenerator.SetImpulse(Gen, 0);
		end if;
	end Tick;

end ICD;