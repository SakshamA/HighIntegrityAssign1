with Measures; use Measures;
with HRM;
with closedloop;
with Network;
with ImpulseGenerator;

-- This package simulates the software component for an 
-- implantable cardioverter-defibrillator (ICD). It is provided with
-- measured heart rates, and calculates and provides the impulse
-- to be delivered to the heart by the impulse generator.

-- Assumptions:
-- i. If both tachycardia and ventricle fibrillation is detected,
-- ICD will only give impulses to treat ventricle fibrillation
-- ii. If patient is getting treated for tachycardia, the ICD cannot be
-- turned off
-- iii. TachyBound and VentFibrilJoulesToDeliver settings can only be 
-- changed if they are within set bounds
package ICD is
	
	-- Number of readings required for detecting
	-- ventricle fibrillation
	NUMREADINGS_VENTFIBRIL : constant Integer := 6;
	-- Amount of BPM above current heart rate for tachycardia
	TACHY_RATECHANGE : constant BPM := 15;

	type ICDType is
		record
	-- Indicates whether the ICD is on.
	IsModeOn : Boolean;
	-- The current upper bound for tachycardia
	TachyBound : BPM;
	-- The current number of joules to deliver for ventricle fibrillation
	VentFibrilJoulesToDeliver : Joules;
	-- Indicates whether tachychardia is occurring
	IsTachy : Boolean;
	-- Indicates whether ventricle fibrillation is occurring
	IsVentFibril : Boolean;
	-- The number of signals that have been sent when tachychardia is detected
	NumTachySignals : Integer;
	-- The number of beats per tick; used to determine when to send impulses 
	-- to treat tachycardia
	NumBeatsPerTick : Integer;
	--  The rate at which impulses for tachycardia need to be sent
	TachyImpulseBPM : BPM;
	-- Number of ticks passed since last impulse was sent for tachycardia
	NumTicksPassed : Integer;
	-- Indicates whether to send an impulse for tachycardia
	SendTachySignal : Boolean;
		end record;

	type VentFibrilHistoryType is array 
	(Integer range 1..NUMREADINGS_VENTFIBRIL) of Network.RateRecord;

	-- Create and initialise an ICD
	procedure Init(Icd : out ICDType);

	-- Turn on the ICD
	procedure On(Icd : out ICDType);

	-- Turn off the ICD
	procedure Off(Icd : out ICDType);

	-- A procedure to detect tachycardia and send impulses if necessary
	procedure CheckTachy(Icd : in out ICDType; Monitor : in HRM.HRMType);

	-- A procedure to detect ventricle fibrillation
	procedure CheckVentFibril(Icd : in out ICDType; Monitor : in HRM.HRMType; 
						   	  VentFibrilHistory : in VentFibrilHistoryType);

	-- A function to calculate average change in heart rate per reading over
	-- the previous six readings
	function AverageChange(Icd : in ICDType; Monitor : in HRM.HRMType; 
						   VentFibrilHistory : in VentFibrilHistoryType) 
		return Integer;

	-- A procedure to change tachycardia and ventricle fibrillation settings
	procedure ChangeSettingsRequest(Icd : in out ICDType; 
		                            NewTachyBound : in BPM; 
		                            NewJoulesToDeliver : in BPM); 

	-- A procedure to read tachycardia and ventricle fibrillation settings
		procedure ReadSettingsRequest(Icd : in ICDType; 
								  	  ReadTachyBound : in out BPM; 
								  	  ReadJoulesToDeliver : in out Joules);

	-- Tick the clock; determine whether or not to send impulses
	-- If both tachycardia and ventricle fibrillation is detected,
	-- Only give impulses to treat ventricle fibrillation
	procedure Tick(Icd : in out ICDType; 
				   Gen : in out ImpulseGenerator.GeneratorType; 
			       Monitor : in HRM.HRMType; 
			       VentFibrilHistory : in VentFibrilHistoryType);

end ICD;
