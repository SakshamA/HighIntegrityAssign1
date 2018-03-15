with Measures; use Measures;
with HRM;
with Network;
with ImpulseGenerator;
--Testing
-- This package simulates the software component for an 
-- implantable cardioverter-defibrillator (ICD). It is provided with
-- measured heart rates, and calculates and provides the impulse
-- to be delivered to the heart by the impulse generator.
package ICD is
	
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

	-- Create and initialise an ICD.
	procedure Init(Icd : out ICDType);

	procedure On(Icd : out ICDType);

	procedure Off(Icd : out ICDType);

	function IsOn(Icd : in ICDType) return Boolean;

	-- A procedure to detect tachycardia
	procedure CheckTachy(Icd : in out ICDType; Monitor : in HRM.HRMType);

	function HasTachy(Icd : in ICDType) return Boolean; 

	function HasVentFibril(Icd : in ICDType) return Boolean; 

	-- A procedure to detect ventricle fibrillation
	procedure CheckVentFibril(Icd : in out ICDType; Monitor : in HRM.HRMType; 
						   		  History : in Network.RateHistory);

	-- A function to calculate average change in heart rate per reading over
	-- the previous six readings
	function AverageChange(Icd : in ICDType; Monitor : in HRM.HRMType; 
						        History : in Network.RateHistory) return Integer;

	procedure Tick(Icd : in out ICDType; Gen : in out ImpulseGenerator.GeneratorType; Monitor : in HRM.HRMType; History : in Network.RateHistory);

end ICD;

	-- Turn on the ICD.

	-- Turn off the ICD.

	-- Get the status of the ICD.

	-- A function to detect tachycardia
	--# return B.IsTachy

	-- A function to detect ventricle fibrillation
	--# return B.IsVentFibril

