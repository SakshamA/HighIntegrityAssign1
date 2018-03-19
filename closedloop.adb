with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Measures; use Measures;
with Heart;
with HRM;
with ImpulseGenerator;
with Network;
with Principal;
with ICD;

package body ClosedLoop is

	NUM_KNOWNPRINCIPALS : constant Integer := 3;
	Hrt : Heart.HeartType;                -- The simulated heart
	Monitor : HRM.HRMType;                -- The simulated heart rate monitor
	Generator : ImpulseGenerator.GeneratorType; -- The simulated generator
	HeartRate : BPM;
	ICDSoftware :ICD.ICDType;
	Net : Network.Network;                -- The simulated network
	Card : Principal.PrincipalPtr;   -- A cardiologist
	Clin : Principal.PrincipalPtr;   -- A clinical assistant
	Patient : Principal.PrincipalPtr; -- A patient
	-- stores whether there was a message available on the network
	MsgAvailable : Boolean;
	-- stores the current message read from the network (if one was available)
	Msg : Network.NetworkMessage;

	-- stores some history information on measured heart rate
	History : Network.RateHistory;
	HistoryPos : Integer := History'First;
	CurrentTime : TickCount := 0;
	-- an array of known principals to use to initialise the network
	-- but note that the network can generate messages from other, unknown,
	-- principals too
	KnownPrincipals : access Network.PrincipalArray;

procedure Init is
begin
	Card := new Principal.Principal;
	Clin := new Principal.Principal;
	Patient := new Principal.Principal;

	-- MsgAvailable := False;

	KnownPrincipals := new Network.PrincipalArray(0..2);

	-- set up the principals with the correct roles
   	Principal.InitPrincipalForRole(Card.all,Principal.Cardiologist);
   	Principal.InitPrincipalForRole(Clin.all,Principal.ClinicalAssistant);
 	Principal.InitPrincipalForRole(Patient.all,Principal.Patient);
   	KnownPrincipals(0) := Card;
	KnownPrincipals(1) := Clin;
	KnownPrincipals(2) := Patient;
   
	Put("Known Principals: "); New_Line;
	Principal.DebugPrintPrincipalPtr(Card); New_Line;
	Principal.DebugPrintPrincipalPtr(Clin); New_Line;
	Principal.DebugPrintPrincipalPtr(Patient); New_Line;
	New_Line; 
	Put("-------------------------------------------"); New_Line;
	Put("               NEW SESSION"); New_Line;
	Put("-------------------------------------------"); New_Line; New_Line;

	-- Initialise the components and turn the machines on
	Heart.Init(Hrt);
	HRM.Init(Monitor);
	ICD.Init(ICDSoftware);
	ImpulseGenerator.Init(Generator);
	Network.Init(Net,KnownPrincipals);

	-- Set the new impulse to 0
	ImpulseGenerator.SetImpulse(Generator, 0);

	--Set the History to 0
	for M in Integer range History'First..History'Last loop
		History(M).Rate := 0;   			
	end loop;

end Init;

procedure Tick is
	AuthorisedPrinciple : Boolean := false;
	RTachyBound : BPM;
	RJoulesToDeliver : Joules;
begin
	-- Get new message and print them out
	Network.GetNewMessage(Net, MsgAvailable, Msg);
	
	if MsgAvailable then
		-- First check if it is an authorised principle
		 for I in Integer range KnownPrincipals'First..KnownPrincipals'Last
		    loop
		 	if (Principal.PrincipalPtrToString(KnownPrincipals(I)) =
		 		Principal.PrincipalPtrToString(Card))
		 	or else (Principal.PrincipalPtrToString(KnownPrincipals(I)) =
		 			 Principal.PrincipalPtrToString(Clin))
		 	or else (Principal.PrincipalPtrToString(KnownPrincipals(I)) =
		 			 Principal.PrincipalPtrToString(Patient))
		 	then
		 		AuthorisedPrinciple := true;
		 	end if;
		 end loop;

		 if AuthorisedPrinciple then
			-- Determine type of message and action accordingly
			case Msg.MessageType is
				when Network.ModeOn =>
			  		ICD.On(ICDSoftware);
			  		HRM.On(Monitor, Hrt);
					ImpulseGenerator.On(Generator);
					Network.DebugPrintMessage(Msg); 
			  	when Network.ModeOff =>
					ICD.Off(ICDSoftware);
					HRM.Off(Monitor);
					ImpulseGenerator.Off(Generator);
					Network.DebugPrintMessage(Msg);
				when Network.ReadRateHistoryRequest =>
			   		if ICDSoftware.IsModeOn then
						Put("REQUEST NOT ALLOWED: Turn Off the ICD first.");
						New_Line;
					else
						Network.DebugPrintMessage(Msg); 
						Network.SendMessage(Net,(MessageType => 
											Network.ReadRateHistoryResponse,
			        						HDestination => Msg.HSource,
			        						History => History));		  			   			  					   			  			   			  			
						end if;
			  	when Network.ReadSettingsRequest =>
			  		if ICDSoftware.IsModeOn then
			  			Put("REQUEST NOT ALLOWED: Turn Off the ICD first");
			  			New_Line;
			  		else
  			            Put("ReadSettingsRequest (RSource: ");
			            Principal.DebugPrintPrincipalPtr(Msg.RSource);
			            Put(")"); New_Line;
			  			ICD.ReadSettingsRequest(ICDSoftware, RTachyBound, 
			  									RJoulesToDeliver);
			  			Put("ReadSettingsResponse (RDestination: ");
						Principal.DebugPrintPrincipalPtr(Msg.RSource);
						Put(");");
						New_Line;
						Put("TachyBound: ");
						Ada.Integer_Text_IO.Put(Integer(RTachyBound));
						Put("; JoulesToDeliver: ");
						Ada.Integer_Text_IO.Put(Integer(RJoulesToDeliver));
						New_Line; New_Line;
			  		end if;
			  	when Network.ChangeSettingsRequest =>
			  		if ICDSoftware.IsModeOn then
			  			Put("REQUEST NOT ALLOWED: Turn off the ICD first");
			  			New_Line;
			  		else
						Put("ChangeSettingsRequest (CSource: ");
						Principal.DebugPrintPrincipalPtr(Msg.CSource);
						Put(")"); New_Line;
			  			ICD.ChangeSettingsRequest(ICDSoftware, Msg.CTachyBound, 
			  									  Msg.CJoulesToDeliver);
						Put("ChangeSettingsResponse (CDestination: ");
						Principal.DebugPrintPrincipalPtr(Msg.CSource);
						Put(")"); New_Line;
						Put("NewTachyBound: ");
			       		Ada.Integer_Text_IO.Put(Integer(Msg.CTachyBound));
			       		Put("; NewJoulesToDeliver: ");
			       		Ada.Integer_Text_IO.Put(Integer(Msg.CJoulesToDeliver));
			  			New_Line;  New_Line;
			  		end if;
			  	when others =>	
				  		null;
			end case;
		 end if;
	end if;

	if ICDSoftware.IsModeOn then
		-- Read and print the current measured heart rate
		HRM.GetRate(Monitor, HeartRate);
		Put("Measured heart rate  = ");
		Put(Item => HeartRate);
		New_Line;

		-- record history
		if HistoryPos <= History'Last then
			History(HistoryPos) := (Rate => HeartRate, Time => CurrentTime);
			HistoryPos := HistoryPos + 1;
		end if;

		if HistoryPos > History'Last then
			for M in Integer range 1..(History'Last-1) loop
				History(M) := History(M+1);
			end loop;
			History(History'Last) := (Rate => HeartRate, Time => CurrentTime);
		end if;

		ICD.Tick(ICDSoftware, Generator, Monitor, History);
		ImpulseGenerator.Tick(Generator, Hrt);
		HRM.Tick(Monitor, Hrt);
	end if;

	Heart.Tick(Hrt);
	Network.Tick(Net);
	CurrentTime := CurrentTime + 1;

end Tick;

end  ClosedLoop;