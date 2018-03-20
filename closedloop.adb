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
	MAX_TACHYBOUND : constant Integer := (Measures.MAX_BPM - 
										  ICD.TACHY_RATECHANGE);

	Hrt : Heart.HeartType;                -- The simulated heart
	Monitor : HRM.HRMType;                -- The simulated heart rate monitor
	Generator : ImpulseGenerator.GeneratorType; -- The simulated generator
	HeartRate : BPM;
	ICDSoftware :ICD.ICDType;
	Net : Network.Network;                -- The simulated network
	Card : Principal.PrincipalPtr;   	  -- A cardiologist
	Clin : Principal.PrincipalPtr;   	  -- A clinical assistant
	Patient : Principal.PrincipalPtr; 	  -- A patient
	-- stores whether there was a message available on the network
	MsgAvailable : Boolean;
	-- stores the current message read from the network (if one was available)
	Msg : Network.NetworkMessage;
	-- stores some history information on measured heart rate
	History : Network.RateHistory;
	HistoryPos : Integer := History'First;
	-- array to store last 6 heart beats to
	-- calculate average for ventrical fibrillation
	VentFibriHistory : ICD.VentFibrilHistoryType;
	-- stores position of pointer of array ventfibrihistory  
	VentFibriHistoryPos : Integer := VentFibriHistory'First;
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
		Put("-------------------------------------------"); New_Line;

		-- Initialise the components and turn the machines on
		Heart.Init(Hrt);
		HRM.Init(Monitor);
		ICD.Init(ICDSoftware);
		ImpulseGenerator.Init(Generator);
		Network.Init(Net,KnownPrincipals);

		-- Set the new impulse to 0
		ImpulseGenerator.SetImpulse(Generator, 0);

		--Set the History to 0 to avoid garbage value in the array
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
				New_Line;
				-- Determine type of message and action accordingly
				case Msg.MessageType is
					when Network.ModeOn =>
				  		ICD.On(ICDSoftware);
				  		HRM.On(Monitor, Hrt);
						ImpulseGenerator.On(Generator);
						Network.DebugPrintMessage(Msg); 
						New_Line;
				  	when Network.ModeOff =>
				  		Network.DebugPrintMessage(Msg);
				  		-- Only allow when patient is not getting treated for
				  		-- tachycardia
						if ICDSoftware.IsTachy = false then
							ICD.Off(ICDSoftware);
							HRM.Off(Monitor);
							ImpulseGenerator.Off(Generator);
						else 
							Put("REQUEST NOT ALLOWED: Patient is ");
							Put("still being treated for tachycardia");
							New_Line;
							New_Line;
						end if;
					when Network.ReadRateHistoryRequest =>
				   		Network.DebugPrintMessage(Msg); 
				   		if ICDSoftware.IsModeOn then
							Put("REQUEST NOT ALLOWED: Turn Off the ICD first");
							New_Line;
							New_Line;
						else
							Network.SendMessage(Net,(MessageType => 
											   Network.ReadRateHistoryResponse,
				        					   HDestination => Msg.HSource,
				        					   History => History));		  			   			  					   			  			   			  			
							end if;
				  	when Network.ReadSettingsRequest =>
				  		-- Prnt directly as Network doesn't
			  			-- have debugging functions for MessageType
  			            Put("ReadSettingsRequest (RSource: ");
			            Principal.DebugPrintPrincipalPtr(Msg.RSource);
			            Put(")"); 
			            New_Line;

				  		if ICDSoftware.IsModeOn then
				  			Put("REQUEST NOT ALLOWED: Turn Off the ICD first");
				  			New_Line;
				  			New_Line;
				  		else
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
							New_Line; 
				  		end if;
				  	when Network.ChangeSettingsRequest =>
					  	-- Print directly as Network doesn't
			  			-- have debugging functions for MessageType
						Put("ChangeSettingsRequest (CSource: ");
						Principal.DebugPrintPrincipalPtr(Msg.CSource);
						Put(")"); 
						New_Line;
				  		if ICDSoftware.IsModeOn then
				  			Put("REQUEST NOT ALLOWED: Turn off the ICD first");
				  			New_Line;
				  			New_Line;
				  		else
							Msg.CTachyBound := MAX_TACHYBOUND+1;
							if Msg.CTachyBound > MAX_TACHYBOUND
							   or Msg.CTachyBound < Measures.MIN_BPM
							   or Msg.CJoulesToDeliver > Measures.MAX_JOULES
							   or Msg.CJoulesToDeliver < Measures.MIN_JOULES
							   then
								Put("REQUEST NOT ALLOWED: ");
								Put("Settings need to be within range");
								New_Line;
								if (Msg.CTachyBound > MAX_TACHYBOUND) then
					  				Put("CTachyBound needs to be 15BPM below ");
					  				Put("MAX_BPM to ensure that impulses can ");
					  				Put("be sent if tachycardia is detected");
					  				New_Line;
					  			end if;
							else 
					  			ICD.ChangeSettingsRequest(ICDSoftware, 
					  									 Msg.CTachyBound, 
					  									 Msg.CJoulesToDeliver);
								Put("ChangeSettingsResponse (CDestination: ");
								Principal.DebugPrintPrincipalPtr(Msg.CSource);
								Put(")"); 
								New_Line;
								Put("NewTachyBound: ");
					       		Ada.Integer_Text_IO.Put(Integer(
					       								Msg.CTachyBound));
					       		Put("; NewJoulesToDeliver: ");
					       		Ada.Integer_Text_IO.Put(Integer(
					       								Msg.CJoulesToDeliver));
					  			New_Line;  
					  		end if;
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

			--storing last 6 heart beats only
			--this works when array is full
			if VentFibriHistoryPos > VentFibriHistory'Last then
				for M in Integer range 1..(VentFibriHistory'Last-1) loop
					VentFibriHistory(M) := VentFibriHistory(M+1);
				end loop;
				--adding latest heart beat to the array
				VentFibriHistory(VentFibriHistory'Last) := 
				(Rate => HeartRate, Time => CurrentTime);
			end if;

			-- record last 5 history to send it on network
			-- this works only when array is not full
			if HistoryPos <= History'Last then
				History(HistoryPos) := 
				(Rate => HeartRate, Time => CurrentTime);
				HistoryPos := HistoryPos + 1;
			end if;

			--record history for ventrical fibrillation
			--this works when array is not full
			if VentFibriHistoryPos <= VentFibriHistory'Last then
				VentFibriHistory(VentFibriHistoryPos) := 
				(Rate => HeartRate, Time => CurrentTime);
				VentFibriHistoryPos := VentFibriHistoryPos + 1;
			end if;

			--stores history of last 5 heart beats
			--this works when array is full
			if HistoryPos > History'Last then
				for M in Integer range 1..(History'Last-1) loop
					History(M) := History(M+1);
				end loop;
				--adding latest heart beat to the array
				History(History'Last) := 
				(Rate => HeartRate, Time => CurrentTime);
			end if;

			ICD.Tick(ICDSoftware, Generator, Monitor, VentFibriHistory);
			ImpulseGenerator.Tick(Generator, Hrt);
			HRM.Tick(Monitor, Hrt);
		end if;

		Heart.Tick(Hrt);
		Network.Tick(Net);
		CurrentTime := CurrentTime + 1;

	end Tick;

end  ClosedLoop;