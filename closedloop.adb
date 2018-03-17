with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings;
-- use Ada.Text_Io, ada.strings.unbounded;
-- with Ada.Streams.Stream_Element_Array; use Ada.Streams.Stream_Element_Array;

with Measures; use Measures;
with Heart;
with HRM;
with ImpulseGenerator;
with Network;
with Principal;
with ICD;

package body closedloop is
	
	procedure Init(closedloop: out closedloopType) is

		Hrt : Heart.HeartType;                -- The simulated heart
   		Monitor : HRM.HRMType;                -- The simulated heart rate monitor
   		Generator : ImpulseGenerator.GeneratorType; -- The simulated generator
   		-- HeartRate : BPM;
   		ICDSoftware :ICD.ICDType;
   		Net : Network.Network;                -- The simulated network
   		Card : Principal.PrincipalPtr := new Principal.Principal;  -- A cardiologist
   		Clin : Principal.PrincipalPtr := new Principal.Principal;  -- A clinical assistant
   		Patient : Principal.PrincipalPtr := new Principal.Principal; -- A patient
   		-- -- stores whether there was a message available on the network
   		MsgAvailable : Boolean := False;
   		-- Messagecl : MessageType;
   -- -- stores the current message read from the network (if one was available)
   		Msg : Network.NetworkMessage;
   		-- Chut : String;
   		
   -- -- stores some history information on measured heart rate
   		History : Network.RateHistory;
   		-- an array of known principals to use to initialise the network
   		-- but note that the network can generate messages from other, unknown,
   		-- principals too
   		KnownPrincipals : access Network.PrincipalArray := new Network.PrincipalArray(0..2); 
   		NewTachyBound : Integer;
   		NewVentFibrilJoulesToDeliver : Integer;
		begin -- Init
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
   
   		-- Initialise the components and turn the machines on
   		Heart.Init(Hrt);
   		HRM.Init(Monitor);
   		ICD.Init(ICDSoftware);
   		ImpulseGenerator.Init(Generator);
   		Network.Init(Net,KnownPrincipals);
   
   		HRM.On(Monitor, Hrt);
   		
   		ImpulseGenerator.On(Generator);
   
   		-- Set the new impulse to 0
   		ImpulseGenerator.SetImpulse(Generator, 0);

   		--Set the History to 0
   		for M in Integer range History'First..History'Last loop
   				History(M).Rate := 0;   			
   		end loop;

   		--Starting the process
   		for I in Integer range 0..20 loop
   			-- read messages from the network but don't act on them here,
      		-- just print them out
	      	Network.GetNewMessage(Net,MsgAvailable,Msg);
	      	if MsgAvailable then
	        	Network.DebugPrintMessage(Msg);
	      	end if;
   			-- Chut := Msg;
   			case Msg is
   				when ModeOn =>
   			  		-- ICD.On(ICDSoftware, Generator, Monitor, Net);
   			  		ICD.On(ICDSoftware);

   			  	when ModeOff =>
					--Will allow network user to change the setting
					ICD.Off(ICDSoftware);
					Put("ICD is now off."); New_Line;
				when ReadRateHistoryRequest =>
				   	if ICDSoftware.IsModeOn then
						Put("Turn Off the ICD first.");
					else
						Put("ReadRateHistoryResponse (HDestination: ");
				        Principal.DebugPrintPrincipalPtr(Message.HDestination);
				        Put("; History: "); 
              	
            			Network.SendMessage(Net,(MessageType => Network.ReadRateHistoryResponse,
                        					HDestination => Message.HSource,
                        					History => History));		  			   			  					   			  			   			  			
			  		end if;
			  	when ReadSettingsRequest =>
			  		if ICDSoftware.IsModeOn then
			  			Put("Turn Off the ICD first");
			  		else
			  			Put("ReadSettingResponse (HDestination: ");
			  			Principal.DebugPrintPrincipalPtr(Message.HDestination);
			  			Put("; TachyBound: ");
			  			Put(ICDSoftware.TachyBound);
			  			Put("; Ventrical Fibrillation: ");
			  			Put(ICDSoftware.VentFibrilJoulesToDeliver); New_Line;
			  		end if;
			  	when ChangeSettingsRequest =>
			  		if ICDSoftware.IsModeOn then
			  			Put("Turn off the ICD first");
			  		else
			  			Put("Please enter the upper bound of Tachy: ");
			  			Get(NewTachyBound);
			  			Put("Please enter the joules to deliver in Ventrical Fibrillation:  ");
			  			Get(NewVentFibrilJoulesToDeliver);
			  			ICD.ChangeSettingsRequest(ICDSoftware, NewTachyBound, NewVentFibrilJoulesToDeliver);
			  			Put("Settings are updated!!"); New_Line;
			  		end if;
			  	when others =>	
   			  		null;
   			end case;  
		      
   		end loop;

	end Init;
	
end closedloop;