--Cyclic scheduler with a watchdog: 

with Ada.Calendar;
with Ada.Text_IO;

use Ada.Calendar;
use Ada.Text_IO;

-- add packages to use randam number generator
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
--use Ada.Numerics.Discrete_Random;
use Ada.Numerics.Float_Random;


procedure cyclic_wd is
    Message: constant String := "Cyclic scheduler with watchdog";
    -- change/add your declarations here
    d: Duration := 1.0;
	Start_Time: Time := Clock;
	Relative_Time: Time := Start_Time;
	half_d: Duration := 0.5;
	Odd: Boolean := False;

	gen: Generator;
	random_delay: Float;
	extra_work: Duration := 3.0;

	f3Start: Time;
	f3_duration: Duration;
        

	procedure f1 is 
		Message: constant String := "f1 executing, time is now";
	begin
		Put(Message);
		Put_Line(Duration'Image(Clock - Start_Time));
	end f1;

	procedure f2 is 
		Message: constant String := "f2 executing, time is now";
	begin
		Put(Message);
		Put_Line(Duration'Image(Clock - Start_Time));
	end f2;

	procedure f3 is 
		Message: constant String := "f3 executing, time is now";
	begin
		Put(Message);
		Put_Line(Duration'Image(Clock - Start_Time));
		-- add a random delay here
		-- occasionally takes more than 0.5 seconds to execute
		random_delay := random(gen);
		if random_delay > 0.7 then --30% chance
			delay extra_work;
		end if;
		--Put_Line("f3 has finished"& Duration'Image(Clock - Start_Time));

	end f3;
	
	task Watchdog is
	       -- add your task entries for communication 	
		entry start;
		entry stop;
	end Watchdog;

	task body Watchdog is 
	begin 
    loop 
      accept start do
	  	null;
      	--Put_Line("Starting watchdog for f3");   
      end start;
  
      select
        delay 0.5;
        
        Put_Line("Watchdog: f3 exceeded 0.5s deadline");
        --Put_Line(Duration'Image(Clock - Start_Time));
        
        accept stop do
			null;
          	--Put_Line("f3 finished");    
        end stop;
      or   
        accept stop do
			null;
          --Put_Line("f3 finished");
        end stop;
      end select;
    end loop;
  end Watchdog;

	begin
		reset(gen);
        loop
            f1;
            f2;
			
			Odd:= not Odd;
			delay until (Relative_Time + half_d);
            if Odd then
				f3Start:= Clock;
				Watchdog.start;
            	f3;
				Watchdog.stop;
				f3_duration := Clock - f3Start;
				if (f3_duration) > half_d then -- if it exceeded the deadline
					Relative_Time := Relative_Time + Duration(Float'Ceiling(Float(f3_duration)));
					delay until Relative_Time;
				else
					Relative_Time := Relative_Time + d;
					delay until (Relative_Time);  
				end if;
			else
				Relative_Time := Relative_Time + d;
				delay until (Relative_Time);    
			end if;


        end loop;

end cyclic_wd;

