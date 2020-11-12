--Process commnication: Ada lab part 3

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;

use Ada.Numerics.Float_Random;
use Ada.Calendar;
use Ada.Text_IO;

procedure comm1 is
	type MyRange is range -1..20;
    
	Message: constant String := "Process communication";
	task buffer is
        entry produce(Item : in MyRange);
		entry consume(Item : out MyRange);
		entry stopBuffer;
	end buffer;

	task producer is
        entry stopProducer;
	end producer;

	task consumer is
            -- add your task entries for communication 
	end consumer;

	-----------
	---- BUFFER
	-----------

	task body buffer is 
		Message: constant String := "buffer executing";
		
		Size : constant Integer := 20;
		Queue : array (0..Size - 1) of MyRange;
		
		Head : Integer  := 0;
		Tail : Integer  := 0;
		Elems : Integer := 0;

		Sum : Integer := 0;

		isDone : Boolean := False;
	begin
		Put_Line(Message);
		-- Initialize buffer
		for I in queue'Range loop
		   queue(I) := -1;
		end loop;

		loop
			select
				when Elems < Size =>
					accept produce(Item : in MyRange) do
						Queue(Tail) := Item;
						Elems := Elems + 1;
						Tail := (Tail + 1) mod Size;
						-- for I in queue'Range loop
		   				-- 	Put(MyRange'Image(Queue(I)));
						-- end loop;
						-- Put_Line("");
					end produce;
			or
				when Elems > 0 =>
					accept consume(Item : out MyRange) do
						Item := Queue(Head);
						Queue(Head) := -1;
						Elems := Elems - 1;
						Head := (Head + 1) mod Size;
					end consume;
			or
				accept stopBuffer do
					isDone := True;
				end stopBuffer;
			end select;
			exit when isDone;
		end loop;
	end buffer;

	-------------
	---- PRODUCER
	-------------

	task body producer is 
		Message: constant String := "producer executing";
		Seed : Generator;
        isDone : Boolean := False;
		CurrItem : MyRange;
	begin
		Put_Line(Message);
		Reset(Seed);
		loop
            delay Duration(Random(Seed) * 0.01);
			CurrItem := MyRange(Random(Seed) * 20.0);
			buffer.produce(CurrItem);
			Put_Line("Item produced: " & MyRange'Image(CurrItem));
			select
				accept stopProducer do
					isDone := True;
				end stopProducer;
			else
				null;
			end select;
			exit when isDone;
		end loop;
	end producer;

	-------------
	---- CONSUMER
	-------------

	task body consumer is 
		Message: constant String := "consumer executing";
        CurrItem : MyRange := 0;
		Sum : Integer := 0;

		Seed : Generator;
	begin
		Put_Line(Message);
		Reset(Seed);
		Main_Cycle:
		loop
			delay Duration(Random(Seed) * 8.0); 
            buffer.consume(CurrItem);
			Sum := Sum + Integer(CurrItem);
			Put_Line("Item consumed: " & MyRange'Image(CurrItem));
			Put_Line(Integer'Image(Sum));
			exit when Sum >= 100;
		end loop Main_Cycle;
		--Put_Line("Out of the loop");
		producer.stopProducer;
		--Put_Line("Stopped producer");
		buffer.stopBuffer;
		Put_Line("Buffer and producer stopped");

		exception
			  when TASKING_ERROR =>
				Put_Line("Buffer finished before producer");
				Put_Line("Ending the consumer");
	end consumer;
begin
	Put_Line(Message);
end comm1;