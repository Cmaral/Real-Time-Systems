--Protected types: Ada lab part 4

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;

use Ada.Numerics.Float_Random;
use Ada.Calendar;
use Ada.Text_IO;

procedure comm2 is
    Message: constant String := "Protected Object";
    type BufferArray is array (0 .. 9) of Integer;
    
	protected  buffer is
		procedure produce(Item : in Integer);
        procedure consume(Item : out Integer);
		function getElems return Integer;
		function getSize return Integer;
	private
		Queue : BufferArray;
		Head : Integer  := 0;
		Tail : Integer  := 0;
		Elems : Integer := 0;
		Size : Integer := 10;
	end buffer;

	task producer is
		entry stopProducer;
	end producer;

	task consumer is
        -- add task entries
	end consumer;


	---------------
	--- BUFFER BODY
	---------------

	protected body buffer is 
		procedure produce(Item : in Integer) is
		begin
			Queue(Tail) := Item;
			Elems := Elems + 1;
			Tail := (Tail + 1) mod Size;
		end produce;

		procedure consume(Item : out Integer) is
		begin
			Item := Queue(Head);
			Queue(Head) := -1;
			Elems := Elems - 1;
			Head := (Head + 1) mod Size;
		end consume;

		function getElems return Integer is
		begin
			return Elems;
		end getElems;
		
		function getSize return Integer is
		begin
			return Size;
		end getSize;
	end buffer;


	-----------------
	--- PRODUCER BODY
	-----------------

    task body producer is 
		Message: constant String := "producer executing";
        Seed : Generator;
        isDone : Boolean := False;
		CurrItem : Integer;
	begin
		Put_Line(Message);
		Reset(Seed);
		loop
            delay Duration(Random(Seed) * 0.05);
			if buffer.getElems < buffer.getSize then
				CurrItem := Integer(Random(Seed) * 20.0);
				buffer.produce(CurrItem);
				Put_Line("Item produced: " & Integer'Image(CurrItem));
			end if;

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


	-----------------
	--- CONSUMER BODY
	-----------------

	task body consumer is 
		Message: constant String := "consumer executing";
        CurrItem : Integer := 0;
		Sum : Integer := 0;

		Seed : Generator;
	begin
		Put_Line(Message);
		Reset(Seed);
		Main_Cycle:
		loop
            delay Duration(Random(Seed));
			if buffer.getElems > 0 then
				buffer.consume(CurrItem);
				Sum := Sum + CurrItem;
				Put_Line("Item consumed: " & Integer'Image(CurrItem));
				Put_Line(Integer'Image(Sum));
			end if;
			exit when Sum >= 100;
		end loop Main_Cycle; 
		producer.stopProducer;
	end consumer;

begin
Put_Line(Message);
end comm2;
