with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Real_Time;  use Ada.Real_Time;
with System;

with Webots_API;   use Webots_API;

package body Tasks is
  -------------
  --  Objects  --
  -------------   
  protected Event is
      entry Wait (id : out EventID);
      procedure Signal (id : in EventID);
  private
    -- assign priority that is ceiling of the
    -- user tasks ’ priorities
      current_id : EventID; -- event data
      signalled : Boolean := false; -- flag for event signal
  end Event;

  protected body Event is
      entry Wait (id : out EventID) when Signalled is
      begin
        id := current_id;
        signalled := false;
      end Wait;

      procedure Signal (id : in EventID) is
      begin
        current_id := id;
        signalled := true;
      end Signal;
  end Event;

  -------------
  --  Tasks  --
  -------------   
  task EventDispatcherTask is
    pragma Priority (System.Priority'First);
  end EventDispatcherTask;

  task MotorControlTask is
    pragma Priority (System.Priority'Last);
  end MotorControlTask;

  task body EventDispatcherTask is    
    Current_Button : EventID;
    Previous_Button : EventID;
    Line_Detection : Integer := 0;
    On_Line : Boolean := False;
  begin

    loop

      -- Button press detection
      if button_pressed(UpButton) then 
        if button_pressed(RightButton) then Current_Button := UpRightPressed;
        elsif button_pressed(LeftButton) then Current_Button := UpLeftPressed;
        else Current_Button := UpPressed;
        end if;
      elsif button_pressed(DownButton) then 
        if button_pressed(RightButton) then Current_Button := DownRightPressed;
        elsif button_pressed(LeftButton) then Current_Button := DownLeftPressed;
        else Current_Button := DownPressed;
        end if;
      elsif button_pressed(LeftButton) then Current_Button := LeftPressed;
      elsif button_pressed(RightButton) then Current_Button := RightPressed;
      else Current_Button := NonePressed;
      end if;
      
      -- Send event signal if currently pressed button is different than the previous state
      if Current_Button /= Previous_Button then 
        Event.Signal(Current_Button);
      end if;
      Previous_Button := Current_Button;

      -- Line detection with light sensor
      Line_Detection := read_light_sensor(LS2);

      -- Send event signal if line detection state has changed from previous state
      if Line_Detection > 800 and On_Line then
        Event.Signal(BlankDetected);
        On_Line := False;
      elsif Line_Detection <= 800 and not On_Line then
        Event.Signal(LineDetected);
        On_Line := True;
      end if;

      delay 0.01;
    end loop;
  end EventDispatcherTask;


  task body MotorControlTask is
    Received_Event : EventID;
    Motor_Speed : Integer := 400;
    Left_M : Integer := 0;
    Right_M : Integer := 0;
    Can_Move : Boolean := True;
  begin
    loop
      Event.Wait(Received_Event);

      -- If a line is detected, halt the car and signal current state with Can_Move flag
      -- Once no line is detected, allow the car to resume by changing state of the Can_Move flag
      if Received_Event = LineDetected then
        Can_Move := False;
        Left_M := 0;
        Right_M := 0;
        Put_Line("Car stopped, line detected");
      elsif Received_Event = BlankDetected then
        Can_Move := True;
        Put_Line("Car can resume, no line detected");
      end if;

      -- While car is in the blank area and buttons are being pressed, receive event signal 
      -- and move accordingly
      if Can_Move then
          if Received_Event = UpPressed then
            Left_M := Motor_Speed;
            Right_M := Motor_Speed;   
            Put_Line("Up");
          elsif Received_Event = DownPressed then
            Left_M := -Motor_Speed;
            Right_M := -Motor_Speed;
            Put_Line("Down");
          elsif Received_Event = LeftPressed then
            Left_M := -Motor_Speed;
            Right_M := Motor_Speed;
            Put_Line("Left");
          elsif Received_Event = RightPressed then
            Left_M := Motor_Speed;
            Right_M := -Motor_Speed;
            Put_Line("Right");
          elsif Received_Event = UpRightPressed then
            Left_M := Motor_Speed;
            Right_M := Motor_Speed/2;
            Put_Line("UpRight");
          elsif Received_Event = UpLeftPressed then
            Left_M := Motor_Speed/2;
            Right_M := Motor_Speed;
            Put_Line("UpLeft");
          elsif Received_Event = DownRightPressed then
            Left_M := Motor_Speed/2;
            Right_M := -Motor_Speed;
            Put_Line("DownRight");
          elsif Received_Event = DownLeftPressed then
            Left_M := -Motor_Speed;
            Right_M := Motor_Speed/2;
            Put_Line("DownLeft");
          elsif Received_Event = NonePressed then
            Left_M := 0;
            Right_M := 0;
            Put_Line("Stop");
          end if;
       end if;
      
       set_motor_speed(LeftMotor, Left_M);
       set_motor_speed(RightMotor, Right_M); 
    end loop;
  end MotorControlTask;


  -- Background procedure required for package
  procedure Background is begin
    while not simulation_stopped loop
      delay 0.25;
    end loop;
  end Background;

end Tasks;
