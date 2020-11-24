with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Real_Time;  use Ada.Real_Time;
with System;

with Webots_API;   use Webots_API;

package body Tasks is
  -------------
  --  Objects  --
  -------------   
  protected SharedData is
      procedure set_motor_speed (speed : Integer);
      procedure set_direction (dir : Directions);
      procedure set_distance (dist : Integer);
      function get_motor_speed return Integer;
      function get_direction return Directions;
      function get_distance return Integer;
    private
      -- Data goes here
      Motor_Speed : Integer := 0;
      Direction : Directions;
      Distance : Integer;
  end SharedData;

  protected body SharedData is
      -- procedures can modify the data
      procedure set_motor_speed (speed : Integer) is
      begin
        Motor_Speed := speed ;
      end set_motor_speed;

      procedure set_direction (dir : Directions) is
      begin
        Direction := dir;
      end set_direction;

      procedure set_distance (dist : Integer) is
      begin
        Distance := dist;
      end set_distance;

      -- functions cannot modify the data
      function get_motor_speed return Integer is
      begin
        return Motor_Speed;
      end get_motor_speed;

      function get_direction return Directions is
      begin
        return Direction;
      end get_direction;

      function get_distance return Integer is
      begin
        return Distance;
      end get_distance;
  end SharedData ;

  -------------
  --  Tasks  --
  -------------   
  task MotorControlTask is
    
  end MotorControlTask;

  task LineFollowingTask is
    
  end LineFollowingTask;

  task DistanceTask is
    
  end DistanceTask;

  task DisplayTask is
    
  end DisplayTask;


  -- Sets speeds of the motors
  task body MotorControlTask is
    Direction : Directions;
    Distance : Integer;
    Motor_Speed : Integer := 400;
    Left_M : Integer := 0;
    Right_M : Integer := 0;
    Next_Time : Time := Time_Zero;
  begin
    loop
    -- Speed
      Distance := SharedData.get_distance;
      if Distance > 70 then Motor_Speed := 0;
      elsif Distance > 65 then Motor_Speed := 200;
      else Motor_Speed := 400;
      end if;

      SharedData.set_motor_speed(Motor_Speed);

    -- Direction 
      Direction := SharedData.get_direction;
      if Direction = Straight then 
          Left_M := Motor_Speed;
          Right_M := Motor_Speed;
      elsif Direction = Right then
          Left_M := Motor_Speed*2;
          Right_M := Motor_Speed;
      elsif Direction = Left then
          Left_M := Motor_Speed;
          Right_M := Motor_Speed*2;
      end if;

      set_motor_speed(LeftMotor, Left_M);
      set_motor_speed(RightMotor, Right_M); 

      Next_Time := Next_Time + Period_MotorControl;
      delay until Next_Time;
    end loop;
  end MotorControlTask;


  -- Reads light sensors, sends commands to MotorControl
  task body LineFollowingTask is
    Next_Time : Time := Time_Zero;
    Line_Detection : Integer;
  begin
    loop
      Line_Detection := read_light_sensor(LS2);

      if Line_Detection > 800 then
        -- Blank detected, find line
        if read_light_sensor(LS1) <= 800 then SharedData.set_direction(Left);
        elsif read_light_sensor(LS3) <= 800  then SharedData.set_direction(Right);
        end if;
      elsif Line_Detection <= 800 then
        SharedData.set_direction(Straight);
      end if;
      Next_Time := Next_Time + Period_LineFollowing;
      delay until Next_Time;
    end loop;
  end LineFollowingTask;

  
  -- Reads distance sensor and sends commands to MotorCOntrol
  task body DistanceTask is
      Next_Time : Time := Time_Zero;
      Obstacle_Distance : Integer;
  begin
    loop
      Obstacle_Distance := read_distance_sensor;
      SharedData.set_distance(Obstacle_Distance);
      Next_Time := Next_Time + Period_Distance;
      delay until Next_Time;
    end loop;
  end DistanceTask;


  -- Displays status information 
  task body DisplayTask is
      Next_Time : Time := Time_Zero;
  begin
    loop
      if SharedData.get_direction = Straight then Put_Line("Car is going straight");
      elsif SharedData.get_direction = Right then Put_Line("Car is turning right");
      elsif SharedData.get_direction = Left then Put_Line("Car is turning left");
      end if;

      Put_Line("Speed is " & Integer'Image(SharedData.get_motor_speed));
      Put_Line("Distance is " & Integer'Image(SharedData.get_distance));

      Next_Time := Next_Time + Period_Display;
      delay until Next_Time;
    end loop;
  end DisplayTask;


  -- Background procedure required for package
  procedure Background is begin
    while not simulation_stopped loop
      delay 0.25;
    end loop;
  end Background;

end Tasks;
