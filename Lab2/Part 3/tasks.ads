with Ada.Real_Time;       use Ada.Real_Time;
-- Add required sensor and actuator package --

package Tasks is
  procedure Background;
private

  --  Define periods and times  --  
  Period_MotorControl : Time_Span := Milliseconds(250);
  Period_LineFollowing : Time_Span := Milliseconds(100);
  Period_Distance : Time_Span := Milliseconds(100);
  Period_Display : Time_Span := Milliseconds(500); 
  Time_Zero      : Time := Clock;
      
  --  Other specifications  --
  type Directions is (Straight, Right, Left);
end Tasks;
