with Ada.Real_Time;       use Ada.Real_Time;
-- Add required sensor and actuator package --

package Tasks is
  procedure Background;
private

  --  Define periods and times  --  
  Period_Sensors : Time_Span := Milliseconds(10); 
  Time_Zero      : Time := Clock;
      
  --  Other specifications  --
  type EventID is (UpPressed, DownPressed, RightPressed, LeftPressed, UpRightPressed, UpLeftPressed, DownRightPressed, DownLeftPressed, NonePressed, LineDetected, BlankDetected);
end Tasks;
