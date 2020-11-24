# Lab 2
## Part 1: Part 1: Hello Webots!
This first part is supposed to get you used to compiling and running programs, together with simple input/output operations using the Webots API.
The program you will write will print the sensed light value on the console.

## Part 2: Event–driven scheduling
In this part, you will learn how to program event–driven schedules. The target application will be a Webots robot that can be manually controlled using the keyboard directional keys (as long as at least one of the keys
is pressed) and while it is not over a black line (in which case it should immediately stop). The robot must not be able to be driven past a black line. You are free to come up with an implementation that comes to a total halt as soon as
the black line is detected, or an alternative implementation where it could be driven in the opposite direction (but not past the line).

## Part 3: Line Tracker
In the last part you need to create a robot that can simultaneously:
• Follow the black line that is drawn on the floor using the light sensors.
• Keep a constant distance to detected objects in front of it using the distance sensors.
