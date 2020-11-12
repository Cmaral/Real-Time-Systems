# Lab 1
## Part 1: Cyclic scheduler
Write a cyclic scheduler which manages three procedures F1, F2, and F3. We demand the following for the executions:

F1 shall be executed every second 
F2 starts when F1 terminates
F3 shall execute every other second, starting 0.5 seconds after F1's start 
The execution times for the functions are not known. However, you can assume that F1 and F2 together execute for less than 0.5 seconds, and that F3 does not take more than 0.5 seconds to execute. 

## Part 2: Cyclic scheduler with watch-dogs
Modify F3 from part 1 so that it occasionally takes more than 0.5 seconds to execute. 

Augment the cyclic scheduler from part 1 with a watchdog task to monitor F3's execution time. When F3 exceeds its deadline (0.5s), the watchdog task should immediately print a warning message. I.e., 0.5s after start of F3, either F3 has finished or the watchdog has printed a message. The watchdog should let F3 finish, even if it misses its deadline. 

The watchdog task should be started at the same time as (or just before) F3 starts executing, and from that point on measure the time that F3 uses. 

When F3 misses its deadline, the cyclic executive should re-synchronize so that F1 is started at whole seconds. 

## Part 3: Process Communication
Create three tasks:

A task that acts as a first in, first out (FIFO) buffer for integers. The buffer should block any task which makes calls that could overflow or underflow the buffer. The buffer should at least be able to buffer 10 integers.
A task that puts integers in the range 0..20 into the buffer at irregular intervals (i.e. a producer of values).
A task that pulls integers out of the buffer at irregular intervals (i.e. a consumer of values), and summarizes them. When the sum is above 100 the task should terminate the program. The information that the buffer and the producer should terminate should be spread using synchronization (not using global variables, the producer should not count, ...). You are not allowed to use abort or terminate commands.

## Part 4: Data driven sychronization
Re-implement the FIFO buffer in the previous part as a protected shared object.
