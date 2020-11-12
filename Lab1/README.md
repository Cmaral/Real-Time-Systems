# Lab 1
Part 1: Cyclic scheduler
Write a cyclic scheduler which manages three procedures F1, F2, and F3. We demand the following for the executions:

F1 shall be executed every second 
F2 starts when F1 terminates
F3 shall execute every other second, starting 0.5 seconds after F1's start 
The execution times for the functions are not known. However, you can assume that F1 and F2 together execute for less than 0.5 seconds, and that F3 does not take more than 0.5 seconds to execute. 
