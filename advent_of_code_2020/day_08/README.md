# Solutions

For the first problem the solution fits quite nicely. Loop through the code lines and memoizing which lines are visited. When looping to an already visited line, break and return the accumalator.

The solution for the second problem is quick and dirty. In a loop, if line command is "jmp" then change it to "nop". If there is no infinite loop then the problem is solved! Else, try changing next line...
