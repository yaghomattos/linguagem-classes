#lang dcc019/iref

let swap = proc (x)
      proc (y)
        let temp = x
        in begin
             set x = y;
             set y = temp
           end
in let a = 33
   in let b = 44
      in begin
           ((swap a) b);
           -(a, b)
         end
