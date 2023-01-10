#lang dcc019/classes

class teste extends object
   field y
   field x

   method initialize (v)
      begin
        set x = v
      end

   method a(z)
      begin
         set y = -(x, z)
      end

   method gety() y

let o1 = new teste(3)
   in begin
     send ol a(5);
     send ol gety()
   end