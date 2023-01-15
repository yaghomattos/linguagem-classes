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

let ca = new teste(35)
   in begin
     send ca a(5);
     send ca gety()
   end
