#lang dcc019/exercise/imp

var x;
{
 x = 3;
 print x;
 var x;
 {
  x = 4;
  print x
 };
 print x
}
