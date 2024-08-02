#lang peg-parser

E <--  Int '+' E / Int;
Int <-- ['0'-'9']+;
start: E



  