#lang peg-parser

S <-- !!(A 'c') 'a'* B !. / !.;
A <-- 'a' A 'b' / epsilon;
B <-- 'b' B 'c' / epsilon;
start: -S