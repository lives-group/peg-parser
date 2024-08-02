;#lang peg-parser/debug/ConstraintsOnly
#lang peg-parser/debug/outputCTX

A <--  B /'b' A;
B <-- 'a' / C 'c';
C <--  D 'd' / E 'e' / F 'f';
D <-- 'd';
E <-- 'e' ;
F <-- 'f' / 'a' A;
start: epsilon '0'



  