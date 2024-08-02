#lang peg-parser

A <-- '(' B* ')' A / 'f' ;
B <-- 'b' / ! 'c';
start: A !.