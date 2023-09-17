#lang peg-parser

Exp  <-- -W Term (-W ('+' / '-') -W Term)*;

Term  <--  ^Factor (-W ('*' / '/') -W ^Factor )*;

Factor <-- ^Number
         /-'(' -W Exp  -W-')';

Number <-- ~(['0'-'9'] +) ;

W <-- -[' ','\n','\t']*;

start: Exp