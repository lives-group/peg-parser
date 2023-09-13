#lang peg-parser

Exp  <-- ^Term ( ('+' / '-') ^Term)*;

Term  <--  ^Factor ( ('*' / '/') ^Factor )*;

Factor <-- ^Number
         / '(' Exp ')';

Number <-- ~(['0'-'9'] +) ;

start: Exp