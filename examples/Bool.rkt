#lang peg-parser

Consts <-- -'T' ~W
         / -'F' ~W;
OR  <-- '\|'~W; 
AND <-- '^' ~W;
AP  <-- '(' W;
FP  <-- ')' W;

Formula  <-- Ands ( ^OR Ands)*;
Ands <-- ^Not ( ^AND ^Not)*;

Not <-- '~' (^Consts / ~AP Formula ~FP ) / ^Consts;

W <-- [' ', '\n', '\t']*;

start: Formula