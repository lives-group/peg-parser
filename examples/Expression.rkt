#lang peg-parser

Exp  <--  Term Term1;
Term <-- Number Term1;

Term1 <-- '+' Term Term1 
        / '-' Term Term1
        /  epsilon;

Number <-- Digit+;
Digit  <-- ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];

start: Exp