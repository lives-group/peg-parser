#lang peg-parser

A <--  'a' A 'b'/ 'a' 'b' ;

start: A