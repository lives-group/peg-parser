#lang peg-parser

A <-- 'a' A 'a' / epsilon ;
start: A!.
