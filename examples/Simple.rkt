#lang peg-parser

A <-- a A / a;
B <-- b B / b;

start: A