#lang info
(define collection "peg-parser")
(define deps '("base"
               "parser-tools-lib"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "rackunit-lib"))
;(define scribblings '(("scribblings/peg-parser.scrbl" ())))
(define pkg-desc "A parser library based on peg")
(define version "0.1")
(define pkg-authors '(elton ))
(define license '(MIT))