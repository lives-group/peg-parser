#lang info
(define collection "peg-parser")
(define deps '("base"
               "parser-tools-lib"
               "rackcheck"
               "peg-gen"
               "rackunit-lib"
               "typed-racket-datatype"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "rackcheck"
                     "rackcheck-lib"
                     "rackunit-lib"
                     "typed-racket-datatype"
                     "peg-gen"))
(define scribblings '(("scribblings/peg-parser.scrbl" ())))
(define pkg-desc "A parser library based on peg")
(define version "0.1")
(define pkg-authors '(Elton Rodrigo Leonardo ))
(define license 'MIT)
