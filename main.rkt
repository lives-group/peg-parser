#lang racket
(module reader racket
  (require "reader.rkt")
  (provide read read-syntax))


(module+ test
   (require rackunit
            rackcheck
            "./test/solver-test.rkt")
 
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (begin (check-property accept-well-typed)
         (check-property reject-ill-typed))
  )

