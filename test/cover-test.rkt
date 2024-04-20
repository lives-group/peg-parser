#lang racket
(require "solver-test.rkt"
         rackcheck)

(check-property  (make-config #:tests 1000
                              #:deadline (* (+ (current-inexact-milliseconds) 3600000) 4))
                 accept-well-typed)

