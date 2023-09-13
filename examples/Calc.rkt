#lang racket
(require "Expression.rkt")

(define (calc t)
   (match t
     [(PTVar "Exp" e)     (calc e)]
     [(PTList (list n xs)) ()])
     [(PTList (list #\+ n)) ()]
     [(PTList (list #\- n)) ()]
     
  )



  