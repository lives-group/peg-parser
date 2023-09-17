#lang racket
(require "Expression.rkt")

(define (calc e)
   (match e
    [(PTVar "Exp" (PTList (cons t xs))) (calc-exp (calc-term (PTList-xs (PTVar-t t))) xs)]
    [(PTVar "Exp" (PTList t))  (calc-term (PTList-xs (PTVar-t t)))]
    [(PTFail) "Oh no!"]
    )
  )

(define (calc-exp v p)
  (match p
    ['() v]
    [(cons (PTSym #\+) (cons t xs)) (calc-exp (+ v (calc-term (PTList-xs (PTVar-t t))))  xs)]
    [(cons (PTSym #\-) (cons t xs)) (calc-exp (- v (calc-term (PTList-xs (PTVar-t t))))  xs)]
    )
  )

(define (calc-term p)
  (match p
    [(cons f xs) (calc-term1 (eval-factor f) xs)]
    [(list f) (eval-factor f)]
    )
  )

(define (calc-term1 v p)
  (match p
    ['() v]
    [(cons  (PTSym #\*) (cons f xs)) (calc-term1 (* v (eval-factor f)) xs)]
    [(cons  (PTSym #\/) (cons f xs)) (calc-term1 (/ v (eval-factor f)) xs)]
    )
  )

(define (eval-factor p)
  (match p
    [(PTStr s) (string->number s)]
    [(PTVar "Exp" _)  (calc p)]
    )
  )

(define (run)
   (let ([inp (read-line)] )
        (calc (run-parse inp)) 
   )
 )
