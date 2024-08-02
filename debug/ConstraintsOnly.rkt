#lang racket

(module reader racket
  
(require "../grammar.rkt"
         "../peg-ast.rkt"
         "../peg-wf.rkt"
         racket/path
         racket/syntax
         )

(provide (rename-out [peg-read read]
                     [peg-read-syntax read-syntax]))
(define (peg-read in)
  (syntax->datum
   (peg-read-syntax #f in)))

(define (peg-read-syntax path port)
  (define grammar (parse port))
  (let ([ctx (peg->constraints grammar)] )
    (datum->syntax
     #f
     `(module peg-parser racket
         (require "../peg-wf.rkt")        
        ,(displayln `"---- Constraint List ----" )
        ,(displayln (ctx-constr-toString ctx))
        ,(displayln `"----Unsolved Nts ----" )
        ,(displayln (unsolved-nts  ctx))
        ,(displayln `"----Type Env ----" )
        ,(displayln (ctx-tyEnv-toString  ctx))
        ))))
  )