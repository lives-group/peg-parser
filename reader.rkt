#lang racket

(require "grammar.rkt"
         "peg-ast.rkt"
         "peg-wf.rkt")

(provide (rename-out [peg-read read]
                     [peg-read-syntax read-syntax]))

(define (peg-read in)
  (syntax->datum
   (peg-read-syntax #f in)))

(define (peg-read-syntax path port)
  (define grammar (parse port))
  (let ([types (type-infer grammar)])
    (if (not (cdr types))
        (error "The grammar isn't well-typed! It can loop on some inputs.")
        (datum->syntax
         #f
         `(module peg-parser racket
            (provide run-parse
                     run-non-verbose-parse
                     list
                     (all-from-out peg-parser/peg-ast))

            (require peg-parser/peg-recognizer
                     peg-parser/peg-ast
                     )
            (define grm ,grammar)
            (define (run-parse s)
              (peg-parse grm (open-input-string s)))
            (define (run-non-verbose-parse s)
              (simplified-peg-parse grm (open-input-string s)))
            (define (list)
              (peg->string grm))

            ))
    )
  ))