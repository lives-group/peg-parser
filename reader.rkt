#lang racket

(require "grammar.rkt"
         "peg-ast.rkt"
         "peg-wf.rkt")

(provide (rename-out [peg-read read]
                     [peg-read-syntax read-syntax]))

(define (peg-read in)
  (syntax->datum
   (peg-read-syntax #f in)))

(define (msg-loops l)
     (string-append "The following non-terminals are left-recursive: "
                           (string-join l ", ") )
  )
(define (msg-ty-erros l)
  (string-append "The following expressions do not type: "
                 (string-join (map pe->err-string l) "\n")))

(define (error-msgs pl)
  (match pl
    [(TyErr '() ty) (error (msg-ty-erros ty))]
    [(TyErr lp '()) (error (msg-loops lp))]
    [(TyErr lp ty) (error (string-append (msg-loops lp) "\n" (msg-ty-erros ty)))])
  
  )

(define (peg-read-syntax path port)
  (define grammar (parse port))
  (let ([types (infer-types grammar)])
    (if (not (satisfied? types))
        (error-msgs  (get-errors types) )
        (datum->syntax
         #f
         `(module peg-parser racket
            (provide (all-from-out peg-parser/peg-simple-recognizer)
                     list-grammar
                     run-parse
                     run-parse-from
                     (all-from-out peg-parser/peg-ast))

            (require peg-parser/peg-simple-recognizer
                     peg-parser/peg-ast
                     )
            (define grm ,grammar)
            (define (run-parse s)
              (peg-parse grm (open-input-string s)))
            (define (run-parse-from i s)
              (peg-parse-from grm i (open-input-string s)))
            (define (list-grammar)
              (foldr string-append "" (peg->string grm)))

            ))
    )
  ))