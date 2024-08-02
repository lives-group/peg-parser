#lang racket

(require "grammar.rkt"
         "peg-ast.rkt"
         "peg-wf.rkt"
         racket/path
         racket/syntax)



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
  (string-append* "The following expressions do not type:\n "
                   (map (lambda (s) (string-append "   " (pe->err-string s) "\n")) l)))

(define (error-msgs pl)
  (match pl
    [(TyErr '() ty) (error  (msg-ty-erros ty))]
    [(TyErr lp '()) (error  (msg-loops lp))]
    [(TyErr lp ty) (error  (string-append (msg-loops lp) "\n" (msg-ty-erros ty)))])
  
  )

(define parse-base-name "parse")


(define (prefix-from-file pth)
  (string->symbol (string-append (path->string (file-name-from-path (path-replace-extension pth #""))) ":") ))

(define (peg-read-syntax path port)
  (define grammar (parse port))
  (let ([types (infer-types grammar)]
        [fname (prefix-from-file path)] )
    (if (not (satisfied? types))
        (error-msgs  (get-errors types) )
        (datum->syntax
                      #f
                    `(module peg-parser racket
                      (provide (all-from-out peg-parser/peg-simple-recognizer)
                        list-grammar
                       (all-from-out peg-parser/peg-ast)
                       (prefix-out ,fname parse)
                       (prefix-out ,fname parse-from-nt)
                       (prefix-out ,fname parse-file)
                       (prefix-out ,fname parse-file-from-nt)
                       )           
                       (require peg-parser/peg-simple-recognizer
                                peg-parser/peg-ast
                       )
                       
                       (define grm ,grammar)

                       (define (parse s)
                           (peg-parse grm (open-input-string s)))
                       
                       (define (parse-from-nt i s)
                           (peg-parse-from grm i (open-input-string s)))
            
                       (define (parse-file fname)
                          (let* ([s (open-input-file fname #:mode 'text)]
                                 [ast (peg-parse grm s)])
                                 ( close-input-port  fname)
                               )
                                 
                       )
            
                       (define (parse-file-from-nt ntname fname)
                               (let* ([s (open-input-file fname #:mode 'text)]
                                     [ast (parse-from-nt grm ntname s)])
                                    ( close-input-port  fname))
                       )
                       
                       (define (list-grammar)
                              (foldr string-append "" (peg->string grm)))

                 )
    ))
  ))