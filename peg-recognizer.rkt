#lang typed/racket
(require typed-racket-datatype)
(require "peg-ast.rkt")


(provide
        ParseTree
        (struct-out TSym)
        (struct-out TEps)
        (struct-out TVar)
        (struct-out TCat)
        (struct-out LChoice)
        (struct-out RChoice)
        (struct-out TRep)
        peg-parse
        simplified-peg-parse
       )


(define stk : (Listof Natural) (list) )
(define (mrk [f : Input-Port]) (set! stk (cons (file-position f) stk)))
(define (rstr [f : Input-Port])
   (cond
       [(null? stk) (error "nothing to restore")]
       [else (begin
               (file-position f (car stk))  
               (set! stk (cdr  stk)))]
     )
  )

(define (mrk-pop)
     (cond
       [(null? stk) stk]
       [else (set! stk (cdr  stk))]
     )
  )

(define-datatype ParseTree  (TFail)
                            (TEps)
                            (TSym [c : Char])
                            (TStr [s : (Listof Char)])
                            (TVar [var : String] [t : ParseTree])
                            (TCat [tl : ParseTree] [tr : ParseTree])
                            (LChoice [tl : ParseTree])
                            (RChoice [tr : ParseTree])
                            (TRep [xs : (Listof ParseTree)])      

  )

(define (mkRep [l :  ParseTree] [t : ParseTree]  )
     (match l
       [(TRep xs) (TRep (cons t xs) )]
       [_ (error (string-append "Expecting a repetition tree given : " (~v l)) )]
  )
)


(define (mkCat [l :  ParseTree] [r : ParseTree]  )  : ParseTree
     (match (cons l r)
       [(cons (TFail) _) (TFail)]
       [(cons _ (TFail)) (TFail)]
       [(cons x (TEps)) x]
       [(cons (TEps) y) y]
       [(cons (TSym c)  (TSym c1)) (TStr (list c c1))]
       [(cons (TStr xs) (TSym c1)) (TStr (append xs (list c1)))]
       [(cons (TSym c)  (TStr xs)) (TStr (cons c xs) )]
       [(cons (TEps) y) y]
       [(cons x y) (TCat x y)]
  )
)

(define (mkRChoice [l :  ParseTree] )  : ParseTree
     (match l
       [(TFail)  (TFail)]
       [x (RChoice x)]
  )
)

(define (pe-parse [verb : Boolean] [g : PEG] [pe : PE] [f : Input-Port ] ) : ParseTree  
        (match pe
             [(Eps _)     (TEps)]
             [(Any _)     (let [(ch1 : (Union Char EOF) (read-char f))]
                             (cond [(eof-object? ch1) (TFail)]
                                   [else (TSym ch1) ]))]
             [(Sym _ ch) (let [(ch1 : (Union Char EOF) (read-char f))]
                               (cond [(eof-object? ch1) (TFail)]
                                   [(char=? ch ch1)  (TSym ch1)]
                                   [else (TFail)]))]
             [(Var _ s)  (let ([r : ParseTree (pe-parse verb g (nonTerminal g s) f)])
                               (match r
                                  [(TFail) (TFail)]
                                  [x       (TVar s x)])
                               )]
             [(Cat _ e d) (let [(x : ParseTree (pe-parse verb g e f))]
                            (match x
                            [(TFail) (TFail)]
                            [x  (mkCat x (pe-parse verb g d f))]
                          ))]
             [(Alt _ e d)  (begin (mrk f)
                                (match (pe-parse verb g e f)
                                       [(TFail) (begin (rstr f)
                                                       (if verb
                                                           (mkRChoice (pe-parse verb g d f))
                                                           (pe-parse verb g d f)))]
                                       [x (begin (mrk-pop)
                                                 (if verb
                                                     (LChoice x)
                                                     x)) ]))]
             [(Rep _ e)   (begin (mrk f)
                                 (match (pe-parse verb g e f)
                                      [(TFail) (begin (rstr f)  (TRep (list)) )]
                                      [x       (begin  (mrk-pop)
                                                       (mkRep (pe-parse verb g pe f) x))]))]
             [(Not _ e)   (begin (mrk f)
                                 (match (pe-parse verb g e f)
                                      [(TFail) (begin (rstr f)  (TEps) )]
                                      [x       (begin  (rstr f)
                                                       (TFail))]))]
         )
      )

(define (peg-parse [g : PEG] [f : Input-Port ] ) : ParseTree  
        (pe-parse true g (PEG-start g) f)
 )

(define (simplified-peg-parse [g : PEG] [f : Input-Port ] ) : ParseTree  
        (pe-parse false g (PEG-start g) f)
 )