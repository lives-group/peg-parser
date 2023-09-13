;#lang typed/racket
#lang typed/racket/no-check

(require "peg-ast.rkt")


(provide
        PegTree
        (struct-out PTSym)
        (struct-out PTStr)
        (struct-out PTVar)
        (struct-out PTList)
        peg-parse
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

(define-type PegTree (Union Eps Any Sym Rng Annot Bind Var Cat Alt Not Rep))

(struct  PTFail () #:transparent)
(struct  PTSym ([c : Char]) #:transparent)
(struct  PTStr ([s : (Listof Char)]) #:transparent)
(struct  PTVar ([var : String] [t : PegTree]) #:transparent)
(struct  PTList ([xs : (Listof PegTree)]) #:transparent)
                           


(define (flatten [x : PegTree] ) : String
      (match x
        [(PTSym  c) (string c)]
        [(PTStr s) s]
        [(PTVar _ t) (flatten t)]
        [(PTList xs) (foldr string-append "" (map flatten xs))]
        )
)

(define (mk-pcat [l :  PegTree] [r : PegTree] ) : PegTree
     (match (cons l r)
       [(cons (PTFail) _) (PTFail)]
       [(cons _ (PTFail)) (PTFail)]
       [(cons (PTList '()) y) (PTList (list y))]
       [(cons y (PTList '())) (PTList (list y))]
       [(cons x (PTList xs)) (PTList (cons x xs))]
       [(cons (PTList xs) x) (PTList (append xs (list x)))]
       [(cons x y) (PTList (list x y))]
  )
)

(define (spe-parse [g : PEG] [pe : PE] [f : Input-Port ] ) : PegTree  
        (match pe
             [(Eps _)     (PTList (list))]
             [(Any _)     (let [(ch1 : (Union Char EOF) (read-char f))]
                               (cond [(eof-object? ch1) (PTFail)]
                                     [else (PTSym ch1) ]))]
             [(Sym _ ch) (let [(ch1 : (Union Char EOF) (read-char f))]
                               (cond [(eof-object? ch1) (PTFail)]
                                   [(char=? ch ch1)  (PTSym ch1)]
                                   [else (PTFail)]))]
             [(Rng _ s e) (let [(ch1 : (Union Char EOF) (read-char f))]
                               (cond [(eof-object? ch1) (PTFail)]
                                     [(and (char>? ch1 s) (char<? ch1 e))  (PTSym ch1)]
                                     [else (PTFail)]))]
             [(Var _ ig s) (let ([r : PegTree (spe-parse g (nonTerminal g s) f)])
                               (match r
                                  [(PTFail) (PTFail)]
                                  [x       (if ig x (PTVar s x))])
                               )]
             [(Annot _ 'Silent e) (match (spe-parse g e f)
                                    [(PTFail) (PTFail)]
                                    [_ (PTList '())] )]
             [(Annot _ 'Flat e) (match (spe-parse g e f)
                                    [(PTFail) (PTFail)]
                                    [x (flatten x)] )]
             [(Cat _ e d) (let ([te (spe-parse g e f)])
                               (cond [(not (PTFail? te)) (mk-pcat te (spe-parse g d f))]
                                     [else (PTFail)]))]
                          
             [(Alt _ e d)  (begin (mrk f)
                                (match (spe-parse g e f)
                                       [(PTFail) (begin (rstr f)
                                                        (spe-parse g d f))]
                                       [x (begin (mrk-pop)
                                                  x) ]))]
             [(Rep _ e)   (begin (mrk f)
                                 (match (spe-parse g e f)
                                      [(PTFail) (begin (rstr f)  (PTList (list)) )]
                                      [x        (begin (mrk-pop)
                                                       (mk-pcat x (spe-parse g pe f)))]))]
             [(Not _ e)   (begin (mrk f)
                                 (match (spe-parse g e f)
                                      [(PTFail) (begin (rstr f)  (PTList '()) )]
                                      [x        (begin (rstr f)
                                                       (PTFail))]))]
         )
      )

(define (peg-parse [g : PEG] [f : Input-Port ] ) : PegTree  
        (spe-parse g (PEG-start g) f)
 )

