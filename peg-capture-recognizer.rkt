;#lang typed/racket
#lang typed/racket/no-check

(require "peg-ast.rkt")


(provide
        PegTree
        (struct-out PTFail)
        ;(struct-out PTSym)
        ;(struct-out PTStr)
        (struct-out PTVar)
        ;(struct-out PTList)
        peg-parse
        peg-parse-from
        peg-parse-file
        peg-parse-file-from
        
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

(define-type PegTree (Union PTFail PTVar (Listof Any)))

(struct  PTFail () #:transparent)
(struct  PTVar ([var : String] [t : PegTree]) #:transparent)

                           


(define (flatten [x : PegTree] ) : String
      (match x
        [(? char? c) (string c)]
        [(? string? s) s]
        [(? symbol? _) (~a x)]
        [(PTVar _ t) (flatten t)]
        [(cons _ _) (foldr string-append "" (map flatten x))]
        )
)

(define (mk-pcat [l :  PegTree] [r : PegTree] ) : PegTree
     (match (cons l r)
       [(cons (PTFail) _) (PTFail)]
       [(cons _ (PTFail)) (PTFail)]
       [(cons (PTVar s t) (cons _ _))  (cons l r)]
       [(cons (PTVar s t) (list))  (list l)]
       [(cons (list) (PTVar s t) )  (list r)]
       [(cons (cons _ _) (PTVar s t) ) (append l (list r))]
       [(cons (cons _ _) (list) )  l]
       [(cons (list) (cons _ _) )  r]
       [(cons (cons _ _) (cons _ _) )  (append l r)]
       [(cons x y)       (list x y)]
  )
)

(define (spe-parse [g : PEG] [pe : PE] [f : Input-Port ] ) : PegTree  
        (match pe
             [(Eps _)     (list)]
             [(Any _)     (let [(ch1 : (Union Char EOF) (read-char f))]
                               (cond [(eof-object? ch1) (PTFail)]
                                     [else (list ch1) ]))]
             [(Sym _ ch) (let [(ch1 : (Union Char EOF) (read-char f))]
                               (cond [(eof-object? ch1) (PTFail)]
                                   [(char=? ch ch1)  (list ch1)]
                                   [else (PTFail)]))]
             [(Rng _ s e) (let [(ch1 : (Union Char EOF) (read-char f))]
                               (cond [(eof-object? ch1) (PTFail)]
                                     [(and (char>? ch1 s) (char<? ch1 e))  (list ch1)]
                                     [else (PTFail)]))]
             [(Var _ ig s) (let ([r : PegTree (spe-parse g (nonTerminal g s) f)])
                               (match r
                                  [(PTFail) (PTFail)]
                                  [x       (if ig x (PTVar s x))])
                               )]
             [(Annot _ 'Silent e) (match (spe-parse g e f)
                                    [(PTFail) (PTFail)]
                                    [_ (list)] )]
             [(Annot _ 'Flat e) (match (spe-parse g e f)
                                    [(PTFail) (PTFail)]
                                    [x  (list (flatten x))] )]
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
                                      [(PTFail) (begin (rstr f)  (list) )]
                                      [x        (begin (mrk-pop)
                                                       (mk-pcat x (spe-parse g pe f)))]))]
             [(Not _ e)   (begin (mrk f)
                                 (match (spe-parse g e f)
                                      [(PTFail) (begin (rstr f)  (list) )]
                                      [x        (begin (rstr f)
                                                       (PTFail))]))]
         )
      )

(define (peg-parse [g : PEG] [f : Input-Port ] ) : PegTree  
        (spe-parse g (PEG-start g) f)
 )

(define (peg-parse-from [g : PEG] [start : String] [f : Input-Port ] ) : PegTree  
        (spe-parse g (nonTerminal g start) f)
 )

(define (peg-parse-file [g : PEG] [filename : String] ) : PegTree
        (let ([h : Input-Port (open-input-file filename #:mode 'text)])
             (begin
                  (spe-parse g (PEG-start g) h)
                  (close-input-port h)
             )
         )
  )

(define (peg-parse-file-from [g : PEG] [start : String] [filename : String] ) : PegTree
        (let ([h : Input-Port (open-input-file filename #:mode 'text)])
             (begin
                  (spe-parse g (nonTerminal g start) h)
                  (close-input-port h)
             )
         )
  )

