#lang typed/racket
(require typed-racket-datatype)
(require peg-parser/name-gen)
(require racket/hash)

(provide
         Hash
         mk-vars
         nonTerminal?
         nonTerminal
         PE
         (struct-out Sym)
         (struct-out Eps)
         (struct-out Any)
         (struct-out Var)
         (struct-out Cat)
         (struct-out Alt)
         (struct-out Not)
         (struct-out Rep)
         (struct-out PEG)

         pe=?
         pe-null?
         kle-remove

         ; Smart constructors
         dcat
         dalt
         dnot
         drep
         peg-simplify
         
         ep-alphabet
         alphabet

         ; Pretty print 
         pe->string
         peg->string
         pprint-pe
         pprint-peg)


(define-type (Hash A B)  (Immutable-HashTable A B))

(define (mk-vars  [l : (Listof (Pair String PE))]) : (Hash String PE)
     (make-immutable-hash l)
  )

; Parsing Expressions Derivates Definitions
; A mirror defininitio of PE, except for the adition for
; tracking pending derivate and delta computations.
(define-datatype PE  (Eps)
                     (Any)
                     (Sym [c : Char])
                     (Var [s : String])
                     (Cat [l : PE] [r : PE])
                     (Alt [l : PE] [r : PE])
                     (Not [p : PE])
                     (Rep [p : PE])      

  )

(define-datatype PEG (PEG [vars : (Hash String PE)] [start : PE] ))

(define (nonTerminal? [p : PEG ] [s : String ] ) : Boolean
      (hash-has-key? (PEG-vars p) s)
  )

(define (nonTerminal [p : PEG ] [s : String ] ) : PE
      (hash-ref (PEG-vars p) s)
  )

; Syntactical equality for parsing expression.
;
(define (pe=? [e : PE] [d : PE] ) : Boolean
   (match (cons e d)
     [(cons (Eps)  (Eps))          #t]
     [(cons (Any)  (Any))          #t]
     [(cons (Sym c)  (Sym c1))     (char=? c c1)]
     [(cons (Var s1) (Var s2))     (string=? s1 s2)]
     [(cons (Cat l r) (Cat l2 r2)) (and (pe=? l l2) (pe=? r r2))]
     [(cons (Alt l r) (Alt l2 r2)) (and (pe=? l l2) (pe=? r r2))]
     [(cons (Not l)   (Not l2))    (pe=? l l2)]
     [(cons (Rep l)   (Rep l2))    (pe=? l l2)]
     [(cons _ _ )                  #f]
     )
  )


(define (pe-null?  [v : (Hash String PE) ] [e : PE]) : Boolean
       (match e
        [(Any)            #f]
        [(Eps)            #t]
        [(Sym c)          #f]
        [(Var s)        (pe-null? v (hash-ref v s))]
        [(Cat p1 p2)    (let ([r : Boolean (pe-null? v p1)] )
                              (cond [r (pe-null? v p2)] [else r]))]
        [(Alt p1 p2)     (let ([r : Boolean (pe-null? v p1)] )
                              (cond [(not r) (pe-null? v p2)] [else r]))] ;(tor (dpe-null? v p1) (dpe-null? v p2))]
        [(Rep p)        #t]
        [(Not p)        (not (pe-null? v p))] ; This needs not to be lazy here !
     )
  )


(define (ep-alphabet [e : PE] ) :  (Listof Char)
     (match e
         [(Any)          null]
         [(Eps)          null]
         [(Sym c)        (list c)]
         [(Var s)        null]
         [(Cat p1 p2)    (set-union (ep-alphabet p1) (ep-alphabet p2))]
         [(Alt p1 p2)    (set-union (ep-alphabet p1) (ep-alphabet p2))]
         [(Rep p)        (ep-alphabet p)]
         [(Not p)        (ep-alphabet p)]
     )
)

(define (union-list [l : (Listof (Listof Char))] ) : (Listof Char)
     (foldr (lambda ([a :(Listof Char)] [b :(Listof Char)]) (set-union a b)) null l)
  )


(define (alphabet [d : PEG ] ) :  (Listof Char)
           (union-list (cons (ep-alphabet (PEG-start d))
                             (map ep-alphabet (hash-values ( PEG-vars d)))
                       )) 
)


;
; SMART Constructors
;
  
(define (dcat [l : PE] [r : PE])
     (match (cons l r)
        [(cons (Eps) d)   d]
        [(cons e (Eps))   e]
        [(cons (Rep (Any)) (Rep (Any)))   (Rep (Any))]
        [(cons (Not e) (Cat (Not e) d))   (Cat (Not e) d)]
        [(cons e d)        (Cat e d)]
       )
  )

(define (dalt [l : PE] [r : PE]  ) : PE
     (match (cons l r)
        [(cons e e)      e]
        [(cons (Eps)     d)                (Eps) ]
        [(cons e         (Cat (Not e) e1)) (Alt e e1)]
        [(cons e         (Not e))          (Alt e (Any))] 
        [(cons (Cat e d) (Cat e d1))       (Cat e (Alt d d1)) ]
        [(cons e d)                        (Alt e d) ]
       )
  )

(define (dnot [l : PE]) : PE
     (match l
        [(Cat e (Rep (Any))) (Not e)]
        [(Not (Not (Not e))) (Not e)]
        [e                   (Not e)]
       )
  )

(define (drep [l : PE]) : PE
     (match l
        [(Any)         (Rep (Any))]
        [(Rep (Rep e)) (Rep e)]
        [e             (Rep e)]
       )
  )


(define (pe-simplify [dpe : PE]) : PE
      (match dpe
         [(Cat p1 p2)    (dcat (pe-simplify p1) (pe-simplify p2))]
         [(Alt p1 p2)    (dalt (pe-simplify p1) (pe-simplify p2))]
         [(Rep p)        (drep (pe-simplify p))]
         [(Not p)        (dnot (pe-simplify p))]
         [p p]
       )
  )


(define (env-simplify [h : (Hash String PE) ]) : (Hash String PE)
     (make-immutable-hash
      (hash-map h (lambda ([s : String] [p : PE]) (cons s (pe-simplify p))) ))
)

(define (peg-simplify [peg : PEG]) : PEG
      (PEG
        (env-simplify  (PEG-vars peg))
        (pe-simplify (PEG-start peg))
      )
 )


;Primary     5
;Kle         4
;Not         3
;Sequence    2 Left
;alternative 1 Left
(define (pe-prec->string [n : Natural ] [e : PE]) : String
    (match e
        [(Any)            "."]
        [(Eps)            "ϵ"]
        [(Sym c)        (string c)]
        [(Var s)        s]
        [(Cat p1 p2)    (parens (> n 2) (string-append (pe-prec->string 2 p1) (pe-prec->string 2 p2)))]
        [(Alt p1 p2)    (parens (> n 1) (string-append (pe-prec->string 1 p1) "/" (pe-prec->string 1 p2)))]
        [(Rep p)        (parens (> n 4) (string-append (pe-prec->string 4 p) "*"))  ]
        [(Not p)        (parens (> n 3) (string-append "!" (pe-prec->string 3 p) )) ]
     )
  )

(define (parens [b : Boolean] [s : String]) : String
     (match b
           [#f   s]
           [else  (string-append "(" s ")")]
  )
)

(define (pe->string [e : PE]) : String
    (pe-prec->string 0 e))

(define (peg->string [e : PEG]) : (Listof String)
    (append (hash-map (PEG-vars e) (lambda ([s : String] [exp : PE]) (string-append s "<-" (pe-prec->string 0 exp))) )
            (list (pe-prec->string 0 (PEG-start e)))
    )
)

(define (pprint-pe [e : PE])
   (display (pe-prec->string 0 e))
 )

(define (pprint-peg [e : PEG])
    (for ([s  (peg->string e)])
         (displayln s)
    )
)


;
; Klenne removal utility 
;

(define (rep-rem [t : (Hash PE String)] [p : PE] ) : (Pair PE (Hash PE String))
  (match p
     [(Cat p1 p2) (let* ([ p11 : (Pair PE (Hash PE String)) (rep-rem t p1)]
                         [ p22 : (Pair PE (Hash PE String)) (rep-rem (cdr p11) p2)])
                         (cons (Cat (car p11) (car p22)) (cdr p22) ))]
     [(Alt p1 p2)  (let* ([ p11 : (Pair PE (Hash PE String)) (rep-rem t p1)]
                          [ p22 : (Pair PE (Hash PE String)) (rep-rem (cdr p11) p2)])
                          (cons (Alt (car p11) (car p22)) (cdr p22) ))]
     [(Not e) (let* ([ e1 : (Pair PE (Hash PE String)) (rep-rem t e)])
                    (cons (Not (car e1)) (cdr e1) ))]
    
     [(Rep e) (let* ([ er1 : (Pair PE (Hash PE String)) (rep-rem t e)]
                     [ ee1 : PE (car er1) ]
                     [ t1  : (Hash PE String) (cdr er1) ]
                     )
                    (cond
                      [(hash-has-key? t1 ee1) (cons (Var (hash-ref t1 ee1)) t1)]
                      [else (let ([s : String (new-name)])
                                  (cons (Var s) (hash-set t1 ee1 s) ))]))]

     [_ (cons p t)]
  )
)


(define (kle-rem-rules [vars : (Hash String PE)]) : (Pairof (Hash String PE) (Hash PE String))
  (let ([t1 : (Hash String PE)  (make-immutable-hash)]
        [t2 : (Hash PE String)   (make-immutable-hash)] )
      (foldr (lambda ([rname : String] [nset :  (Pairof (Hash String PE) (Hash PE String)) ])
                     (let ([k : (Pair PE (Hash PE String)) (rep-rem (cdr nset) (hash-ref vars rname)) ])
                          (cons (hash-set (car nset) rname (car k))
                                (cdr k)))      
              )
              (cons t1 t2)
              (hash-keys vars))
  )
)

(define (un-rep [x : (Pairof PE String)] ) : (Pairof String PE)
    (cons (cdr x)
          (Alt (Cat (car x) (Var (cdr x))) (Eps)))
  )

(define (kle-remove [g : PEG] ) : PEG
        (start-gen (hash-keys (PEG-vars g)) "K_" 0)
        (let* ([vt : (Pairof (Hash String PE) (Hash PE String)) (kle-rem-rules (PEG-vars g)) ]
               [et :  (Pair PE (Hash PE String)) (rep-rem (cdr vt) (PEG-start g)) ]
               [nw-nt : (Hash String PE) (make-immutable-hash (map un-rep (hash->list (cdr et)))) ])
              (PEG (hash-union (car vt) nw-nt) (car et))
          )
  )

(define pegtest1 : PEG
  (PEG  (make-immutable-hash
         (list (cons "A" (Rep (Sym #\a) ) )
               (cons "B" (Cat (Rep (Sym #\a) ) (Rep (Sym #\b) ) ))
               )
         )
         (Cat (Var "A") (Var "B")) ))