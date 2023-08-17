#lang typed/racket

(require typed-racket-datatype)
(require "name-gen.rkt")
(require/typed racket/hash
               [hash-union (All (a b) (-> (Immutable-HashTable a b) * (Immutable-HashTable a b)))])

(provide
         Hash
         mk-vars
         nonTerminal?
         nonTerminal
         PESyn
         SrcLoc
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
         pprint-peg
         )


(define-type (Hash A B)  (Immutable-HashTable A B))

(define (mk-vars  [l : (Listof (Pair String PE))]) : (Hash String PE)
     (make-immutable-hash l)
  )

; Parsing Expressions Derivates Definitions
; A mirror defininitio of PE, except for the adition for
; tracking pending derivate and delta computations.


#;(define-datatype PE (Eps)
                     (Any)
                     (Sym [c : Char])
                     (Var [s : String])
                     (Cat [l : PE] [r : PE])
                     (Alt [l : PE] [r : PE])
                     (Not [p : PE])
                     (Rep [p : PE])      
  )

(struct SrcLoc ([l : Natural ] [c : Natural ])  #:prefab)
(struct PESyn ([src : SrcLoc])  #:prefab)

(struct PEG ( [vars : (Hash String PE) ]  [start : PE] ) #:prefab)

;(struct rhs ( [e : PE] [sem : (-> Any* Any)] ) #:prefab)

(define-type PE (Union Eps Any Sym Var Cat Alt Not Rep))


;(struct Eps  ()                  #:prefab)
;(struct Any  ()                  #:prefab)
;(struct Sym  ([c : Char])        #:prefab)
;(struct Var  ([s : String])      #:prefab)
;(struct Cat  ([l : PE] [r : PE]) #:prefab)
;(struct Alt  ([l : PE] [r : PE]) #:prefab)
;(struct Not  ([p : PE])          #:prefab)
;(struct Rep  ([p : PE])          #:prefab)


(struct Eps PESyn ()                  #:prefab)
(struct Any PESyn ()                  #:prefab)
(struct Sym PESyn ([c : Char])        #:prefab)
(struct Var PESyn ([s : String])      #:prefab)
(struct Cat PESyn ([l : PE] [r : PE]) #:prefab)
(struct Alt PESyn ([l : PE] [r : PE]) #:prefab)
(struct Not PESyn ([p : PE])          #:prefab)
(struct Rep PESyn ([p : PE])          #:prefab)


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
     [(cons (Eps k)     (Eps k))        #t]
     [(cons (Any k)     (Any k))        #t]
     [(cons (Sym k c)   (Sym k1 c1))    (char=? c c1)]
     [(cons (Var k s1)  (Var k s2))     (string=? s1 s2)]
     [(cons (Cat k l r) (Cat k l2 r2))  (and (pe=? l l2) (pe=? r r2))]
     [(cons (Alt k l r) (Alt k l2 r2))  (and (pe=? l l2) (pe=? r r2))]
     [(cons (Not k l)   (Not k l2))     (pe=? l l2)]
     [(cons (Rep k l)   (Rep k l2))     (pe=? l l2)]
     [(cons _ _ )                       #f]
     )
  )



(define (pe-null?  [v : (Hash String PE) ] [e : PE]) : Boolean
       (match e
        [(Any k)            #f]
        [(Eps k)            #t]
        [(Sym k c)          #f]
        [(Var k s)        (pe-null? v (hash-ref v s))]
        [(Cat k p1 p2)    (let ([r : Boolean (pe-null? v p1)] )
                              (cond [r (pe-null? v p2)] [else r]))]
        [(Alt k p1 p2)     (let ([r : Boolean (pe-null? v p1)] )
                              (cond [(not r) (pe-null? v p2)] [else r]))] ;(tor (dpe-null? v p1) (dpe-null? v p2))]
        [(Rep k p)        #t]
        [(Not k p)        (not (pe-null? v p))] ; This needs not to be lazy here !
     )
  )


(define (ep-alphabet [e : PE] ) :  (Listof Char)
     (match e
         [(Any k)          null]
         [(Eps k)          null]
         [(Sym k c)        (list c)]
         [(Var k s)        null]
         [(Cat k p1 p2)    (set-union (ep-alphabet p1) (ep-alphabet p2))]
         [(Alt k p1 p2)    (set-union (ep-alphabet p1) (ep-alphabet p2))]
         [(Rep k p)        (ep-alphabet p)]
         [(Not k p)        (ep-alphabet p)]
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
        [(cons (Eps k) d)   d]
        [(cons e (Eps k))   e]
        [(cons (Rep k (Any k1)) (Rep k2 (Any k3)))   (Rep k (Any k1))]
        [(cons (Not k e) (Cat k1 (Not k2 e) d))      (Cat k (Not k1 e) d)]
        [(cons e d)                                  (Cat (PESyn-src e) e d)]
       )
  )

(define (dalt [l : PE] [r : PE]  ) : PE
     (match (cons l r)
        [(cons e e)      e]
        [(cons (Eps k)     d)                    (Eps k) ]
        [(cons e         (Cat k (Not k1 e) e1))  (Alt k e e1)]
        [(cons e         (Not k e))              (Alt (PESyn-src e) e (Any k))] 
        [(cons (Cat k e d) (Cat k1 e d1))        (Cat k e (Alt k1 d d1)) ]
        [(cons e d)                              (Alt (PESyn-src e) e d) ]
       )
  )

(define (dnot [l : PE]) : PE
     (match l
        [(Cat k e (Rep k1 (Any k2))) (Not k e)]
        [(Not k (Not k1 (Not k2 e))) (Not k e)]
        [e                   (Not (PESyn-src e) e)]
       )
  )

(define (drep [l : PE]) : PE
     (match l
        [(Any k)              (Rep k (Any k))]
        [(Rep k1 (Rep k2 e))  (Rep k1 e)]
        [e                    (Rep (PESyn-src e) e)]
       )
  )


(define (pe-simplify [dpe : PE]) : PE
      (match dpe
         [(Cat k p1 p2)    (dcat (pe-simplify p1) (pe-simplify p2))]
         [(Alt k p1 p2)    (dalt (pe-simplify p1) (pe-simplify p2))]
         [(Rep k p)        (drep (pe-simplify p))]
         [(Not k p)        (dnot (pe-simplify p))]
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
        [(Any _)            "."]
        [(Eps _)            "ϵ"]
        [(Sym _ c)        (string-append "'" (string c) "'")]
        [(Var _ s)        (string-append "\"" s "\"")]
        [(Cat _ p1 p2)    (parens (> n 2) (string-append (pe-prec->string 2 p1) (pe-prec->string 2 p2)))]
        [(Alt _ p1 p2)    (parens (> n 1) (string-append (pe-prec->string 1 p1) "/" (pe-prec->string 1 p2)))]
        [(Rep _ p)        (parens (> n 4) (string-append (pe-prec->string 4 p) "*"))  ]
        [(Not _ p)        (parens (> n 3) (string-append "!" (pe-prec->string 3 p) )) ]
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
    (append (hash-map (PEG-vars e)
                      (lambda ([s : String] [exp : PE]) (string-append s "<-" (pe-prec->string 0 exp) "\n")) )
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
     [(Cat k p1 p2) (let* ([ p11 : (Pair PE (Hash PE String)) (rep-rem t p1)]
                           [ p22 : (Pair PE (Hash PE String)) (rep-rem (cdr p11) p2)])
                            (cons (Cat k (car p11) (car p22)) (cdr p22) ))]
     [(Alt k p1 p2)  (let* ([ p11 : (Pair PE (Hash PE String)) (rep-rem t p1)]
                          [ p22 : (Pair PE (Hash PE String)) (rep-rem (cdr p11) p2)])
                          (cons (Alt k (car p11) (car p22)) (cdr p22) ))]
     [(Not k e) (let* ([ e1 : (Pair PE (Hash PE String)) (rep-rem t e)])
                    (cons (Not k (car e1)) (cdr e1) ))]
    
     [(Rep k e) (let* ([ er1 : (Pair PE (Hash PE String)) (rep-rem t e)]
                     [ ee1 : PE (car er1) ]
                     [ t1  : (Hash PE String) (cdr er1) ]
                     )
                    (cond
                      [(hash-has-key? t1 ee1) (cons (Var k (hash-ref t1 ee1)) t1)]
                      [else (let ([s : String (new-name)])
                                  (cons (Var k s) (hash-set t1 ee1 s) ))]))]

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
          (Alt (SrcLoc 0 0) (Cat (SrcLoc 0 0) (car x) (Var (SrcLoc 0 0) (cdr x))) (Eps (SrcLoc 0 0))))
  )

(define (kle-remove [g : PEG] ) : PEG
        (start-gen (hash-keys (PEG-vars g)) "K_" 0)
        (let* ([vt : (Pairof (Hash String PE) (Hash PE String)) (kle-rem-rules (PEG-vars g)) ]
               [et :  (Pair PE (Hash PE String)) (rep-rem (cdr vt) (PEG-start g)) ]
               [nw-nt : (Hash String PE) (make-immutable-hash (map un-rep (hash->list (cdr et)))) ])
              (PEG (hash-union (car vt) nw-nt) (car et))
          )
  )
