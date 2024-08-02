;#lang typed/racket
#lang typed/racket/no-check

(require "peg-ast.rkt")

(provide  start-exp-var
          (struct-out Ctx)
          (struct-out Ty)
          (struct-out TyErr)
          TyEx
          (struct-out ExHs)
          (struct-out Exb)
          
          build-initial-ctx
          ctx-getNt
          
          nwExVar
          nwTyVar
          rstvar
          nwTyNulVar
          mk-cat-constraint
          mk-alt-constraint
          mk-null-same-headset-constraint
          mk-const-constraint
          mk-var-constraint
          
          dbg-cat-constraint
          dbg-alt-constraint
          dbg-null-same-headset-constraint
          dbg-const-constraint
          dbg-var-constraint          
          ctx-pushr
          ctx-pushr-with-debug

          ctx-tyEnv-toString
          ctx-constr-toString
          ctx-subst-toString
          ty->string
          
          solve-ctx

          get-start-exp-ty
          nt-occurs
          satisfied?
          unsolved-nts
          unsolved-pe
          get-errors)
          

(define (start-exp-var)
  (Ty (ExVar 0) (ExVar 1))
  )

(struct TyErr ([loops : (Listof String)]
              [unsolved : (Listof PE)])
       #:transparent)

; Parsing Expressions Derivates Definitions
; A mirror defininitio of PE, except for the adition for
; tracking pending derivate and delta computations.

(struct Ctx (;[env : (Hash String Ty)]   ; ψ
             [env : (Hash Natural Ty)]
             [nt-names : (Hash String Natural) ]
             [subs : (Hash Natural TyEx)] ; ϕ
             [clist : (Listof Constr) ] ; UUID term ≡ term
             [vcount : Natural] ;  Variable counter
             [ccount : Natural] ; Constraint counter
             [vsrc : (Hash Natural PE)]
            )
       #:transparent)




;(struct VDep   ([cnst : Natrual] [src : Natural] [targs : (Listof Natural)] ) #:transparent)
;(define-type Deps (Hash Natural (Pairof Natural (Listof Natural)) ))

;(define-type Term (Union Ty ))

(struct Ty   ([nul? : TyEx] [headset : TyEx] ) #:transparent)

(struct Pair   ([fst : Any] [snd : Any] ) #:transparent)

(define-type TyEx (Union ExHs Exb ExVar ExU ExNot ExOr ExAnd ExImp))

(struct ExHs   ([headset : (Listof Natural)] ) #:transparent)
(struct Exb    ([nul : Boolean] ) #:transparent)
(struct ExVar  ([name : Natural])#:transparent)
(struct ExU    ([left : TyEx] [right : TyEx]) #:transparent)
(struct ExAnd  ([left : TyEx] [right : TyEx])#:transparent)
(struct ExOr   ([left : TyEx] [right : TyEx]) #:transparent)
(struct ExNot  ([not : TyEx] ) #:transparent)
(struct ExImp  ([cd : TyEx] [hs1 : TyEx] [hs2 : TyEx])#:transparent)

(struct Constr ([uid : Natural] [lft : TyEx] [rght : TyEx]) #:transparent)

(define (rename-TyEx [t : (Hash String Natural)] [e : TyEx] )
       (match e
        [(ExHs hs) (ExHs (map (lambda (x) (hash-ref t x)) hs))]
        [(ExU  l r)  (ExU  (rename-TyEx t l) (rename-TyEx t r))]
        [(ExImp c h1 h2) (ExImp c (rename-TyEx t h1) (rename-TyEx t h2))]
        [_ e]
     )
   )

(define (reverse-names  [c : Ctx] ) : [t : (Hash Natural String)]
       (make-immutable-hash (map (lambda (p) (cons (cdr p) (car p)) ) (hash->list (Ctx-nt-names c))))
   )

(define (reverse-name-TyEx [t : (Hash Natural String)] [e : TyEx] )
       (match e
        [(ExHs hs)       (ExHs (map (lambda (x) (hash-ref t x)) hs))]
        [(ExU  l r)      (ExU  (reverse-name-TyEx t l) (reverse-name-TyEx t r))]
        [(ExImp c h1 h2) (ExImp c (reverse-name-TyEx t h1) (reverse-name-TyEx t h2))]
        [_ e]
     )
   )

(define (mkTyList [xs : (Listof Natural)]
                  [start : Natural] ) : (Pairof  (Listof (Pairof Natural Ty)) Natural)
   (foldl (lambda (x p)
                   (cons (cons (cons x (Ty (ExVar (cdr p)) (ExVar (+ (cdr p) 1))) ) (car p))
                         (+ (cdr p) 2)))
          (cons '() start)
          xs)
  )

(define (build-initial-ctx [xs : (Listof String)]) : Ctx
    (let* ([names : (Listof (Pairof String Natural))
                    (map cons xs (build-list (length xs) values))]
           [l : (Pairof (Listof (Pairof Natural Ty)) Natural)
                (mkTyList (map cdr names) 2)]
           [m : (Hash Natural Ty) (make-immutable-hash (car l))]) 
           (Ctx m (make-immutable-hash names) (make-immutable-hash) (list) (cdr l) 0 (make-immutable-hash))
       )
  )

(define (mkPair [n : Natural] [xs : (Listof Natural)]) : (Pairof Natural (Listof Natural))
      (cons n xs)
  )


(define (nwExVar [c : Ctx] ) : (Pairof TyEx Ctx)
     (cons (ExVar (Ctx-vcount c))
           (Ctx (Ctx-env c)
                (Ctx-nt-names c)
                (Ctx-subs c)
                (Ctx-clist c)
                (+ (Ctx-vcount c)  1 )
                (Ctx-ccount c)
                (Ctx-vsrc c)))
  ) 

(define (rstvar [c : Ctx] ) : Ctx
        (Ctx (Ctx-env c) (Ctx-nt-names c) (Ctx-subs c) (Ctx-clist c) 2 (Ctx-ccount c) (Ctx-vsrc c))
  )

(define (nwTyVar [c : Ctx] ) : (Pairof Ty Ctx)
        (let* ([c1 : (Pairof TyEx Ctx) (nwExVar c)]
               [c2 : (Pairof TyEx Ctx) (nwExVar (cdr c1))])
              (cons (Ty (car c1) (car c2)) (cdr c2))
        )
)


(define (nwTyNulVar [nll : Boolean] [ c : Ctx]) : (Pair Ty Ctx)
  (let ([c1 : (Pairof TyEx Ctx) (nwExVar c) ])
       (cons (Ty (Exb nll) (car c1))  (cdr c1)))
)



(define (ctx-pushr [c : Ctx] [r : (Pairof Ty Ty)] ) : Ctx
      (Ctx (Ctx-env c)
           (Ctx-nt-names c)
           (Ctx-subs c)
           (cons (Constr (Ctx-ccount c) (Ty-nul? (car r)) (Ty-nul? (cdr r)))
                 (cons (Constr (+ (Ctx-ccount c) 1)
                               (rename-TyEx  (Ctx-nt-names c) (Ty-headset (car r)))
                               (rename-TyEx  (Ctx-nt-names c) (Ty-headset (cdr r))))
                       (Ctx-clist c)))
           (Ctx-vcount c)
           (+ (Ctx-ccount c) 2)
           (Ctx-vsrc c)
           ) 
  )

(define (ctx-pushr-with-debug [c : Ctx] [r : (Pairof Ty Ty)] [p : PE] ) : Ctx
      (Ctx (Ctx-env c)
           (Ctx-nt-names c) 
           (Ctx-subs c)
           (cons (Constr (Ctx-ccount c) (Ty-nul? (car r)) (Ty-nul? (cdr r)))
                 (cons (Constr (+ (Ctx-ccount c) 1)
                               (rename-TyEx  (Ctx-nt-names c) (Ty-headset (car r)))
                               (rename-TyEx  (Ctx-nt-names c) (Ty-headset (cdr r))))
                       (Ctx-clist c)))
           (Ctx-vcount c)
           (+ (Ctx-ccount c) 2)
           (hash-set* (Ctx-vsrc c) (Ctx-ccount c) p (+ (Ctx-ccount c) 1) p)
           ) 
  )

(define (ctx-getNt [c : Ctx] [v : String]  ) : Ty
         (let ([ns : String (hash-ref (Ctx-nt-names c) v) ])
              (hash-ref (Ctx-env c) ns) )
  )

(define (mk-alt-constraint [x : Ctx]  [v : Ty] [l : Ty] [r : Ty] ) : Ctx
     (ctx-pushr x (cons v (Ty (ExOr (Ty-nul? l) (Ty-nul? r))
                              (ExU (Ty-headset l)
                                   (Ty-headset r))
                           ))
     )
)

(define (mk-cat-constraint [x : Ctx]  [v : Ty] [l : Ty] [r : Ty] ) : Ctx
     (ctx-pushr x (cons v (Ty (ExAnd (Ty-nul? l) (Ty-nul? r))
                              (ExImp (Ty-nul? l)
                                      (Ty-headset l)
                                      (Ty-headset r))
                           ))
     )
)

(define (mk-null-same-headset-constraint [x : Ctx]  [v : Ty] [l : Ty] ) : Ctx
        (ctx-pushr x (cons v (Ty (Exb #t) (Ty-headset l)) )) 
)

(define (mk-var-constraint [x : Ctx] [s : String] [v : Ty]  [tyvar : Ty] ) : Ctx
        (ctx-pushr x (cons v (Ty (Ty-nul? tyvar) (ExU (Ty-headset tyvar) (ExHs (list s))) ) ))
)

(define (mk-const-constraint [x : Ctx] [v : Ty]  [b : Boolean] [h : (Listof String)] )
     (ctx-pushr x (cons v (Ty (Exb b)  (ExHs h)) ) )
)



(define (dbg-alt-constraint [x : Ctx]  [v : Ty] [l : Ty] [r : Ty] [p : PE] ) : Ctx
     (ctx-pushr-with-debug x (cons v (Ty (ExOr (Ty-nul? l) (Ty-nul? r))
                                     (ExU (Ty-headset l)
                                          (Ty-headset r))
                             ))
                             p
     )
)

(define (dbg-cat-constraint [x : Ctx]  [v : Ty] [l : Ty] [r : Ty] [p : PE] ) : Ctx
     (ctx-pushr-with-debug x (cons v (Ty (ExAnd (Ty-nul? l) (Ty-nul? r))
                                     (ExImp (Ty-nul? l)
                                         (Ty-headset l)
                                         (Ty-headset r))
                           ))
                  p
     )
)

(define (dbg-null-same-headset-constraint [x : Ctx]  [v : Ty] [l : Ty] [p : PE]) : Ctx
        (ctx-pushr-with-debug x (cons v (Ty (Exb #t) (Ty-headset l)) )  p) 
)

(define (dbg-var-constraint [x : Ctx] [s : String] [v : Ty]  [tyvar : Ty] [p : PE]) : Ctx
        (ctx-pushr-with-debug x (cons v (Ty (Ty-nul? tyvar) (ExU (Ty-headset tyvar) (ExHs (list s))) ) ) p)
)

(define (dbg-const-constraint [x : Ctx] [v : Ty]  [b : Boolean] [h : (Listof String)] [p : PE] )
     (ctx-pushr-with-debug x (cons v (Ty (Exb b)  (ExHs h)) ) p )
)

(define (vars-of [ e : TyEx] ) : (Listof Natural)
    (match e
        [(ExU  l r)  (set-union (vars-of l) (vars-of r))]
        [(ExAnd l r) (set-union (vars-of l) (vars-of r))]
        [(ExOr l r)  (set-union (vars-of l) (vars-of r))]
        [(ExNot x)   (vars-of x)]
        [(ExImp c h1 h2) (set-union (vars-of c) (vars-of h1) (vars-of h2))]
        [(ExVar n) (list n)] 
        [_ null]
     )
)

(define (solve-self-ref-union [ e : TyEx] [name : (Option Natural)]) :  TyEx
    (match e
        [(ExU  l r)  (ExU  (solve-self-ref-union l name) (solve-self-ref-union r name)) ]
        [(ExImp c h1 h2) (ExImp c (solve-self-ref-union h1 name) (solve-self-ref-union h2 name))]
        [(ExVar n) (cond
                     [(equal? n name) (ExHs '())]
                     [else e] )] 
        [_ e]
     )
)


#;(define (normalizeU [e : TyEx] ) : TyEx
    (match e
        [(ExU (ExHs ys) (ExHs xs)) (ExHs (set-union ys xs))]
        [(ExU l (ExVar n)) (ExU (ExVar n) l)]
        [(ExU l r) (match (cons (normalizeU l) (normalizeU r))

                     [(cons (ExU (ExVar n1) r) (ExU (ExVar n2) r2) ) (ExU (ExVar n1) (ExU (ExVar n2) (ExU r r2)))]
                     [(cons (ExU (ExVar n1) r) (ExU l r2) ) (ExU (ExVar n1) (ExU r (ExU l r2))) ]
                     [(cons (ExU (ExVar n1) r) ld ) (ExU (ExVar n1) (ExU r ld))]
                     [(cons le (ExU (ExVar n1) r) ) (ExU (ExVar n1) (ExU r le))]
                     [(cons (ExU l r) (ExU (ExVar n1) r2) )  (ExU (ExVar n1) (ExU r (ExU l r2)))]
                     [(cons (ExU l r) (ExU l2 r2) )  (ExU l (ExU r (ExU l2 r2))) ]
                     [(cons qq qq2) (ExU qq qq2)]) ]
        [a a]
     )
 )

(define (foldr1 [p : (-> Any Any)] [xs : (Listof Any)] ) : (Listof Any)
      (match xs
          ['() '()]
          [(list x) x]
          [(cons x xs1) (p x (foldr1 p xs1))]
  ))

#;(define (normalizeU [e : TyEx] ) : TyEx
    (match e
        [(ExU _ _) (match (breakU e)
                     [(Pair '() '()) (error "normalizeU: impossible situation !")]
                     [(Pair '() xs)  (foldr1 (lambda (x y) (ExU x y)) xs)]
                     [(Pair  vs '()) (foldr1 (lambda (x y) (ExU x y)) vs)]
                     [(Pair vs xs) (ExU  (foldr1 (lambda (x y) (ExU x y)) vs)
                                         (foldr1 (lambda (x y) (ExU x y)) xs))]
                     [a a])]
        [a a]
     )
 )

#;(define (breakU [e : TyEx] ) : Pair
    (match e
        [(ExU (ExHs ys) (ExHs xs)) (Pair '() (list (ExHs (set-union ys xs))))]
        [(ExU (ExHs ys) (ExVar n)) (Pair  (list (ExVar n) ) (list (ExHs ys)))]
        [(ExU (ExVar n) (ExHs xs)) (Pair  (list (ExVar n) ) (list (ExHs xs)))]
        [(ExU l r) (match (cons (breakU l) (breakU r))
                     [(cons (Pair vs1 xs1) (Pair vs2 ys1) )
                          (Pair  (set-union vs1 vs2) (set-union xs1 ys1))]
                     [(cons (Pair vs1 xs1) (ExVar v))
                           (Pair (set-union vs1 (list (ExVar v))) xs1)]
                     [(cons (ExVar v) (Pair vs1 xs1)) (Pair (set-union vs1 (list (ExVar v)))
                                                           xs1)]
                     #;[(cons  v (Pair vs1 xs1)) (Pair vs1 (list (ExU v (car xs1))) )]
                     [(cons w v) (Pair '() (list (ExU w v)))]
                     )]
        [a (Pair '() (list a))]
     )
 )

(define (reduce-ex [e : TyEx]) : TyEx
    (match e
        [(ExU  l r)               (match (cons (reduce-ex l) (reduce-ex r))
                                       [(cons (ExHs (list)) rr) rr ]
                                       [(cons ll (ExHs (list)) ) ll ]
                                       [(cons (ExHs xs) (ExHs ys)) (ExHs (set-union xs ys)) ]
                                       [(cons l1 r1) (ExU l1 r1)])]
        [(ExAnd l r)              (match (cons (reduce-ex l) (reduce-ex r))
                                       [(cons (Exb #f) _) (Exb #f) ]
                                       [(cons _ (Exb #f)) (Exb #f) ]
                                       #;[(cons (Exb #t) y) y ]
                                       #;[(cons x (Exb #t)) x ]
                                       [(cons (Exb x) (Exb y)) (Exb (and x y)) ]
                                       [(cons l1 r1) (ExAnd l1 r1)])]
        [(ExOr l r)               (match (cons (reduce-ex l) (reduce-ex r))
                                       [(cons (Exb #t) _) (Exb #t) ]
                                       [(cons _ (Exb #t)) (Exb #t) ]
                                       [(cons (Exb x) (Exb y)) (Exb (or x y)) ]
                                       [(cons l1 r1) (ExOr l1 r1)])]
        [(ExNot x)               (match (reduce-ex x) 
                                       [(Exb x) (Exb (not x)) ]
                                       [z      (ExNot z)])]
        [(ExImp c h1 h2)         (match (reduce-ex c) 
                                       [(Exb #f)  h1]
                                       [(Exb #t)  (reduce-ex  (ExU h1 h2))]
                                       [z   (ExImp z h1 h2)])]
        [x x]
     )
  )


(define (unify-ex [t1 : TyEx] [t2 : TyEx] [cv : Natural]) :
      (Option (Pairof (Listof (Pairof Natural TyEx)) Natural))
    (match (cons t1 t2)
        [(cons (ExVar n)     (Exb b))     (cons (list (cons n t2) ) cv)]
        [(cons (ExVar n)     (ExHs l))    (cons (list (cons n t2) ) cv)]
        ;[(cons (Exb b)       (ExVar n))   (cons (list (cons n t2) ) cv)]
        ;[(cons (ExHs l)      (ExVar n))   (cons (list (cons n t2) ) cv)]
        #;[(cons (ExVar n1)    (ExVar n2))  (let ([nv : TyEx (ExVar cv)])
                                               (cons (list (cons n1 nv)
                                                           (cons n2 nv))
                                                     (+ cv 1)))]
        ;[(cons (ExVar n1)    (ExU (ExVar n1) r))  (cons (list (cons n1 r)) cv)]
        ;[(cons (ExVar n1)    (ExU l (ExVar n1)))  (cons (list (cons n1 l)) cv)]
        ;[(cons (ExVar n1)    (ExAnd (ExVar n1) (Exb #f))) (list (cons n1 #f))]
        ;[(cons (ExVar n1)    (ExAnd (Exb #f) (ExVar n1))) (list (cons n1 #f))]
        ;[(cons (ExVar n1)    (ExOr (ExVar n1) (Exb #t))) (list (cons n1 #t))]
        ;[(cons (ExVar n1)    (ExOr (Exb #t) (ExVar n1))) (list (cons n1 #t))]
        ;[(cons (ExVar n1)    (ExU l r))  (cons (list (cons n1 t2)) cv)]
        [(cons x x) (cons (list) cv)]
        [_ #f]
     )
  )

(define (red-norm [ xs : (Option (Listof (Pairof Natural TyEx))) ])
  (cond
    [xs  (map (lambda ([ pp : (Pairof Natural TyEx)])
                      (cons (car pp) (reduce-ex (cdr pp) #;(normalizeU (cdr pp)) ) ))
             xs
          )]
    [else #f])
)

#;(define (unify  [t1 : Ty] [t2 : Ty] [cv : Natural]) :
       (Option (Pairof (Listof (Pairof Natural TyEx)) Natural))
    (match (cons t1 t2)
        [(cons (Ty b1 h1)  (Ty b2 h2))   (let* ([h3 : (Option (Pairof (Listof (Pairof Natural TyEx)) Natural))
                                                     (unify-ex h1 h2 cv) ]
                                                [b3 : (Option (Pairof (Listof (Pairof Natural TyEx)) Natural))
                                                      (cond [h3 (unify-ex b1 b2 (cdr h3))]
                                                            [else #f])]
                                                [r : (Option (Listof (Pairof Natural TyEx)))
                                                     (cond
                                                       [(and h3 b3) (red-norm (compose-sub-list (car h3)
                                                                                                (car b3)))]
                                                       [else #f])])
                                                (cond
                                                  [(and r b3) (cons r (cdr b3))]
                                                  [else #f]))]
        [_ #f]
     )
  )


(define (compose-sub-list [s1 : (Option (Listof (Pairof Natural TyEx)))]
                          [s2 : (Option (Listof (Pairof Natural TyEx)))]) : (Option (Listof (Pairof Natural TyEx)))
        (cond
          [(and s1 s2) (append (map (lambda ([x : (Pairof Natural TyEx)])
                                    (cons (car x)  (apply-sub-list s2 (cdr x))) ) s1)
                               s2)]
          [else #f])
  )

(define (apply-sub-list [s : (Option (Listof (Pairof Natural TyEx)))]
                        [t : TyEx]) :  TyEx
        (match t
          [(ExVar n) (let ([ x : (Option (Pairof Natural TyEx)) (cond [s (assoc n s)] [else #f]) ])
                          (cond [x (cdr x)]
                                [else t]))]
          [(ExAnd t1 t2) (ExAnd (apply-sub-list s t1) (apply-sub-list s t2))]
          [(ExOr t1 t2) (ExOr (apply-sub-list s t1) (apply-sub-list s t2))]
          [(ExU t1 t2)  (ExU (apply-sub-list s t1) (apply-sub-list s t2) )]
          [(ExNot t)    (ExNot (apply-sub-list s t) )]
          [(ExImp c t1 t2) (ExImp (apply-sub-list s c) (apply-sub-list s t1) (apply-sub-list s t2) )]
          [x x])
  )

(define (apply-sub [h : (Hash Natural TyEx)] [t : TyEx] ) : TyEx
   (match t
     [(ExVar n) (hash-ref h n (lambda () t)) ]
     [(ExU t1 t2) (ExU (apply-sub h t1) (apply-sub h t2)) ]
     [(ExAnd t1 t2) (ExAnd (apply-sub h t1) (apply-sub h t2)) ]
     [(ExOr t1 t2) (ExOr (apply-sub h t1) (apply-sub h t2)) ]
     [(ExNot t1) (ExNot (apply-sub h t1)) ]
     [(ExImp c t1 t2) (ExImp (apply-sub h c) (apply-sub h t1) (apply-sub h t2))]
     [_ t]
  )
)

(define (apply-sub-ty [h : (Hash Natural TyEx)] [ty : Ty] ) : Ty
      (Ty (apply-sub h (Ty-nul? ty)) (reduce-ex (apply-sub h (Ty-headset ty)) #;(normalizeU (apply-sub h (Ty-headset ty)))))
  )

(define (sub-ctx [ x : Ctx]) : Ctx
   (Ctx (make-immutable-hash (hash-map (Ctx-env x)
                                       (lambda ([s : String] [t : Ty])
                                               (cons s  (apply-sub-ty (Ctx-subs x) t) ))))
        (Ctx-nt-names x)
        (Ctx-subs x)
        (map (lambda ([c : Constr ])
                     (Constr (Constr-uid c)
                             (Constr-lft c)
                             (reduce-ex (apply-sub (Ctx-subs x) (Constr-rght c)) #;( normalizeU (apply-sub (Ctx-subs x) (Constr-rght c)))) ))
             (Ctx-clist x) )
        (Ctx-vcount x)
        (Ctx-ccount x)
        (Ctx-vsrc x)
        )
)

(: anyof (All (a) (-> (-> a Boolean) (Listof a) Boolean) ))
(define (anyof f xs)
    (cond
      [(null? xs) #f]
      [else (or (f (car xs)) (anyof f (cdr xs)))])
  )

#;(define (compose [s : (Hash Natural TyEx)]
                 [l : (Listof (Pairof Natural TyEx))]) : (Option (Hash Natural TyEx))
    (cond
       [(null? l) s]
       [(anyof (lambda ([n : (Pairof Natural TyEx)])
                (hash-has-key? s (car n)))  l) #f]
       [else (let* ([h : (Hash Natural TyEx) (make-immutable-hash l)]
                    [s1  : (Hash Natural TyEx)
                           (cast (hash-map/copy s (lambda ([k : Natural] [t : TyEx])
                                                          (values k (apply-sub h t) )) )
                                 (Hash Natural TyEx))]
                   )
                   (foldl (lambda ([z : (Pairof Natural TyEx)] [h2 :  (Hash Natural TyEx)])
                          (hash-set h2 (car z) (cdr z)) ) s1 l)
    )]
  )
 )



(define (compose-s  [s : (Hash Natural TyEx)] [l : (Listof (Pairof Natural TyEx))]) : (Hash Natural TyEx)
    (cond
       [(null? l) s]
        [else (let* ([h : (Hash Natural TyEx) (make-immutable-hash l)]
                     [s1 : (Hash Natural TyEx)
                           (cast (hash-map/copy s (lambda ([k : Natural] [t : TyEx])
                                                          (values k  (reduce-ex  (apply-sub h t) #;( normalizeU (apply-sub h t))) )) )
                                 (Hash Natural TyEx))]
                  
                    )
                    (foldl (lambda ([z : (Pairof Natural TyEx)] [h2 :  (Hash Natural TyEx)])
                           (hash-set h2 (car z) (cdr z)) ) s1 l)
    )]
  )
 )



(define (solve-iterate [x : Ctx]
                        [l : (Listof Constr)]
                        ) : (Pairof Ctx (Listof Constr))
     (cond
       [(null? (Ctx-clist x) ) (cons x l)]
       [else (let* ([ctr : Constr (car (Ctx-clist x))]
                    [ctrl : TyEx (reduce-ex (apply-sub (Ctx-subs x) (Constr-lft ctr))
                               #;(normalizeU (apply-sub (Ctx-subs x) (Constr-lft ctr))))]
                    [lvname : (Option Natural) (match ctrl
                                                 [(ExVar n) n]
                                                 [_ #f]) ]
                    [ctrr : TyEx (reduce-ex (solve-self-ref-union (apply-sub (Ctx-subs x) (Constr-rght ctr))
                                                                  lvname)
                                          #;(normalizeU (apply-sub (Ctx-subs x) (Constr-rght ctr))))]
                    [r : (Option (Pairof (Listof (Pairof Natural TyEx)) Natural))
                         (unify-ex ctrl ctrr (Ctx-vcount x))]
                    [ht :  (Hash Natural TyEx) (cond
                                                 [r (compose-s (Ctx-subs x) (car r))]
                                                 [else (Ctx-subs x)])]
                    [x1 : Ctx
                        (cond
                          [r    (Ctx (Ctx-env x) (Ctx-nt-names x) ht (cdr (Ctx-clist x)) (cdr r) (Ctx-ccount x) (Ctx-vsrc x))]
                          [else (Ctx (Ctx-env x) (Ctx-nt-names x) ht (cdr (Ctx-clist x)) (Ctx-vcount x) (Ctx-ccount x) (Ctx-vsrc x))])])
                  (cond
                    [r     (solve-iterate x1 l)]
                    [else  (solve-iterate x1 (cons (Constr (Constr-uid ctr) ctrl ctrr) l) ) ] ))])
  )



(define (solve-helper [n : Natural] [x : Ctx]) : Ctx 
     (let* ([res : (Pairof Ctx (Listof Constr)) (solve-iterate x null)]
            [y : Ctx (car res)]
            [k : Natural (length (cdr res))]
            [z : Ctx (Ctx (Ctx-env y)
                          (Ctx-nt-names x)
                          (Ctx-subs y)
                          (cdr res)
                          (Ctx-vcount y)
                          (Ctx-ccount y)
                          (Ctx-vsrc y)) ])
           (cond
             [(eq? (abs (- k n)) 0) (sub-ctx z)]
             [else (solve-helper k (sub-ctx z))])
       )    
  )

(define (solve-ctx [x : Ctx]) : Ctx 
     (solve-helper 0 x) 
  )

(define (get-start-exp-ty [x : Ctx]) : Ty
   (let ([t (reverse-names x)]
         [nwst (apply-sub-ty (Ctx-subs x) (start-exp-var))])
        (Ty (Ty-nul? nwst) 
            (ExHs (map (lambda (z) (hash-ref t z)) (ExHs-headset (Ty-headset nwst))))) 
         
  ))

(define (isntancied-ty? [t : Ty]) : Boolean
   (match t
          [(Ty (Exb _ ) (ExHs _)) #t]
          [else #f])
  )

(define (nt-occurs [x : Ctx]) :  (Listof  Natural )
    (map (lambda ([z : (Pairof Natural Ty)]) (car z))
              (filter (lambda ([nt-ty : (Pairof Natural Ty)])
                              (match nt-ty
                                  [(cons s (Ty _ (ExHs h))) (member s h)]
                                  [_ #f]) )
              (hash->list (Ctx-env x))))
  )

(define (all-nt-solved? [x : Ctx]) : Boolean
      (foldr (lambda ([a : Boolean] [b : Boolean]) (and a b) )
             #t
            (map (lambda ([z : (Pairof String Ty)])
                         (isntancied-ty? (cdr z)))
                           
                (hash->list (Ctx-env x))))
  )

(define (unsolved-nts [x : Ctx]) : (Listof Natural)
      (foldr (lambda ([a : (Pairof Natural Boolean)] [b : (Listof String)])
                     (if (not (cdr a)) (cons (car a) b) b))
             (list)
            (map (lambda ([z : (Pairof Natural Ty)])
                         (cons (car z) (isntancied-ty? (cdr z))))
                           
                (hash->list (Ctx-env x))))
  )


(define (satisfied? [x : Ctx]) : Boolean
              (and (all-nt-solved? x)
                   (isntancied-ty? (get-start-exp-ty x))
                   (null? (Ctx-clist x))
                   (null? (nt-occurs x)))
  )

(define (unsolved-pe [x : Ctx]) : (Listof PE)
      (foldr (lambda ([ c : Constr] [xs : (Listof PE)])
                     (cond
                       [(hash-has-key? (Ctx-vsrc x) (Constr-uid c))
                         (cons  (hash-ref (Ctx-vsrc x) (Constr-uid c))
                               xs)]
                       [else xs]))
             (list)
             (Ctx-clist x))
  )

(define (get-errors [x : Ctx] ) : TyErr
  (let ([t (reverse-names x)])
        (TyErr (map (lambda (y) (hash-ref (Ctx-nt-names x) y)) (nt-occurs x))
               (set-union (map (lambda (z) (hash-ref t z)) (unsolved-nts x))
                          (unsolved-pe x))))
  )


(define (tyEx->string [t : TyEx]) : String
     (match t
       [(ExHs hs) (string-join	 hs "," #:before-first "[" #:after-last "]")]
       [(Exb n)   (~a n)]
       [(ExVar n) (string-append "V#" (~a n))]
       [(ExU l r) (string-append (tyEx->string l) "∪" (tyEx->string r))]
       [(ExAnd l r) (string-append (tyEx->string l) "∧" (tyEx->string r))]
       [(ExOr l r) (string-append (tyEx->string l)  "∨" (tyEx->string r))]
       [(ExNot e) (string-append "¬" (tyEx->string e))]
       [(ExImp e h1 h2) (string-append (tyEx->string e) "=>" (tyEx->string h1) " : " (tyEx->string h2))]
      ; ['() "(:-0)"]
      )
  )

;(struct Constr ([uid : Natural] [lft : TyEx] [rght : TyEx]) #:transparent)
(define (constr->string [c : Constr]) : String
    (string-append (number->string (Constr-uid c))
                   " : "
                   (tyEx->string   (Constr-lft c))
                   " = "
                   (tyEx->string   (Constr-rght c)))
  )
(define (constr->string-hsh [tab : (Hash Natural String)] [c : Constr]) : String
    (string-append (number->string (Constr-uid c))
                   " : "
                   (tyEx->string   (reverse-name-TyEx tab (Constr-lft c)))
                   " = "
                   (tyEx->string   (reverse-name-TyEx tab (Constr-rght c))))
  )

(define (ty->string [t : Ty])
     (string-append "<" (tyEx->string (Ty-nul? t)) ", " (tyEx->string (Ty-headset t) ) ">")
  )

(define (ty->string-hsh [tab : (Hash Natural String)] [t : Ty])
     (string-append "<" (tyEx->string (Ty-nul? t)) ", "
                    (tyEx->string (reverse-name-TyEx tab (Ty-headset t))) ">")
  )

(define (ctx-tyEnv-toString [c : ctx]) : String
      (let ([t (reverse-names c)])
           (map (lambda (x)
                (string-append (hash-ref t (car x))
                               "::"
                               (ty->string-hsh t (cdr x)) "\n"))
                (hash->list (Ctx-env c))))
  )

(define (ctx-constr-toString [c : ctx]) : String
      (let ([t (reverse-names c)])
           (map (lambda ([x : Constr]) : String
                (string-append (constr->string-hsh t x) "\n"))
                (Ctx-clist c)))
  )

(define (ctx-subst-toString [c : ctx]) : String
      (let ([t (reverse-names c)])
           (string-join (hash-map (Ctx-subs c)
                                  (lambda ([n : Natural] [tex : TyEx]) : String
                                          (string-append "V#" (~a n) " :=> " (tyEx->string (reverse-name-TyEx t tex)))))
                                          " \n"))
  )