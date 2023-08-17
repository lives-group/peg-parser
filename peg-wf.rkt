#lang typed/racket
;#lang typed/racket/no-check

(require "peg-ast.rkt")
(require/typed racket/hash
               [hash-union (All (a b) (-> (Immutable-HashTable a b) * (Immutable-HashTable a b)))])

(provide type-infer)


(define (start-exp-var)
  (Ty (ExVar 0) (ExVar 1))
  )


; Parsing Expressions Derivates Definitions
; A mirror defininitio of PE, except for the adition for
; tracking pending derivate and delta computations.

(struct Ctx ([env : (Hash String Ty)]   ; ψ
             [subs : (Hash Natural TyEx)] ; ϕ
             [clist : (Listof (Pairof TyEx TyEx)) ] ; term ≡ term
             [vcount : Natural]
            )
        #:prefab)

(define (nwExVar [c : Ctx] ) : (Pairof TyEx Ctx)
     (cons (ExVar (Ctx-vcount c))
           (Ctx (Ctx-env c) (Ctx-subs c) (Ctx-clist c)  (+ (Ctx-vcount c)  1)))
  )

(define (rstvar [c : Ctx] ) : Ctx
        (Ctx (Ctx-env c) (Ctx-subs c) (Ctx-clist c) 2)
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
           (Ctx-subs c)
           (cons (cons (Ty-nul? (car r)) (Ty-nul? (cdr r)))
                                          (cons (cons (Ty-headset (car r)) (Ty-headset (cdr r)))
                                                (Ctx-clist c)))
           (Ctx-vcount c)
           ) 
  )

(define (ctx-recordNt [c : Ctx] [v : String]  [t : Ty] ) : Ctx
         (Ctx (hash-set (Ctx-env c) v t) (Ctx-subs c) (Ctx-clist c) (Ctx-vcount c))
  )

(define (ctx-getNt [c : Ctx] [v : String]  ) : Ty
         (hash-ref (Ctx-env c) v) 
  )

(define-type Term (Union Ty ))

(struct Ty   ([nul? : TyEx] [headset : TyEx] ) #:prefab)


(define-type TyEx (Union ExHs Exb ExVar ExU ExNot ExOr ExAnd ExImp))
(struct ExHs   ([headset : (Listof String)] ) #:prefab)
(struct Exb   ([nul : Boolean] ) #:prefab)
(struct ExVar  ([name : Natural]) #:prefab)
(struct ExU    ([left : TyEx] [right : TyEx])  #:prefab)
(struct ExAnd  ([left : TyEx] [right : TyEx]) #:prefab)
(struct ExOr   ([left : TyEx] [right : TyEx]) #:prefab)
(struct ExNot  ([not : TyEx] ) #:prefab)
(struct ExImp  ([cd : TyEx] [hs1 : TyEx] [hs2 : TyEx]) #:prefab)


(define (constraints-of [p : PE] [v : Ty] [x : Ctx]) : Ctx
     (match p
        [(Any _)        (ctx-pushr x (cons v (Ty (Exb #f) (ExHs null)) ))]
        [(Eps _)        (ctx-pushr x (cons v (Ty (Exb #t) (ExHs null)) ))]
        [(Sym _ c)      (ctx-pushr x (cons v (Ty (Exb #f) (ExHs null)) ))]
        [(Var _ s)      (let ([t : Ty (ctx-getNt x s) ])
                             (ctx-pushr x (cons v (Ty (Ty-nul? t)
                                                      (ExU (Ty-headset t) (ExHs (list s))) ) ))) ]
        [(Cat _ p1 p2)  (let* ([cv1 : (Pair Ty Ctx) (nwTyVar x)]
                               [cv2 : (Pair Ty Ctx) (nwTyVar (cdr cv1))]
                               [c1 : Ctx (constraints-of p1 (car cv1) (cdr cv2))]
                               [c2 : Ctx (constraints-of p2 (car cv2) c1)])
                               (ctx-pushr c2 (cons v (Ty (ExAnd (Ty-nul? (car cv1)) (Ty-nul? (car cv2)))
                                                       (ExImp (Ty-nul? (car cv1))
                                                              (Ty-headset (car cv1))
                                                              (Ty-headset (car cv2))) ))) )]
        [(Alt _ p1 p2)  (let* ([cv1 : (Pair Ty Ctx) (nwTyVar x)]
                               [cv2 : (Pair Ty Ctx) (nwTyVar (cdr cv1))]
                               [c1 : Ctx (constraints-of p1 (car cv1) (cdr cv2))]
                               [c2 : Ctx (constraints-of p2 (car cv2) c1)])
                               (ctx-pushr c2 (cons v (Ty (ExOr (Ty-nul? (car cv1)) (Ty-nul? (car cv2)))
                                                         (ExU (Ty-headset (car cv1)) (Ty-headset (car cv2))) ))) )]
        [(Rep _ p)      (let* ([v1 : (Pairof Ty Ctx) (nwTyNulVar #f x)]
                               [v2 : Ty (Ty (Exb #t) (Ty-headset (car v1)))]
                               [c1 : Ctx (constraints-of p (car v1) (cdr v1))])
                             (ctx-pushr c1 (cons v v2)) )]
        [(Not _ p)      (let* ([v1 : (Pairof Ty Ctx) (nwTyVar x)]
                               [v2 : Ty (Ty (Exb #t) (Ty-headset (car v1)))]
                               [c1 : Ctx    (constraints-of p (car v1) (cdr v1))])
                             (ctx-pushr c1 (cons v  v2)) )]
     )
  )

(define (peg->constraints [p : PEG] ) : Ctx
    (let* ([nts : (Listof String) (hash-keys (PEG-vars p))]
           [inital-ctx : Ctx (build-ctx nts)]
           [rules : (Listof (Pairof String PE)) (hash->list (PEG-vars p)) ]
           )
         (constraints-of-rules rules
                               (constraints-of (PEG-start p) (start-exp-var) inital-ctx))
  )
)


(define (mkTyList [xs : (Listof String)] [r : (Listof (Pairof String Ty))] [start : Natural] ) : (Pairof  (Listof (Pairof String Ty)) Natural)
   (cond
     [(null? xs) (cons r start)]
     [else (mkTyList (cdr xs)
                     (cons (cons (car xs) (Ty (ExVar start) (ExVar (+ start 1)))) r)
                     (+ start 2))] )
  )

(define (build-ctx [xs : (Listof String)]) : Ctx
    (let* ([l : (Pairof (Listof (Pairof String Ty)) Natural)
               (mkTyList xs null 2)]
           [m : (Hash String Ty) (make-immutable-hash (car l))]) 
          (Ctx m (make-immutable-hash) (list) (cdr l))
       )
  )

(define (constraints-of-rules [xs : (Listof (Pair String PE))] [x : Ctx]) : Ctx
          (foldr (lambda ([r : (Pairof String PE)] [x1 : Ctx])
                         (constraints-of (cdr r) (ctx-getNt x1 (car r)) x1)) x xs)      
 )

(define (reduce-term [t : Ty]) : Ty
     (Ty (reduce-ex (Ty-nul? t)) (reduce-ex (Ty-headset t)) )
  )

(define (reduce-ex [e : TyEx]) : TyEx
    (match e
        [(ExU  l r)               (match (cons (reduce-ex l) (reduce-ex r))
                                       [(cons (ExHs (list)) rr) rr ]
                                       [(cons ll (ExHs (list)) ) ll ]
                                       [(cons (ExHs xs) (ExHs ys)) (ExHs (set-union xs ys)) ]
                                       [(cons l1 r1) e])]
        [(ExAnd l r)              (match (cons (reduce-ex l) (reduce-ex r))
                                       [(cons (Exb #f) _) (Exb #f) ]
                                       [(cons _ (Exb #f)) (Exb #f) ]
                                       #;[(cons (Exb #t) y) y ]
                                       #;[(cons x (Exb #t)) x ]
                                       [(cons (Exb x) (Exb y)) (Exb (and x y)) ]
                                       [(cons l1 r1) e])]
        [(ExOr l r)               (match (cons (reduce-ex l) (reduce-ex r))
                                       [(cons (Exb #t) _) (Exb #t) ]
                                       [(cons _ (Exb #t)) (Exb #t) ]
                                       [(cons (Exb x) (Exb y)) (Exb (or x y)) ]
                                       [(cons l1 r1) e])]
        [(ExNot x)               (match (reduce-ex x) 
                                       [(Exb x) (Exb (not x)) ]
                                       [_      e])]
        [(ExImp c h1 h2)         (match (reduce-ex c) 
                                       [(Exb #f)  h1]
                                       [(Exb #t)  (reduce-ex  (ExU h1 h2))]
                                       [_ e])]
        [x x]
     )
  )


(define (unify-ex [t1 : TyEx] [t2 : TyEx] [cv : Natural]) : (Option (Pairof (Listof (Pairof Natural TyEx)) Natural))
    (match (cons t1 t2)
        [(cons (ExVar n)     (Exb b))     (cons (list (cons n t2) ) cv)]
        [(cons (ExVar n)     (ExHs l))    (cons (list (cons n t2) ) cv)]
        ;[(cons (Exb b)       (ExVar n))   (cons (list (cons n t2) ) cv)]
        ;[(cons (ExHs l)      (ExVar n))   (cons (list (cons n t2) ) cv)]
        [(cons (ExVar n1)    (ExVar n2))  (let ([nv : TyEx (ExVar cv)])
                                               (cons (list (cons n1 nv)
                                                           (cons n2 nv))
                                                     (+ cv 1)))]
        [(cons (ExVar n1)    (ExU (ExVar n1) r))  (cons (list (cons n1 r)) cv)]
        [(cons (ExVar n1)    (ExU l (ExVar n1)))  (cons (list (cons n1 l)) cv)]
        ;[(cons (ExVar n1)    (ExAnd (ExVar n1) (Exb #f))) (list (cons n1 #f))]
        ;[(cons (ExVar n1)    (ExAnd (Exb #f) (ExVar n1))) (list (cons n1 #f))]
        ;[(cons (ExVar n1)    (ExOr (ExVar n1) (Exb #t))) (list (cons n1 #t))]
        ;[(cons (ExVar n1)    (ExOr (Exb #t) (ExVar n1))) (list (cons n1 #t))]
        [(cons x x) (cons (list) cv)]
        [_ #f]
     )
  )

(define (unify  [t1 : Ty] [t2 : Ty] [cv : Natural]) : (Option (Pairof (Listof (Pairof Natural TyEx)) Natural))
    (match (cons t1 t2)
        [(cons (Ty b1 h1)  (Ty b2 h2))   (let* ([h3 : (Option (Pairof (Listof (Pairof Natural TyEx)) Natural))
                                                     (unify-ex h1 h2 cv) ]
                                                [b3 : (Option (Pairof (Listof (Pairof Natural TyEx)) Natural))
                                                      (cond [h3 (unify-ex b1 b2 (cdr h3))]
                                                           [else #f])]
                                                [r : (Option (Listof (Pairof Natural TyEx)))
                                                     (cond
                                                       [(and h3 b3) (compose-sub-list (car h3) (car b3))]
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
          [(and s1 s2) (map (lambda ([x : (Pairof Natural TyEx)])
                                    (cons (car x) (apply-sub-list s2 (cdr x))) ) s1) ]
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
      (Ty (apply-sub h (Ty-nul? ty)) (apply-sub h (Ty-headset ty)))
  )

(define (sub-ctx [ x : Ctx]) : Ctx
   (Ctx (make-immutable-hash (hash-map (Ctx-env x)
                                       (lambda ([s : String] [t : Ty])
                                               (cons s (apply-sub-ty (Ctx-subs x) t) ))))
        (Ctx-subs x)
        (map (lambda ([c : (Pairof TyEx TyEx)])
                     (cons (car c) (reduce-ex (apply-sub (Ctx-subs x) (cdr c))) )) (Ctx-clist x) )
        (Ctx-vcount x)
        )
)

(: anyof (All (a) (-> (-> a Boolean) (Listof a) Boolean) ))
(define (anyof f xs)
    (cond
      [(null? xs) #f]
      [else (or (f (car xs)) (anyof f (cdr xs)))])
  )

(define (compose [s : (Hash Natural TyEx)] [l : (Listof (Pairof Natural TyEx))]) : (Option (Hash Natural TyEx))
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
                                                          (values k (apply-sub h t) )) )
                                 (Hash Natural TyEx))]
                  
                    )
                    (foldl (lambda ([z : (Pairof Natural TyEx)] [h2 :  (Hash Natural TyEx)])
                           (hash-set h2 (car z) (cdr z)) ) s1 l)
    )]
  )
 )



(define (solve-iterate [x : Ctx]
                        [l : (Listof (Pairof TyEx TyEx))]
                        ) : (Pairof Ctx (Listof (Pairof TyEx TyEx)))
     (cond
       [(null? (Ctx-clist x) ) (cons x l)]
       [else (let* ([ctr : (Pairof TyEx TyEx) (car (Ctx-clist x))]
                    [ctrl : TyEx (reduce-ex (apply-sub (Ctx-subs x) (car ctr)))]
                    [ctrr : TyEx (reduce-ex (apply-sub (Ctx-subs x) (cdr ctr)))]
                    [r : (Option (Pairof (Listof (Pairof Natural TyEx)) Natural))
                         (unify-ex ctrl ctrr (Ctx-vcount x))]
                    [ht :  (Hash Natural TyEx) (cond
                                                 [r (compose-s (Ctx-subs x) (car r))]
                                                 [else (Ctx-subs x)])]
                    [x1 : Ctx
                        (cond
                          [r    (Ctx (Ctx-env x) ht (cdr (Ctx-clist x)) (cdr r))]
                          [else (Ctx (Ctx-env x) ht (cdr (Ctx-clist x)) (Ctx-vcount x))])])
                  (cond
                    [r (solve-iterate x1 l)]
                    [else      (solve-iterate x1 (cons (cons ctrl ctrr) l) ) ] ))])
  )



(define (solve-helper [n : Natural] [x : Ctx]) : Ctx 
     (let* ([res : (Pairof Ctx (Listof (Pairof TyEx TyEx))) (solve-iterate x null)]
            [y : Ctx (car res)]
            [k : Natural (length (cdr res))]
            [z : Ctx (Ctx (Ctx-env y) (Ctx-subs y) (cdr res) (Ctx-vcount y)) ])
           (cond
             [(eq? (abs (- k n)) 0) (sub-ctx z)]
             [else (solve-helper k (sub-ctx z))])
       )
       
  )

(define (type-infer [p : PEG]) : Ctx 
     (let ([c : Ctx (solve-helper 0 (peg->constraints p))])
           c )
     
  )


(define (true? [v : Any]) : Boolean
    (match v
      [#f #f]
      [_  #t]
      )
  )

(define (nt-occurs [x : Ctx]) :  (Listof  String )
    (map (lambda ([z : (Pairof String Ty)]) (car z))
              (filter (lambda ([nt-ty : (Pairof String Ty)])
                              (match nt-ty
                                  [(cons s (Ty _ (ExHs h))) (member s h)]
                                  [_ #f]) )
              (hash->list (Ctx-env x))))
  )


(define L0 : SrcLoc (SrcLoc 0 0))

(define pegtest1 : PEG
  (PEG  (make-immutable-hash
         (list (cons "A" (Rep L0 (Sym L0 #\a) ) )
               (cons "B" (Cat L0 (Rep L0 (Sym L0 #\a) ) (Var L0 "B")))
               )
         )
         (Cat L0 (Var L0 "A") (Var L0 "B")) ))

(define pegtest2 : PEG
  (PEG  (make-immutable-hash)
         (Rep L0 (Sym L0 #\a) ) )
)

(define pegtest3 : PEG
  (PEG  (make-immutable-hash
         (list (cons "B" (Cat L0  (Eps L0) (Var L0 "B")))
               )
         )
         (Var L0 "B") ) )

(define pegtest4 : PEG
  (PEG  (make-immutable-hash)
         (Rep L0 (Eps L0)) )
 )

(define pegtest5 : PEG
  (PEG  (make-immutable-hash
         (list (cons "A" (Rep L0 (Sym L0 #\a) ) )
               (cons "B" (Rep L0 (Var L0 "A") ))
               )
         )
         (Var L0 "B")) )