#lang racket
;#lang typed/racket/no-check

(require "../peg-ast.rkt"
         "../peg-wf.rkt"
         "../tysolver.rkt"
         rackcheck
         rackunit
         peg-gen
         peg-gen/peg-gen-syntax
         peg-gen/peg-gen-types)

(provide  accept-well-typed
          reject-ill-typed)

(define l0 (SrcLoc 0 0))
(setSynFactory PEGStructF)

(define (translate-ex gpeg)  
     (match gpeg
        [(GEps )        (Eps l0)]
        [(GLit c)       (Sym l0 (integer->char (+ 65 c)))]
        [(GVar s)       (Var l0 #f (symbol->string s))]
        [(GSeq p1 p2)   (Cat l0 (translate-ex p1) (translate-ex p2))]
        [(GAlt p1 p2)   (Alt l0 (translate-ex p1) (translate-ex p2))]
        [(GKle p)       (Rep l0 (translate-ex p))]
        [(GNot p)       (Not l0 (translate-ex p))]
     )
  )

(define (translate-nt xs)
     (map (lambda (p) (cons (symbol->string (car p)) (translate-ex (cdr p))) ) xs)
  )

(define (translate gpeg)  
     (PEG "grmtest" (make-immutable-hash (translate-nt (hash->list (GPEG-nt gpeg))) )
          (translate-ex (GPEG-start gpeg)) )
  )

(define-property accept-well-typed ([peg  (gen:peg 3 3 3)])
    (satisfied? (solve-ctx (peg->constraints (translate peg))))
  )

(define-property reject-ill-typed ([peg  (gen:ill-peg 3 3 3)])
    (let ([p (translate peg) ])
        (pprint-peg p)
        (not (satisfied? (solve-ctx (peg->constraints p))))
        (displayln "\n------------------xxx-----------")
    )
  )



;
; Some interesting counter-examples
;

(define peg-f1 (GPEG (make-immutable-hash (list (cons 'A (GAlt (GSeq (GLit 0) (GLit 0))
                                                               (GSeq (GEps) (GLit 0)) ))))
                             (GSeq (GKle (GVar 'A))
                                   (GAlt (GLit 0) (GLit 0)))
                             (list (cons 'A (TyPEG #f '()) )))
; This one was a foll error on mkSeqConstraint 
  )


(define peg-f2
  (GPEG (make-immutable-hash (list (cons 'A  (GSeq (GAlt (GLit 3) (GLit 3))
                                                   (GSeq (GVar 'B) (GEps))))
                                   (cons 'B  (GNot (GAlt (GLit 2) (GVar 'A))))))
        (GSeq (GSeq (GVar 'B) (GVar 'A))
              (GAlt (GLit 1) (GLit 0)))
        (list (cons 'A  (TyPEG #f '()))
              (cons 'B  (TyPEG #t '(A)))))
  ; Allowed unification of vars to terms of union.
  ; Needed test to verify if all variables are instanciated.
  )


(define peg-ill1
  (GPEG (make-immutable-hash (list (cons 'O  (GSeq (GSeq (GEps) (GEps))
                                             (GAlt (GLit 4) (GVar 'T))))
                                   (cons 'Q  (GAlt (GSeq (GLit 4) (GEps)) (GSeq (GVar 'T) (GEps))))
                                   (cons 'T  (GSeq (GSeq (GEps) (GLit 1))
                                             (GAlt (GEps) (GEps))))))
        (GSeq (GNot (GEps)) (GAlt (GLit 4) (GEps)))
        (list (cons 'T  (TyPEG #f '()))
              (cons 'O  (TyPEG #f '(T)))
              (cons 'Q 'ill-typed)))
 )

;V<-(ϵ/ϵ)V/V/V
;T<-(V/'A')X'C'/!'C'!'C'
;X<-!(T'D''A''C')
;('A'/ϵ)ϵ'B'/!'B''C''A'
(define peg-ill2
   (GPEG (make-immutable-hash (list (cons 'V (GAlt (GAlt (GSeq (GAlt (GEps) (GEps)) (GVar 'V) )
                                                   (GVar 'V))
                                                   (GVar 'V)))
                                    (cons 'T (GAlt (GSeq (GSeq (GAlt (GVar 'V) (GLit 0)) (GVar 'X)) (GLit 2))
                                                   (GSeq (GNot (GLit 2)) (GNot (GLit 2)))))
                                    (cons 'X (GNot (GSeq (GSeq (GSeq (GVar 'T) (GLit 3)) (GLit 0)) (GLit 2))))
                               ))
         (GAlt (GSeq (GSeq (GAlt (GLit 0) (GEps)) (GEps)) (GLit 1))
               (GSeq (GSeq (GNot (GLit 1)) (GLit 2)) (GLit 0)))
         (list))
)

;V<-ϵV
;T<-(V/'A')X/!'C'
;X<-!(T'D')
;('A'/ϵ)'B'/!'B'
(define peg-ill2-s
   (GPEG (make-immutable-hash (list (cons 'V   (GSeq (GEps) (GVar 'V)))
                                    (cons 'T (GAlt (GSeq (GAlt (GVar 'V) (GLit 0)) (GVar 'X))      
                                                   (GNot (GLit 2)) ))
                                    (cons 'X (GNot  (GSeq (GVar 'T) (GLit 3))) )
                               ))
         (GAlt (GSeq (GAlt (GLit 0) (GEps)) (GLit 1))
               (GNot (GLit 1)) )
         (list))
)