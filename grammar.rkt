#lang racket

(require parser-tools/yacc
         parser-tools/lex
         "lexer.rkt"
         "peg-ast.rkt")

;; converting a string token into a tree of
;; characters concatenation

(define (string->tree p s)
  (match s
    ['() (Eps) ]
    [(cons c '()) (Sym (src p) c)]
    [(cons c s1) (Cat (src p)
                      (Sym (src p) c)
                      (string->tree (position (position-line p) (+ 1 (position-col p) ) 1) s1))]))


(define (src pos)
     (SrcLoc (position-line pos) (position-col pos))
  )

(define (chr->sym pos s)
     (Sym (src pos) (car (string->list s)))
  )

(define (foldl1 f l)
   (match l
     [(list x) x]
     [(cons x xs) (f x (foldl1 f xs))])
  )

(define (mkAltList pos s e)
   (foldl1 (lambda (c p) (Alt pos c p))
           (map (lambda (c) (Sym pos c) ) (chrange->list (car (string->list s))
                                                         (car (string->list e)))))
  )

(define core-parser
  (parser
   (start peg)
   (end EOF)
   (tokens value-tokens op-tokens)
   (src-pos)
   (error
    (lambda (a b c d e)
      (begin (printf "parse error:\na = ~a\nb = ~a\nc = ~a\nd = ~a\ne = ~a\n" a b c d e)
             (void))))
   (grammar
    (peg [(rules START expr) (PEG (mk-vars $1) $3)])
    (rules [() '()]
           [(rule rules) (cons $1 $2)])
    (rule [(VAR ARROW expr SEMI) (cons $1 $3)])
    (expr [(cat OR expr) (Alt (src $2-start-pos) $1 $3)]
          [(cat) $1])
    (cat [(cat term) (Cat (src $1-start-pos) $1 $2)]
         [(term) $1])
    (term [(prefixop term) ($1 $2)]
          [(factor)   $1])
    (prefixop [(NOT)  (lambda (e) (Not (src $1-start-pos) e))]
              [(AND)  (lambda (e) (Not (src $1-start-pos) (Not (src $1-start-pos) e)))]
              [(DASH) (lambda (e) (Annot (src $1-start-pos) 'Flat e))]
              [(TIL)  (lambda (e) (Annot (src $1-start-pos) 'Silent e))]
              [(AT)   (lambda (e) (Annot (src $1-start-pos) 'Capture e))])
    (factor [(factor postfix) ($2 $1)]
            [(atom) $1])
    (postfix [(STAR) (lambda (e) (Rep (src $1-start-pos) e))]
             [(PLUS) (lambda (e) (Cat (src $1-start-pos) e (Rep (src $1-start-pos) e)))]
             [(OPTION) (lambda (e) (Alt (src $1-start-pos) e (Eps (src $1-start-pos)) ))])
    (char-list [(CHAR) (chr->sym $1-start-pos  $1)]
               [(char-rng) $1]
               [(char-rng COMMA char-list)  (Alt (src $2-start-pos) $1 $3)]
               [(CHAR COMMA char-list) (Alt (src $2-start-pos) (chr->sym $1-start-pos $1) $3)])
    (char-rng [(CHAR DASH CHAR) (mkAltList  (src $1-start-pos) $1 $3)]) 
    (atom [(EPSILON) (Eps (src $1-start-pos)) ]
          [(CHAR)    (chr->sym $1-start-pos $1)]
          [(STRING)  (string->tree $1-start-pos (string->list $1))]
          [(LBRACK char-list RBRACK) $2]
          [(ANY)     (Any (src $1-start-pos) )]
          [(UP VAR)  (Var (src $1-start-pos) #t $2)]
          [(VAR)     (Var (src $1-start-pos) #f $1)]
          [(LPAREN expr RPAREN) $2])
    )))

(define (parse ip)
  (port-count-lines! ip)  
  (core-parser (lambda () (next-token ip))))

(provide parse)
