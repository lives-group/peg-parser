#lang racket

(require parser-tools/yacc
         parser-tools/lex
         "XMLlexer.rkt")

;; converting a string token into a tree of
;; characters concatenation

(struct XML (prolog vlaues) #:transparent)
(struct HEADER (content) #:transparent)
(struct Attr (att value) #:transparent)
(struct Element (name attr body) #:transparent)


(define core-parser
  (parser
   (start doc)
   (end EOF)
   (tokens value-tokens op-tokens)
   (src-pos)
   (error
    (lambda (a b c d e)
      (begin (printf "parse error:\na = ~a\nb = ~a\nc = ~a\nd = ~a\ne = ~a\n" a b c d e)
             (void))))
   (grammar
    (doc    [(prolog values)              (XML $1 $2)]
            [(values)                     (XML '() $1)])
    (prolog [(BEGIN_XML attrList END_XML) (HEADER $2)] )
    (values [(element)                      (list $1)]
            [(element values)               (cons $1 $2)])
    (element [(TAG_ST  attrList  content) (Element $1 $2 $3)])
    (content [(TAG_END) (list)]
             [(TAG_CLOSE content1 TAG_END) $2])
    (m_name [(NAME) $1]
            [() ""] )
    (content1 [(content1 field) (append $1 (list $2))]
              [() (list)])
    (field [(text) $1]
           [(element) $1])
    (text [(text CHDATA ) (append $1 (list $2))]
          [()  (list)])
    #;(line  [(TAG_OPEN NAME attrList BACKSLASH TAG_CLOSE)                       (ENTRY (list $2 $3))])
    #;(multi [(TAG_OPEN NAME TAG_CLOSE values TAG_OPEN BACKSLASH NAME TAG_CLOSE) (ENTRY (cons $2 $4))])
    #;(cdata [(CDATA_OPEN TEXT CDATA_CLOSE) (CDATA $2)])

    (attrList [(attrList attr) (append $1 (list $2))]
              [() (list)])
    (attr [(ATTR_INIT ATTR) (Attr $1 $2)] ))
  )) 

(define (parse ip)
  (port-count-lines! ip)  
  (core-parser (lambda () (next-token ip))))

(provide parse)
