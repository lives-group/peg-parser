#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens
  (TAG_ST TAG_END TEXT ATTR ATTR_INIT NAME CHDATA))

(define-empty-tokens op-tokens
  (EOF
   BEGIN_XML
   END_XML
   CDATA_OPEN
   CDATA_CLOSE
   TAG_CLOSE
   COLON
   EQUAL
   BACKSLASH
   ))

(define-lex-abbrev esc (:or (:: (:: "&#" (:+ (char-range #\0 #\9))) ";")
                            (:: ( :: "&#x" (:+ (:or numeric (char-range #\a #\f) (char-range #\A #\F)) ) ";"))))
(define-lex-abbrev chr-name-start (:or (char-range #\A #\Z) (char-range #\a #\z) (char-range #\200 #\377) #\_))
(define-lex-abbrev data-chr (char-complement (:or #\< #\n #\&) ) )
(define-lex-abbrev name  (:: chr-name-start (:* (:or chr-name-start numeric #\_ #\. #\- #\:))))

(define (strip-name-token str)
     (string-trim (substring str 1 (string-length str) ))
  )

(define next-token
  (lexer-src-pos
   [(eof) (token-EOF)]
   [(:+ whitespace #\newline) (return-without-pos (next-token input-port))]
   ["<![CDATA[" (token-CDATA_OPEN)]
   ["]]>" (token-CDATA_CLOSE)] 
   ["<?xml" (token-BEGIN_XML)]
   ["?>" (token-END_XML)]
   [(:: "<" (:* whitespace #\newline) name) (token-TAG_ST (strip-name-token lexeme))]
   [(:: "<" (:* whitespace #\newline) "/" (:* whitespace #\newline) (:? name) (:* whitespace #\newline) ">")  (token-TAG_END (strip-name-token lexeme))]
   [">" (token-TAG_CLOSE)] 
   [(:: name  (:* whitespace) "=" ) (token-ATTR_INIT lexeme)]

   #;[name (token-NAME lexeme)]
   [(:: #\" (complement (:: any-string #\" any-string)) #\")
    (token-ATTR (let* ([s lexeme]
                         [n (string-length s)])
                    (substring s 1 (- n 1))))]
   
   [(:: #\' (complement (:: any-string #\' any-string)) #\')
    (token-ATTR (let* ([s lexeme]
                         [n (string-length s)])
                    (substring s 1 (- n 1))))]
   [(:: "<!--" (:* (complement "-->")) "-->")
    (return-without-pos (next-token input-port))]
   [(:: data-chr )   (token-CHDATA lexeme)]
))

(provide value-tokens op-tokens next-token)
