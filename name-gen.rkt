#lang typed/racket

(provide new-name
         start-gen)

(define count : Natural 0)
(define prefix : String "" )
(define forbiden : (Setof String) (list->set null) )


(define (mkname) : String
     (string-append prefix (number->string count)))

(define (new-name) : String
   (cond
     [(set-member? forbiden (mkname))  (begin (set! count (+ count 1)) (new-name))]
     [else (let ([ s : String (mkname)]) (begin (set! count (+ count 1)) s)) ])
  )

(define (start-gen [l : (Listof String)] [prfx : String] [n : Natural])
    (set! count n)
    (set! prefix prfx)
    (set! forbiden (list->set l))
  )