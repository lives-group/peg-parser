(module tests mzscheme
  (require rackunit
           rackunit/text-ui)
  #;(require (lib "class.ss"))
  #;(require "../path-params.ss")
  (require "../syntax/parser.ss")
  #;(require "../semantics/semantic-object.ss")
  #;(require "../semantics/class-resolver.ss")
  #;(require "../semantics/standard-resolver.ss")

  (define (run-parser path)
       (parse-file "")
    )

  #;(define (parser-test str)
    (test-case 
     (substring str 0 (min 25 (string-length str)))
     (check-not-exn (lambda () (parse-string str)))))

  #;(define (parser-fail-test str)
    (test-case 
     (string-append "Fail: " (substring str 0 (min 25 (string-length str))))
      (check-exn (lambda x #t) (lambda () (parse-string str)))))

  #;(define parser-tests
    (test-suite "Tests for the parser"
                (parser-test "")
                (parser-test "class C {}")
                (parser-fail-test "class C")
                (parser-fail-test "class C {String x = \"foo;}")
                (parser-test "class C {String x = \"foo\";}")
                ))
  
  #;(run-tests parser-tests)

  )
