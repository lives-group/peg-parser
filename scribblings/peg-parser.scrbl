#lang scribble/manual

(@require[ scribble/example
          peg-parser
          @for-label[racket/base
                     racket/contract
                     racket/string
                     "../peg-ast.rkt"
                     "../peg-simple-recognizer.rkt"
                     ]])

@defmodule[peg-parser]

@title{PEG based parsers for Racket}
@author{Elton M. Cardoso, Rodrigo G. Ribeiro, Leonardo V. S. Reis}


@table-of-contents[]

@(begin
  (define ev (make-base-eval))
  (ev '(require rackcheck racket/list racket/stream peg-parser))
  (define-syntax-rule (ex body ...)
    (begin
     (random-seed 1337)
     (examples
      #:eval ev
      #:label #f
      body ...))))

@section{Overview}

This library provide a peg-based parser for use with the Racket language.
The library is provided as a Racket language with a syntax close to the
proposed PEG notation.

From a PEG specification a PEG interpreter, which reads its input from a port, is constructed.
The parser is a scanner-less bactracking parsing wose result is a specific tree which
allows to track the path of recognition.  


@section{Parser specification}

The PEG language of a set of rules, followes by a start parsing expression.
The syntax for each rule folows the format:

<ID> <-- Exp ;

Ehere <ID> is the name if an identifier and Exp is a PEG expression.
The Expressions sintax is as follows:

Basic and elementray expressions: 
@itemlist[    
  @item{'c' : Is the syntax of a single character. A character can also be specified by
      a backslash followed by a numeric code Ex.: '\65'  is the character A. }
  @item{['i'-'f'] or also ['a','b','c']: Is the syntax for the a range of characters.  }
  @item{. : Is the any char PEG expressions. }
  @item{"ab" : A double quote matchs a sequence of characters (string). }
  @item{epsilon : Is a reserved word that matchs the empty input.}
 ]

The parse expressions sytaxes are described below. 
@itemlist[
          
  @item{e1 / e2 : Is the syntax for prioritized alternative. It is rigth associative, so 'a'/'b'/'c'
                  is the same as 'a'/('b'/'c')}
  @item{e1 e2 : The concatenation operation the juxtaposition of two expressions.  }
  @item{e * : Is the syntax for the repetition operation}
  @item{e + : The same as the repetition, but requires at least one ocurrence of the pattern e}
  @item{e ? : The optional operation.}
  @item{! e : Is the syntax for the negation predicate}
  @item{A : Any name that is not a reserved word is considered to be a variable. Variables can be preceded
 by the symbol ^ which indicate that the variable itself is not relavant to the AST built by the parser.}
 ]

  Any expression can be preceded by the flatten operator (-) which will fat the tree to a string, or by
the silent operator (~) that will case no tree to be constructed by the recognizer (usefull to discard whites
for example). The negation predicate always buids an empty or a fail AST.
  
  The tool will type check a PEG expression, to certify that it can terminate on all possible inputs,
and type erros are reported whenver th tool detects them.

An example of a Grammar for simple expressions:

@codeblock|{
#lang peg-parser
Exp  <-- -W Term (-W ('+' / '-') -W Term)*;
Term  <--  ^Factor (-W ('*' / '/') -W ^Factor )*;
Factor <-- ^Number /-'(' -W Exp  -W -')';
Number <-- ~(['0'-'9'] +) ;
W <-- -[' ','\n','\t']*;

start: Exp
}|


@section{Parser Result}

The result of a parser is a PegTree which is the union of the structures PTFail, PTSym, PTVar, PTStr
and PTList.

 @defstruct*[PTFail ()]{ This tree node is produced when the parse fails on the input}

 @defstruct*[PTSym ([c char?])]{ The PTSym tree node is returned when the parser matches
 an input symbol.}

 @defstruct*[PTVar ([var  string?] [t PegTree?])]{ The PTVar tree node has two fields,
        one to the non-terminal name and one for the tree of the parsed body.}

 @defstruct*[PTStr ([s  string?])]{ The PTStr tree node has two fields,
        one to the non-terminal name and one for the tree of the parsed body.}
 @defstruct*[PTList ([xs  (listof? PegTree?)])]{  PTList is a list of parsetrees. }
       
The tree contains no node to represent the predicate not operation. Instead a not peg epxression produces
an empty PTList tree if it succeds on the input or a TFail otherwise. 
    
                                                  
@section{Using the Parser}

When we define a new parser, using the langauge, we can import it as a module in any other racket file.
The impoerted file will provide the fowolling functions run-parse and run-parse-from . 

@codeblock|{
#lang racket
(require "Expression.rkt")

}|


@defproc[(run-parse [s string?])
         PegTree?]{
 Run the parser defined in the language module on a given input string. Returns the PegTree structure
 representing the matched input.
 Ex.:
  @codeblock|{
 > (run-parse "3 + 2*5")
(PTVar
 "Exp"
 (PTList
  (list
   (PTVar "Term" (PTList (list (PTStr "3"))))
   (PTSym #\+)
   (PTVar "Term" (PTList (list (PTStr "2") (PTSym #\*) (PTStr "5")))))))
  }|
}

@defproc[(run-parse-from [nt string?] [s string?])
         PegTree?]{
 Run the parser defined in the language module on a given input string from the given non-terminal
 . Returns the PegTree structure representing the matched input.
 Ex.:
@codeblock|{
 > (run-parse-from "Factor""3 + 2*5")
(PTStr "3")
   }|
}

@section{Example of use: A simple calculator}

We already have the code the aritmetic expressions parser.

@codeblock|{
#lang peg-parser
Exp  <-- -W Term (-W ('+' / '-') -W Term)*;
Term  <--  ^Factor (-W ('*' / '/') -W ^Factor )*;
Factor <-- ^Number /-'(' -W Exp  -W -')';
Number <-- ~(['0'-'9'] +) ;
W <-- -[' ','\n','\t']*;

start: Exp
}|

The structure of the parser is such that the return is a variable Exp whose body
contains a non-empty list of sums (or subtractions) of terms (t1 + t2 + t3 ...).
We take the firts term and evaluate it. A continuation function (calc-exp) process the rest of the list. 

@codeblock|{
#lang racket
(require "Expression.rkt")

(define (calc e)
   (match e
    [(PTVar "Exp" (PTList (cons t xs))) (calc-exp (calc-term (PTList-xs (PTVar-t t))) xs)]
    [(PTVar "Exp" (PTList t))  (calc-term (PTList-xs (PTVar-t t)))]
    [(PTFail) "Oh no!"]
    )
  )
  
(define (calc-exp v p)
  (match p
    ['() v]
    [(cons (PTSym #\+) (cons t xs)) (calc-exp (+ v (calc-term (PTList-xs (PTVar-t t))))  xs)]
    [(cons (PTSym #\-) (cons t xs)) (calc-exp (- v (calc-term (PTList-xs (PTVar-t t))))  xs)]
    )
  )
}|

The terms themselfs follow the same structure, a list of mutiplication or division of factors
(f1 * f2 * f3 ...). So we use the same strategy as before. 

@codeblock|{

(define (calc-term p)
  (match p
    [(cons f xs) (calc-term1 (eval-factor f) xs)]
    [(list f) (eval-factor f)]
    )
  )

(define (calc-term1 v p)
  (match p
    ['() v]
    [(cons  (PTSym #\*) (cons f xs)) (calc-term1 (* v (eval-factor f)) xs)]
    [(cons  (PTSym #\/) (cons f xs)) (calc-term1 (/ v (eval-factor f)) xs)]
    )
  )
}|

The factors either will be a number (as a string) in which case we just onvert it to a number,
or the variale Exp, for a parenthised expression, in which case We just the calc function again !

@codeblock|{
(define (eval-factor p)
  (match p
    [(PTStr s) (string->number s)]
    [(PTVar "Exp" _)  (calc p)]
    )
  )

}|

Finnaly we just need to read a string from the input, parse it and run our interpreter on the resulting
tree. We define the run function to execute the test the code. 

@codeblock|{
(define (run)
   (let ([inp (read-line)] )
        (calc (run-parse inp)) 
   )
 )
}|