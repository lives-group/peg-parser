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

This library provides a PEG-based parser for use with the Racket language.
It is implemented in Racket and closely follows the proposed PEG notation.

With this library, you can construct a PEG interpreter from a PEG specification.
This interpreter reads input from a port, making it a versatile tool for parsing.
The parser utilizes a scanner-less, backtracking approach, resulting in a specific parse tree.


@section{Parser specification}

he PEG language consists of a set of rules, followed by a
starting parsing expression. The syntax for each rule follows the format:

<ID> <-- Exp ;

Here, <ID> represents the name of an identifier, and Exp is a PEG expression.
Followed by the rules is the specification of a start PEG expression, as follows:

start: Exp

The syntax for PEG expressions is as follows:

@itemlist[    
  @item{'c' :Represents a single character. A character can also be specified by
             a backslash followed by a numeric code (e.g., '\65' is the character 'A') }
  @item{['i'-'f'] or also ['a','b','c']: Indicates a range of characters. The first case
        denotes all character from 'i' to 'f'. The second case any of the character 'a',
        'v' or 'c'.}
  @item{. : Represents any character in a PEG expression.. }
  @item{"ab" : A double-quoted expression represents a sequence of characters (a string). }
  @item{epsilon : A reserved word that represents the empty input. This expression will
                  always succeds.}
 ]

The parse expressions sytaxes are described below. 
@itemlist[
          
  @item{e1 / e2 :  Denotes prioritized alternatives. It is right-associative,
                   so 'a'/'b'/'c' is the same as 'a'/('b'/'c')}
  @item{e1 e2 : The concatenation operation is represented by the juxtaposition of the expressions.  }
  @item{e * : Denotes the repetition operation.}
  @item{e + : Similar to repetition but requires at least one occurrence of the pattern e.}
  @item{e ? : Denotes an optional operation.}
  @item{! e : Denotes an optional operation.}
  @item{A : Any name that is not a reserved word is considered a variable. Variables can be preceded
            by the symbol ^, indicating that the variable itself is not relevant to the AST constructed
            by the parser.}
 ]

  Any expression can be preceded by the flatten operator (-), which flattens the tree to a string, or by the
  silent operator (~), which prevents the construction of a tree by the recognizer (useful, for example,
  to discard whitespace). The negation predicate always results in an empty or fail AST.

  The tool performs type checking on PEG expressions to ensure termination on all possible inputs.
  Type errors are reported whenever the tool detects them.

An example of a Grammar for simple expressions:

@codeblock|{
#lang peg-parser
Exp    <-- ~W Term (~W ('+' / '-') ~W Term)*;
Term   <--  ^Factor (~W ('*' / '/') ~W ^Factor )*;
Factor <-- ^Number /~'(' ~W Exp  ~W ~')';
Number <-- -(['0'-'9'] +) ;
W      <-- ~[' ','\n','\t']*;

start: Exp
}|



@section{Parser Result}

The result of a parser is a PegTree, which is a union of the following structures:
PTFail, PTSym, PTVar, PTStr, and PTList.

@defstruct*[PTFail ()]{ This tree node is generated when the parsing process fails on the input.}

@defstruct*[PTSym ([c char?])]{ The PTSym tree node is returned when the parser
                                successfully matches an input symbol.}

@defstruct*[PTVar ([var string?] [t PegTree?])]{ The PTVar tree node has two fields:
                     one for the non-terminal name and another for the tree representing the parsed body.}

@defstruct*[PTStr ([s string?])]{ The PTStr tree node also contains two fields:
     one for the non-terminal name and another for the tree of the parsed body.}

@defstruct*[PTList ([xs (listof? PegTree?)])]{PTList represents a list of parse trees.}

It's important to note that there is no specific node to represent the "not" predicate operation in the tree
structure. Instead, when a "not" PEG expression succeeds on the input, it results in an empty `PTList` tree,
or a `PTFail` tree if it fails.

This structure simplifies the representation of parser results and provides a clear understanding of the
various nodes within a `PegTree`. The reader can now easily identify the purpose and structure of each
result type.

                                                  
@section{Using the Parser}

When we define a new parser, using the language peg-parser, we can import it as a module
in any other racket file. The imported file will provide the fowolling
functions run-parse and run-parse-from . 

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

@section{Example of Use: A simple Calculator}

Below is the code for an arithmetic expression parser:

@codeblock|{
#lang peg-parser
Exp    <-- ~W Term (~W ('+' / '-') ~W Term)*;
Term   <--  ^Factor (~W ('*' / '/') ~W ^Factor )*;
Factor <-- ^Number /~'(' ~W Exp  ~W ~')';
Number <-- -(['0'-'9'] +) ;
W      <-- ~[' ','\n','\t']*;

start: Exp
}|

The structure of the parser is designed so that it returns a variable named `Exp`.
The body of `Exp` contains a non-empty list of sums (or subtractions) of terms (t1 + t2 + t3 ...).
The first term is evaluated, and a continuation function (`calc-exp`) processes the rest of the list.
 

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

The terms themselves follow the same structure, a list of multiplication
or division of factors (f1 * f2 * f3 ...). So, we use the same strategy as before. 

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


The factors will either be a number (as a string), in which case we convert it to a number,
or the variable Exp, for a parenthesized expression, in which case we just call the calc function again!

@codeblock|{
(define (eval-factor p)
  (match p
    [(PTStr s) (string->number s)]
    [(PTVar "Exp" _)  (calc p)]
    )
  )

}|

Finally, we need to read a string from the input, parse it, and run our interpreter on the
resulting tree. We define the run function to test the code.  

@codeblock|{
(define (run)
   (let ([inp (read-line)] )
        (calc (run-parse inp)) 
   )
 )
}|