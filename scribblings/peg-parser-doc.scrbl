#lang scribble/manual

@require[ scribble/example
          peg-parser
          @for-label[racket/base
                     racket/contract
                     racket/string
                     "../peg-ast.rkt"
                     ;"../peg-simple-recognizer.rkt"
                     ]]

@title{PEG based parsers for Racket}

@author{Elton M. Cardoso, Rodrigo G. Ribeiro, Leonardo V. S. Reis}


@table-of-contents[]

@(begin
  (define ev (make-base-eval))
  (ev '(require peg-parser rackcheck racket/list racket/stream ))
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

This library can construct a PEG interpreter from a PEG specification.
This interpreter reads input from a port, making it a versatile tool for parsing.
The parser utilizes a scanner-less, backtracking approach, resulting in a specific parse tree.


@section{Parser specification}

The PEG language consists of a set of rules followed by a
starting parsing expression. The syntax for each rule follows the format:

<ID> <-- Exp ;

Here, <ID> represents the name of an identifier, and Exp is a PEG expression.
Followed by the rules is the specification of a start PEG expression, as follows:

start: Exp

The syntax for the more simple PEG expressions is as follows:

@itemlist[
  @item{'c' : Represents a single character. A character can also be specified by a backslash followed by a numeric code (e.g., '\65' is the character 'A') }
  @item{['i'-'f'] or also ['a','b','c']: Indicates a range of characters. The first case denotes all characters from 'i' to 'f'. In the second case, any of the characters 'a, 'v', or 'c.'}
  @item{. : Represents any character in a PEG expression.}
  @item{"ab" : A double-quoted expression represents a sequence of characters (a string). }
  @item{epsilon : A reserved word that represents the empty input. This expression will always succeed without consuming any input.}

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

An example of a Grammar for simple expressions:

@verbatim{
#lang peg-parser
Exp    <-- ~W Term (~W ('+' / '-') ~W Term)*;
Term   <--  ^Factor (~W ('*' / '/') ~W ^Factor )*;
Factor <-- ^Number /~'(' ~W Exp  ~W ~')';
Number <-- -(['0'-'9'] +) ;
W      <-- ~[' ','\n','\t']*;

start: Exp
}

Our implementation of the type inference verifies all specifications. No type annotations
are allowed in the code because our system can infer all types if the PEG is well-formed.
Any errors found during this phase will be reported. For example, the parser generator will not accept the code below since the sub-expression ('a' / !'b') might succeed without consuming any input and causing the repetition operation to loop.

@verbatim{
#lang peg-parser
S <-- ('a' / !'b')* "cc";
start: S
}

@section{Parser Result}

The result of a parser is a PegTree, which is a union of the following structures:
PTFail, PTSym, PTVar, PTStr, and PTList.

@defstruct*[#:link-target? #f PTFail ()]{ This tree node is generated when the parsing process fails on the input.}

@defstruct*[#:link-target? #f PTSym ([c char?])]{ The PTSym tree node is returned when the parser
                                matches an input symbol successfully.}

@defstruct*[#:link-target? #f PTVar ([var string?] [t PegTree?])]{ The PTVar tree node has two fields:
                     One is for the non-terminal name, and the other is for the parsed body tree.}

@defstruct*[#:link-target? #f PTStr ([s string?])]{ The PTStr tree node also contains two fields:
     One for the non-terminal name and another for the tree of the parsed body.}

@defstruct*[#:link-target? #f PTList ([xs (listof? PegTree?)])]{PTList represents a list of parse trees.}

It is important to note that no specific node represents the "not" predicate operation in the tree
structure. Instead, when a "not" PEG expression succeeds on the input, it results in an empty `PTList` tree
or a `PTFail` tree if it fails.

This structure simplifies the representation of parser results and provides a clear understanding of the
various nodes within a `PegTree`. The reader can now quickly identify the purpose and structure of each
result type.


@section{Using the Parser}

When we define a new parser using the language peg-parser, we can import it as a module
in any other racket file. The module generated by the specification of the parser
will export its parser definitions as qualified names using the name of the
file as their prefix. This way, the user can require more than one parser
without causing name conflicts.

@codeblock|{
#lang racket
(require "Expression.rkt")

}|


@defproc[#:link-target? #f (parse [s string?])
         PegTree?]{
 Run the parser defined in the language module on a given input string. Returns the PegTree structure
 representing the matched input.
 Ex.:
  @codeblock|{
 > (Expression:parse "3 + 2*5")
(PTVar
 "Exp"
 (PTList
  (list
   (PTVar "Term" (PTList (list (PTStr "3"))))
   (PTSym #\+)
   (PTVar "Term" (PTList (list (PTStr "2") (PTSym #\*) (PTStr "5")))))))
  }|
}

@defproc[#:link-target? #f (parse-from-nt [nt string?] [s string?])
         PegTree?]{
 Run the parser defined in the language module on a given input string from the given non-terminal
 . Returns the PegTree structure representing the matched input.
 Ex.:
@codeblock|{
 > (Expression:parse-from "Factor""3 + 2*5")
(PTStr "3")
   }|
}

@defproc[#:link-target? #f (parse-file [fame string?])
         PegTree?]{
 Run the parser defined in the language module on a given file.
 The parameter name is the path to the file containing the input to be read.
 Ex.:
@codeblock|{
 > (Expression:parse-from "Factor""3 + 2*5")
(PTStr "3")
   }|
}

@defproc[#:link-target? #f (parse-file-from-nt [ntname string?] [fname string?])
         PegTree?]{
 Run the parser defined in the language module on a given file, using the given ntname as the starting
 expression.
 Returns the PegTree structure representing the matched input.
 Ex.:
@codeblock|{
 > (Expression:parse-from "Factor""3 + 2*5")
(PTStr "3")
   }|
}
@section{Example of Use: A simple Calculator}

Below is the code for an arithmetic expression parser:

@verbatim{
#lang peg-parser
Exp    <-- ~W Term (~W ('+' / '-') ~W Term)*;
Term   <--  ^Factor (~W ('*' / '/') ~W ^Factor )*;
Factor <-- ^Number /~'(' ~W Exp  ~W ~')';
Number <-- -(['0'-'9'] +) ;
W      <-- ~[' ','\n','\t']*;

start: Exp
}

The parser returns a variable named `Exp` whose body contains a non-empty list of sums (or subtractions) of terms (t1 + t2 + t3 ...).
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

The terms follow the same structure, a list of multiplication
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
or the variable `Exp`, for a parenthesized expression, in which case we just call the calc function again!

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
        (calc (Expression:parse inp))
   )
 )
}|
