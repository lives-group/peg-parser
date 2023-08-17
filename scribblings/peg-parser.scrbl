#lang scribble/manual

(@require[ scribble/example
          peg-parser
          @for-label[racket/base
                     racket/contract
                     racket/string
                     "../peg-ast.rkt"
                     "../peg-recognizer.rkt"
                     ]])

@title{PEG based parsers for Racket}
@author{Elton M. Cardoso, Rodrigo G. Ribeiro, Leonardo V. S. Reis}

@defmodule[peg-parser]


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
The expression





@section{Parser Result}

The result of a parser is a ParseTree whose constructors are shonwed below:

@itemlist[
  @item{TFail : This tree node is produced when the parse fails on the input}
  @item{TEps : This tree node is produced when the parse succeds consuming no input}
  @item{(TSym [c : Char]) : The TSym tree node is returned when the parser matches
 an input symbol.}
  ;(TStr [s : (Listof Char)])
  @item{(TVar [var : String] [t : ParseTree]): The TVar tree node has two fields,
        one to the non-terminal name and one for the tree of the parsed body.}
  @item{(TCat [tl : ParseTree] [tr : ParseTree]) : The TCat tree node is the
  concatenation of the result of a concatenation parsing expression.}
  @item{(LChoice [tl : ParseTree]) and (RChoice [tr : ParseTree]): Represents the result of
 choice operation. Note that constructors respective to the sucessfull parsed branch of the expression.}
  @item{(TRep [xs : (Listof ParseTree)]): A repetition tree node is returned when a repetition expression
 is parsed. The list xs contains one tree node for each time the body of the repetion parsed sucessfully.}                                                                                                                                                                                                                  
]

The tree contains no node to represent the predicate not operation. Instead a not peg epxression produces
a TEps tree if it succeds on the input or a TFail otherwise. 




