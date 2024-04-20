![example workflow](https://github.com/lives-group/peg-parser/actions/workflows/build.yml/badge.svg)
![example workflow](https://github.com/lives-group/peg-parser/actions/workflows/tests.yml/badge.svg)

Welcome to the PEG-Parser for Racket repository!

# Overview

The PEG-Parser for Racket is a powerful tool designed to facilitate the creation of parsers for the Racket programming language. It is based on Parsing Expression Grammars (PEG) and incorporates type inference to ensure the termination of all parsing expressions.

## Features

  -  PEG-based Parsing: Leverage the simplicity and expressive power of Parsing Expression Grammars to define your parsers.
  -  Type Inference: Ensure the termination of parsing expressions by utilizing type inference techniques.
     No type annotations required !
  -  Generic Abstract Syntax Tree (AST): A simple, generic AST structure is generated as the result of parsing, making it easy to manipulate and work with the parsed data.
 

# Getting Started

To start using the PEG Parser for Racket, follow these simple steps:

## Installation

 Clone this repository to your local machine.

```bash
git clone https://github.com/lives-group/peg-parser.git
```

Additionally you can also install PEG-Parser from the Racket package repository:  

```bash
raco pkg install peg-parser
```

## Usage

Here is an example of a simple parser for logic expressions involving only
the constants True "T" and False "F".

```Racket
#lang peg-parser

Consts <-- -'T' ~W
         / -'F' ~W;
OR  <-- '\|'~W; 
AND <-- '^' ~W;
AP  <-- '(' ~W;
FP  <-- ')' ~W;

Formula  <-- Ands ( ~^OR Ands)*;
Ands <-- ^Not ( ~^AND ^Not)*;
Not <-- (~'~' ~W) (^Consts / ~AP Formula ~FP ) / ^Consts;
W <-- [' ', '\n', '\t']*;

start: Formula
```


 Consult the documentation and examples provided to learn how to define and 
generate parsers using the tool.


# Documentation

Please read the documentation [here](https://docs.racket-lang.org/peg-parser/index.html) for more
specific details on the usage of library.

# License

This project is licensed under the MIT License.
