#lang peg-parser


resw <-- tyPrimitive; 

type       <--  (^tyPrimitive / jName) tyPosArr;
tyPrimitive  <-- -("int" / "byte" /"char" / "short" / "long"
                 / "char"
                 / "float" / "double"
                 / "boolean") ~W;

tyPosArr    <-- (-(AH FH))*;

jID <-- -(jLetter (jLetter / jDigit)*) ~W;
jLetter <-- ['A'-'Z'] / ['a'-'z'] / ['_', '$'];
jDigit  <-- ['0'-'9'];

jName <-- ^jID (~DOT ^jID)*;

importList <--  importDecl*;
importDecl <-- ~IMPORT  ^jID (~DOT ^jID)*  (-(~DOT '*'))? ~SEMI;
pkgDecl <-- ~PACKAGE jName ~SEMI;

compilationUnit <--  pkgDecl ? importList;

modfier <-- -("public"    / "private" / "protected" / "static" /
              "abstract"  / "final"   / "strictfp"  / "synchronized" /
              "transient" / "volatile");

extends    <-- ~EXTENDS jName;
implements <-- ~IMPLEMENTS (jName)*;
classDec   <-- modfier* CLASS extends? implements? body;
body       <-- ~AC ~FC ;

fieldDec <--  (^modfier)* ^type ^varDec (COMMA ^varDec)* SEMI;
varDec <-- var EQ varInit;
varInit <-- (arrayInit / exp);
var <-- ^jID (-(AH FH))*;
arrayInit <-- ~AC varInitList? COMMA? ~FC;
varInitList <-- varInit (COMMA varInit)*;  

assigment <-- lhs assigmentOp assigmentExp;

assigmentOp <-- "";

exp <-- '0' ~W;



value  <-- object / array / string / number / ^T / ^F / ^NLL;
object <-- ~AC (pair (COMMA pair)*)? ~FC;
pair   <-- ^string ~COLON ^value;
array  <-- ~AH (^value (COMMA ^value)*)? ~FH;

IMPORT <-- "import" ~W;
PACKAGE <-- "package" ~W;
PACKAGE <-- "package" ~W;
EXTENDS <-- "extends" ~W;
IMPLEMENTS <-- "implements" ~W;
CLASS <-- "class" ~W;

W    <-- [' ','\n','\t']*;
WPlus<-- [' ','\n','\t']+;
EQ   <-- "=" ~W;
AC   <-- "{" ~W;
FC   <-- "}" ~W;
AH   <-- "[" ~W;
FH   <-- "]" ~W;
COMMA <-- "," ~W;
DOT   <-- '.' ~W;
COLON <-- ':' ~W;
SEMI  <-- ';' ~W;
T <-- -"true" ~W;
F <-- -"false" ~W;
NLL <-- -"null" ~W;
string <-- -('\"' (!'\"' .)* '\"') ~W;
number <-- -(['0'-'9']+ (DOT ['0'-'9']+)?) ~W;

start: importList