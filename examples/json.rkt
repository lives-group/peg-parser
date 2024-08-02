#lang peg-parser/debug/outputCTX

value <-- object / array / string / number / ^T / ^F / ^NLL;
object <-- ~AC (pair (COMMA pair)*)? ~FC;
pair   <-- ^string ~COLON ^value;
array  <-- ~AH (^value (COMMA ^value)*)? ~FH;

W    <-- [' ','\n','\t']*;
AC   <-- "{" ~W;
FC   <-- "}" ~W;
AH   <-- "[" ~W;
FH   <-- "]" ~W;
COMMA <-- "," ~W;
DOT   <-- '.' ~W;
COLON <-- ':' ~W;
T <-- -"true" ~W;
F <-- -"false" ~W;
NLL <-- -"null" ~W;
string <-- -('\"' (!'\"' .)* '\"') ~W;
number <-- -(['0'-'9']+ (DOT ['0'-'9']+)?) ~W;

start: ^value