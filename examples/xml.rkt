#lang peg-parser

%%Main

XML <-- value ;
value <-- head?  content*;

content <-- multi / line / cdata / html / comment;

W <-- [' ', '\t','\n','\r' ]*;
leftArrow  <-- -"<" ~W;
rightArrow <-- -">" ~W;
quotation  <--  '\"' ~W;
equal      <-- -"=" ~W;
backslash  <-- -"/" ~W;
beginCdata <-- -"<![CDATA[" ~W;
endCdata   <-- -"]]>" ~W;
beginXml   <-- -"<?xml" ~W;
endXml     <-- -"?>" ~W;
beginComment <-- -"<!--" ~W;
endComment <-- -"-->" ~W;

StartTag <-- -(^leftArrow  ^TagName) keyword* -^rightArrow ;
EndTag   <-- -(^leftArrow  ^backslash  ^TagName ^rightArrow) ;
TagName  <-- -((['a'-'z'] / ['A'-'Z']/ ['_',':','?'])(['a'-'z'] / ['A'-'Z']/['0'-'9']/ ['_',':','?'])*) ~W ;
TagValue <-- (!'<'.)+ ;

%% value
head <-- beginXml keyword+ endXml ;
multi <-- StartTag content* EndTag ;
line <-- leftArrow  TagName keyword+ backslash rightArrow;
cdata <-- beginCdata text endCdata;
text  <-- -((!"]]>" .)*) ;
html <-- -((!'<'.)+);

keyword <-- ^TagName ^equal ^string ;
string <-- ~quotation -(char *) ~quotation;

char <-- unescaped /
     '\\' ([ '\"','\\','/','b', 'f', 'n', 'r','t','u'] / -(HEX HEX HEX HEX)) ;

unescaped <-- !(['\000'-'\031'] / ['\"','\\']).;
HEX <-- ['0'-'9']/['a'-'f'] / ['A'-'F'];

comment <-- beginComment -((!"-->").)* endComment;

start: XML !.

  