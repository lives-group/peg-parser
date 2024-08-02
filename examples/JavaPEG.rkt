#lang peg-parser
%% ===========================================================================
%%
%%  Parsing Expression Grammar of Java for Mouse 2.3.
%%  Based on Java Language Specification, Java SE 18 Edition, 2022-02-23.
%%
%%---------------------------------------------------------------------------
%%
%%  Copyright (C) 2021, 2022
%%  by Roman R Redziejowski(www.romanredz.se).
%%
%%  The author gives unlimited permission to copy and distribute
%%  this file, with or without modifications, as long as this notice
%%  is preserved, and any changes are properly documented.
%%
%%  This file is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
%%

%%=========================================================================
%%  Compilation
%%=========================================================================

Compilation <-- W CompilationUnit SUB? EOT;

%%=========================================================================
%%  JLS 3  Lexical Structure
%%=========================================================================
%%-------------------------------------------------------------------------
%%  JLS 3.1-3  Unicode
%%-------------------------------------------------------------------------
%%  The Unicode escapes in Java source are converted
%%  to Java characters by a preprocessor prior to parsing.
%%  This is not emulated here; the Unicode escapes are only allowed
%%  in string and character literals. They are treated as error in other
%%  structures (except comments). The warning in JLS 3.10.5 against using
%%  Unicode escapes for line terminators and quotes in string and character
%%  literals does not apply here.
%%-------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%%  JLS 3.5  Input Elements and Tokens
%%-------------------------------------------------------------------------

SUB <-- '\26' ;
EOT <-- !. ;

%%-------------------------------------------------------------------------
%%  JLS 3.6-7  Spacing
%%-------------------------------------------------------------------------

W <-- ~(( [ ' ', '\t','\r','\n','\12']+    %% WhiteSpace
         / "/*" (!"*/".)* "*/"               %% TraditionalComment
         / "//" (!['\r','\n'].)* ['\r','\n']        %% EndOfLineComment
        )*) ;

%%-------------------------------------------------------------------------
%%  JLS 3.8  Identifiers
%%-------------------------------------------------------------------------

Word <-- -(^Letter (^LetterOrDigit)*) ;

Identifier  <-- !Reserved ^Word W ;

TypeIdentifier  <-- !(Reserved / "var" / "yield" / "record" / "permits" / "sealed" ) Word W ;

Letter <-- ['a'-'z'] / ['A'-'Z'] / '$';

LetterOrDigit <-- ['a'-'z'] / ['A'-'Z'] / ['0'-'9'] / '$' ;

%% These are traditional definitions of letters and digits.
%% JLS defines letters and digits as Unicode characters recognized
%% as such by special Java procedures, which is difficult
%% to express in terms of Parsing Expressions.

%%-------------------------------------------------------------------------
%%  Character strings that can not be identifiers.
%%-------------------------------------------------------------------------

Reserved <-- -"abstract"
    / -"assert"
    / -"boolean"
    / -"break"
    / -"byte"
    / -"case"
    / -"catch"
    / -"char"
    / -"class"
    / -"const"
    / -"continue"
    / -"default"
    / -"double"
    / -"do"
    / -"else"
    / -"enum"
    / -"extends"
    / -"false"
    / -"finally"
    / -"final"
    / -"float"
    / -"for"
    / -"goto"
    / -"if"
    / -"implements"
    / -"import"
    / -"interface"
    / -"int"
    / -"instanceof"
    / -"long"
    / -"native"
    / -"new"
    / -"null"
    / -"package"
    / -"private"
    / -"protected"
    / -"public"
    / -"return"
    / -"short"
    / -"static"
    / -"strictfp"
    / -"super"
    / -"switch"
    / -"synchronized"
    / -"this"
    / -"throws"
    / -"throw"
    / -"transient"
    / -"true"
    / -"try"
    / -"void"
    / -"volatile"
    / -"while"
    / -"_"
    ;

%%-------------------------------------------------------------------------
%%  JLS 3.9  Keywords
%%-------------------------------------------------------------------------
%%  Reserved Keywords
%%  According to JLS, "true", "false", and "null"
%%  are technically not keywords - but still must not appear as identifiers.
%%  Keywords "const" and "goto" are not used; JLS explains the reason.
%%-------------------------------------------------------------------------

ABSTRACT     <-- -"abstract" W;
ASSERT       <-- -"assert"   W ;
BOOLEAN      <-- -"boolean"  W ;
BREAK        <-- -"break"    W ;
BYTE         <--  -"byte"    W ;
CASE         <--  -"case"    W ;
CATCH        <--  -"catch"   W ;
CHAR         <--  -"char"    W ;
CLASS        <--  -"class"        W ;
CONTINUE     <--  -"continue"     W ;
DEFAULT      <--  -"default"      W ;
DOUBLE       <--  -"double"       W ;
DO           <--  -"do"           W ;
ELSE         <--  -"else"         W ;
ENUM         <--  -"enum"         W ;
EXTENDS      <--  -"extends"      W ;
FALSE        <--  -"false"        W ;
FINALLY      <--  -"finally"      W ;
FINAL        <--  -"final"        W ;
FLOAT        <--  -"float"        W ;
FOR          <--  -"for"          W ;
IF           <--  -"if"           W ;
IMPLEMENTS   <--  -"implements"   W ;
IMPORT       <--  -"import"       W ;
INTERFACE    <--  -"interface"    W ;
INT          <--  -"int"          W ;
INSTANCEOF   <--  -"instanceof"   W ;
LONG         <--  -"long"         W ;
NATIVE       <--  -"native"       W ;
NEW          <--  -"new"          W ;
NULL         <--  -"null"         W ;
PACKAGE      <--  -"package"      W ;
PRIVATE      <--  -"private"      W ;
PROTECTED    <--  -"protected"    W ;
PUBLIC       <--  -"public"       W ;
RETURN       <--  -"return"       W ;
SHORT        <--  -"short"        W ;
STATIC       <--  -"static"       W ;
STRICTFP     <--  -"strictfp"     W ;
SUPER        <--  -"super"        W ;
SWITCH       <--  -"switch"       W ;
SYNCHRONIZED <--  -"synchronized" W ;
THIS         <--  -"this"         W ;
THROWS       <--  -"throws"       W ;
THROW        <--  -"throw"        W ;
TRANSIENT    <--  -"transient"    W ;
TRUE         <--  -"true"         W ;
TRY          <--  -"try"          W ;
VOID         <--  -"void"         W ;
VOLATILE     <--  -"volatile"     W ;
WHILE        <--  -"while"        W ;

%%-------------------------------------------------------------------------
%%  Contextual Keywords
%%-------------------------------------------------------------------------

EXPORTS     <--  -"exports"      W ;
MODULE      <--  -"module"       W ;
NONSEALED   <--  -"non-sealed"   W ;
OPEN        <--  -"open"         W ;
OPENS       <--  -"opens"        W ;
PERMITS     <--  -"permits"      W ;
PROVIDES    <--  -"provides"     W ;
RECORD      <--  -"record"       W ;
REQUIRES    <--  -"requires"     W ;
SEALED      <--  -"sealed"       W ;
TO          <--  -"to"           W ;
TRANSITIVE  <--  -"transitive"   W ;
USES        <--  -"uses"         W ;
VAR         <--  -"var"          W ;
WITH        <--  -"with"         W ;
YIELD       <--  -"yield"        W ;

%%-------------------------------------------------------------------------
%%  JLS 3.10  Literals
%%-------------------------------------------------------------------------

Literal <-- FloatLiteral
    / IntegerLiteral          %% May be a prefix of FloatLiteral
    / BooleanLiteral
    / CharLiteral
    / TextBlock
    / StringLiteral           %% May be a prefix of TextBlock
%%    / NullLiteral
    ;

%%-------------------------------------------------------------------------
%%  JLS 3.10.1  Integer Literals
%%-------------------------------------------------------------------------

IntegerLiteral <-- ( HexNumeral
                    / BinaryNumeral
                    / OctalNumeral            %% May be a prefix of HexNumeral or BinaryNumeral
                    / DecimalNumeral          %% May be a prefix of OctalNumeral
                     ) ['l','L']? W ;

DecimalNumeral <-- "0" / -(['1'-'9'](.*['0'-'9'])*) ;

HexNumeral  <-- ("0x" / "0X") ^HexDigits ;

OctalNumeral <-- "0" -((.*['0'-'7'])+) ;

BinaryNumeral <-- ("0b" / "0B") -(['0','1'](.*['0','1'])*) ;

%%-------------------------------------------------------------------------
%%  JLS 3.10.2  Floatng-point Literals
%%-------------------------------------------------------------------------

FloatLiteral <-- ( HexadecimalFloatingPointLiteral
                  / DecimalFloatingPointLiteral   %% May be a prefix of above
                 ) W
    ;

DecimalFloatingPointLiteral <-- ^Digits '.' ^Digits?  ^Exponent? ['f','F','d','D']?
                              / "." ^Digits ^Exponent? ['f','F','d','D']?
                              / ^Digits ^Exponent ['f','F','d','D']?
                              / ^Digits ^Exponent? ['f','F','d','D']
                              ;
Exponent <-- ['e','E'] ['+','-']? ^Digits ;

HexadecimalFloatingPointLiteral
    <-- HexSignificand BinaryExponent ['f','F','d','D']? ;

HexSignificand <-- ("0x" / "0X") HexDigits? "." ^HexDigits
                   / HexNumeral "."?                        %% May be a prefix of above
                   ;

HexDigits <-- ^HexDigit (.* ^HexDigit)* ;

HexDigit <-- ['a'-'f'] / ['A'-'F'] / ['0'-'9'] ;

BinaryExponent <-- ['p','P'] ['+','\','-']? Digits ;

Digits <-- ['0'-'9'](.*['0'-'9'])* ;

%%-------------------------------------------------------------------------
%%  JLS 3.10.3  Boolean Literals
%%-------------------------------------------------------------------------

BooleanLiteral <-- TRUE / FALSE;

%%-------------------------------------------------------------------------
%%  JLS 3.10.4  Character Literals
%%-------------------------------------------------------------------------


CharLiteral <-- "\'" (Escape / '\n' / '\r') "\'" W ;

%%-------------------------------------------------------------------------
%%  JLS 3.10.5 String Literals
%%-------------------------------------------------------------------------

StringLiteral <-- '\"' (Escape / '\n' / '\r')* '\"' W ;

%%-------------------------------------------------------------------------
%%  JLS 3.10.6 Text Blocks
%%-------------------------------------------------------------------------

TextBlock <-- TRIQUOTE [ '\t','\012']* ['\r','\n']['\r','\n']? (StrongEscape / .)* TRIQUOTE W ;

TRIQUOTE <-- '\"' '\"' '\"';

%%-------------------------------------------------------------------------
%%  JLS 3.10.7 Escape Sequences
%%-------------------------------------------------------------------------

Escape <-- "\\" ['b','s','t','n','f','r','\"','\'','\\'] / OctalEscape / UnicodeEscape ;

StrongEscape <-- Escape
    / "\\" LineEscape
    ;

LineEscape <-- "\n\r" / "\r\n" / ['\r','\n'] ;

OctalEscape <-- ['0'-'3']['0'-'7']['0'-'7']
    / ['0'-'7']['0'-'7']
    / ['0'-'7']
    ;
UnicodeEscape <--  'u' HexDigit HexDigit HexDigit HexDigit ;

NullLiteral <-- NULL ;


%%-------------------------------------------------------------------------
%%  JLS 3.11  Separators
%%-------------------------------------------------------------------------

AT         <-- "@"      ~W ;
COLONCOLON <-- "::"     ~W ;
COMMA      <-- ","      ~W ;
DOT        <-- "."      ~W ;
ELLIPSIS   <-- "..."    ~W ;
LPAR       <-- "("      ~W ;
LBRK       <-- "["      ~W ;
RBRK       <-- "]"      ~W ;
RPAR       <-- ")"      ~W ;
LWING      <-- "{"      ~W ;
RWING      <-- "}"      ~W ;
SEMI       <-- ";"      ~W ;

%%-------------------------------------------------------------------------
%%  JLS 3.12  Operators
%%  The operators &, |, +, -, * appear as InfixOperators
%%  but also as AND, OR, PLUS, MINUS, STAR.
%%  The operators < and > appear as InfixOperators
%%  but also as LPOINT and RPOINT.
%%  The last two are used in the type context, to emulate the translation
%%  rule given in JLS 3.2.
%%-------------------------------------------------------------------------

ARROW           <--   "->"   W ;
AND             <--   "&"    W ;
BANG            <--   "!"    W ;
COLON           <--   ":"    W ;
DEC             <--   "--"   W ;
EQU             <--   "="    W ;
INC             <--   "++"   W ;
LPOINT          <--   "<"    W ;
MINUS           <--   "-"    W ;
OR              <--   '|'    W ;
QUERY           <--   '?'    W ;
PLUS            <--   '+'    W ;
RPOINT          <--   ">"    W ;
STAR            <--   "*"    W ;
TILDE           <--   "~"    W ;

InfixOperator <-- ( '|''|'
      / "&&"
      / "|"
      / "^"
      / "&"
      / "=="
      / "!="
      / ">>>"
      / "<<"
      / ">>"
      / "<="
      / ">="
      / "<"
      / ">"
      / "+"
      / "-"
      / "*"
      / "/"
      / "%"
      ) W ;

AssignmentOperator
    <-- -( "=" 
        / "*="  
        / "/=" 
        / "%=" 
        / "+=" 
        / "-=" 
        / ">>>=" 
        / "<<=" 
        / ">>=" 
        / "&=" 
        / "^=" 
        / "|=" 
      ) W ;


%%=========================================================================
%%  JLS 4  Types, Values and Variables
%%=========================================================================
%%-------------------------------------------------------------------------
%%  JLS 4.2  Primitive Types and Values
%%-------------------------------------------------------------------------

PrimitiveType <-- Annotation* NumericType
    / Annotation* BOOLEAN
    ;

NumericType <-- IntegralType
    / FloatingPointType
    ;

IntegralType <-- BYTE
    / SHORT
    / INT
    / LONG
    / CHAR
    ;

FloatingPointType
    <-- FLOAT
    / DOUBLE
    ;


%%-------------------------------------------------------------------------
%%  JLS 4.3  Reference Types and Values
%%-------------------------------------------------------------------------

ReferenceType
    <-- ArrayType
    / ClassType
    / TypeVariable
    ;

ClassType
    <-- ( Annotation* TypeIdentifier TypeArguments?
      / Name DOT Annotation* TypeIdentifier TypeArguments? )
      ( DOT Annotation* TypeIdentifier TypeArguments? )*
    ;

TypeVariable
    <-- Annotation* TypeIdentifier ;

ArrayType
    <-- PrimitiveType Dims
    / ClassType Dims
    / TypeVariable Dims
    ;

Dims
    <-- Annotation* LBRK RBRK (Annotation* LBRK RBRK)* ;


%%-------------------------------------------------------------------------
%%  JLS 4.4  Type Variables
%%-------------------------------------------------------------------------

TypeParameter
    <-- TypeParameterModifier* Identifier TypeBound? ;

TypeParameterModifier <-- Annotation ;

TypeBound
    <-- EXTENDS ClassType AdditionalBound*
    / EXTENDS TypeVariable
    ;

AdditionalBound
    <-- AND ClassType ;

%%-------------------------------------------------------------------------
%%  JLS 4.5  Parametrized Types
%%-------------------------------------------------------------------------

TypeArguments
    <-- LPOINT TypeArgumentList RPOINT ;

TypeArgumentList
    <-- TypeArgument (COMMA TypeArgument)* ;

TypeArgument
    <-- ReferenceType
    / Wildcard
    ;

Wildcard
    <-- Annotation* QUERY WildcardBounds? ;

WildcardBounds
    <-- EXTENDS ReferenceType
    / SUPER ReferenceType
    ;


%%=========================================================================
%%  JLS 6  Names
%%=========================================================================
%%-------------------------------------------------------------------------
%%  JLS 6.5  Determining the Meaning of a Name
%%-------------------------------------------------------------------------

Name
    <-- Identifier (DOT Identifier)* ;

TypeName
    <-- TypeIdentifier (DOT TypeIdentifier)* ;
    
%%=========================================================================
%%  JLS 7  Packages
%%=========================================================================
%%-------------------------------------------------------------------------
%%  JLS 7.3  Compilation Units
%%-------------------------------------------------------------------------

CompilationUnit
    <-- ModularCompilationUnit
    / OrdinaryCompilationUnit
    ;

OrdinaryCompilationUnit
    <-- PackageDeclaration? ImportDeclaration* TopLevelClassOrInterfaceDeclaration* ;

ModularCompilationUnit
    <-- ImportDeclaration* ModuleDeclaration ;

%%-------------------------------------------------------------------------
%%  JLS 7.4  Package Declarations
%%-------------------------------------------------------------------------

PackageDeclaration
    <-- PackageModifier* PACKAGE Identifier (DOT Identifier)* SEMI ;

PackageModifier
    <-- Annotation ;

%%-------------------------------------------------------------------------
%%  JLS 7.5  Import Declarations
%%-------------------------------------------------------------------------

ImportDeclaration
    <-- SingleTypeImportDeclaration
    / TypeImportOnDemandDeclaration
    / SingleStaticImportDeclaration
    / StaticImportOnDemandDeclaration
    / SEMI
    ;

SingleTypeImportDeclaration
    <-- IMPORT TypeName SEMI ;

TypeImportOnDemandDeclaration
    <-- IMPORT Name DOT STAR SEMI ;

SingleStaticImportDeclaration
    <-- IMPORT STATIC TypeName (DOT Identifier)? SEMI ;

StaticImportOnDemandDeclaration
    <-- IMPORT STATIC TypeName DOT STAR SEMI ;

%%-------------------------------------------------------------------------
%%  JLS 7.6  Top Level Type Declarations
%%-------------------------------------------------------------------------

TopLevelClassOrInterfaceDeclaration
    <-- ClassDeclaration
    / InterfaceDeclaration
    / SEMI
    ;

%%-------------------------------------------------------------------------
%%  JLS 7.7  Module Declarations
%%-------------------------------------------------------------------------

ModuleDeclaration
    <-- Annotation* OPEN? MODULE Identifier (DOT Identifier)* LWING ModuleDirective* RWING ;

ModuleDirective
    <-- REQUIRES RequiresModifier* Name SEMI
    / EXPORTS Name (TO Name (COMMA Name)*)? SEMI
    / OPENS Name (TO Name (COMMA Name)*)? SEMI
    / USES TypeName SEMI
    / PROVIDES TypeName WITH TypeName (COMMA TypeName)* SEMI
    ;

RequiresModifier
    <-- TRANSITIVE
    / STATIC
    ;


%%=========================================================================
%%  JLS 8  Classes
%%=========================================================================
%%-------------------------------------------------------------------------
%%  JLS 8.1  Class Declarations
%%-------------------------------------------------------------------------

ClassDeclaration
    <-- NormalClassDeclaration
    / EnumDeclaration
    / RecordDeclaration
    ;

NormalClassDeclaration
    <-- ClassModifier* CLASS Identifier TypeParameters?
        ClassExtends? ClassImplements? ClassPermits? ClassBody
    ;

Teste <-- ClassModifier*;
ClassModifier
    <-- Annotation
    / ^PUBLIC
    / ^PROTECTED
    / ^PRIVATE
    / ^ABSTRACT
    / ^STATIC
    / ^FINAL
    / ^SEALED
    / ^NONSEALED
    / ^STRICTFP
    ;

TypeParameters
    <-- LPOINT TypeParameterList RPOINT ;

TypeParameterList
    <-- TypeParameter (COMMA TypeParameter)* ;

ClassExtends
    <-- EXTENDS ClassType ;

ClassImplements
    <-- IMPLEMENTS InterfaceTypeList ;

InterfaceTypeList
    <-- ClassType (COMMA ClassType)* ;
    
ClassPermits
    <-- PERMITS ClassType (COMMA ClassType)* ;

ClassBody
    <-- LWING ClassBodyDeclaration* RWING ;

ClassBodyDeclaration
    <-- ClassMemberDeclaration
    / InstanceInitializer
    / StaticInitializer
    / ConstructorDeclaration
    ;

ClassMemberDeclaration
    <-- FieldDeclaration
    / MethodDeclaration
    / ClassDeclaration
    / InterfaceDeclaration
    / SEMI
    ;

%%-------------------------------------------------------------------------
%%  JLS 8.3  Field Declarations
%%-------------------------------------------------------------------------

FieldDeclaration
    <-- FieldModifier* UnannType VariableDeclaratorList SEMI ;

FieldModifier
    <-- Annotation
    / PUBLIC
    / PROTECTED
    / PRIVATE
    / STATIC
    / FINAL
    / TRANSIENT
    / VOLATILE
    ;

VariableDeclaratorList
    <-- VariableDeclarator (COMMA VariableDeclarator)* ;

VariableDeclarator
    <-- VariableDeclaratorId (EQU VariableInitializer)? ;

VariableDeclaratorId
    <-- Identifier Dims? ;

VariableInitializer
    <-- Expression
    / ArrayInitializer
    ;

UnannType
    <-- UnannReferenceType
    / UnannPrimitiveType
    ;

UnannPrimitiveType
    <-- NumericType
    / BOOLEAN
    ;

UnannReferenceType
    <-- UnannArrayType
    / UnannClassType
    / UnannTypeVariable
    ;

UnannClassType
    <-- ( TypeIdentifier TypeArguments?
      / Name DOT Annotation* TypeIdentifier TypeArguments? )
      ( DOT Annotation* TypeIdentifier TypeArguments? )*
    ;

UnannTypeVariable
    <-- TypeIdentifier ;

UnannArrayType
    <-- UnannPrimitiveType Dims
    / UnannClassType Dims
    / UnannTypeVariable Dims
    ;

%%-------------------------------------------------------------------------
%%  JLS 8.4  Method Declarations
%%-------------------------------------------------------------------------

MethodDeclaration
    <-- MethodModifier* MethodHeader MethodBody ;

MethodModifier
    <-- Annotation
    / PUBLIC
    / PROTECTED
    / PRIVATE
    / ABSTRACT
    / STATIC
    / FINAL
    / SYNCHRONIZED
    / NATIVE
    / STRICTFP
    ;

MethodHeader
    <-- Result MethodDeclarator Throws?
    / TypeParameters Annotation* Result MethodDeclarator Throws?
    ;

Result
    <-- UnannType
    / VOID
    ;

MethodDeclarator
    <-- Identifier LPAR FormalParametersWithReceiver RPAR Dims? ;

FormalParametersWithReceiver
    <-- ReceiverParameter (COMMA FormalParameterList)?
    / FormalParameterList?
    ;

ReceiverParameter
    <-- Annotation* UnannType (Identifier DOT)* THIS ;

FormalParameterList
    <-- FormalParameter (COMMA FormalParameter)* (COMMA VariableArityParameter)? 
    / VariableArityParameter
    ;

FormalParameter
    <-- VariableModifier* UnannType VariableDeclaratorId ;

VariableArityParameter
    <-- VariableModifier* UnannType Annotation* ELLIPSIS Identifier ;

VariableModifier
    <-- Annotation
    / FINAL
    ;

Throws
    <-- THROWS ExceptionTypeList ;

ExceptionTypeList
    <-- ExceptionType (COMMA ExceptionType)* ;

ExceptionType
    <-- ClassType
    / TypeVariable
    ;

MethodBody
    <-- Block
    / SEMI
    ;


%%-------------------------------------------------------------------------
%%  JLS 8.6  Instance Initializers
%%-------------------------------------------------------------------------

InstanceInitializer
    <-- Block ;

%%-------------------------------------------------------------------------
%%  JLS 8.7  Static Initializers
%%-------------------------------------------------------------------------

StaticInitializer
    <-- STATIC Block ;

%%-------------------------------------------------------------------------
%%  JLS 8.8  Constructor Declarations
%%-------------------------------------------------------------------------

ConstructorDeclaration
    <-- ConstructorModifier* ConstructorDeclarator Throws? ConstructorBody ;

ConstructorModifier
    <-- Annotation
    / PUBLIC
    / PROTECTED
    / PRIVATE
    ;

ConstructorDeclarator
    <-- TypeParameters? SimpleTypeName LPAR FormalParametersWithReceiver RPAR ;

SimpleTypeName
    <-- TypeIdentifier ;

ConstructorBody
    <-- LWING ExplicitConstructorInvocation? BlockStatements? RWING ;

ExplicitConstructorInvocation
    <-- TypeArguments? THIS LPAR ArgumentList? RPAR SEMI
    / TypeArguments? SUPER LPAR ArgumentList? RPAR SEMI
    / Primary DOT TypeArguments? SUPER LPAR ArgumentList? RPAR SEMI
    / Name DOT TypeArguments? SUPER LPAR ArgumentList? RPAR SEMI
    ;

%%-------------------------------------------------------------------------
%%  JLS 8.9  Enum Types
%%-------------------------------------------------------------------------

EnumDeclaration
    <-- EnumModifier* ENUM TypeIdentifier ClassImplements? EnumBody ;
    
EnumModifier
    <-- Annotation
    / PUBLIC
    / PROTECTED
    / PRIVATE
    / STATIC
    / STRICTFP
    ;

EnumBody
    <-- LWING EnumConstantList? COMMA? EnumBodyDeclarations? RWING ;

EnumConstantList
    <-- EnumConstant (COMMA EnumConstant)* ;

EnumConstant
    <-- EnumConstantModifier* Identifier (LPAR ArgumentList? RPAR)? ClassBody? ;

EnumConstantModifier
    <-- Annotation ;

EnumBodyDeclarations
    <-- SEMI ClassBodyDeclaration* ;

%%-------------------------------------------------------------------------
%%  JLS 8.10 Record Classes
%%-------------------------------------------------------------------------

RecordDeclaration
    <-- RecordModifier* RECORD TypeIdentifier TypeParameters? RecordHeader
        ClassImplements? RecordBody ;
        
RecordModifier
    <-- Annotation
    / PUBLIC
    / PROTECTED
    / PRIVATE
    / STATIC
    / FINAL
    / STRICTFP
    ;

RecordHeader
    <-- LPAR RecordComponentList? RPAR ;
    
RecordComponentList
    <-- RecordComponent(COMMA RecordComponent)* ;
    
RecordComponent
    <-- RecordComponentModifier* UnannType Identifier
    / VariableArityRecordComponent
    ;

VariableArityRecordComponent
    <-- RecordComponentModifier* UnannType Annotation* ELLIPSIS Identifier ;
    
RecordComponentModifier
    <-- Annotation ;
    
RecordBody
    <-- LWING RecordBodyDeclaration* RWING ;
    
RecordBodyDeclaration
    <-- ClassBodyDeclaration
    / CompactConstructorDeclaration
    ;
    
CompactConstructorDeclaration
    <-- ConstructorModifier* SimpleTypeName ConstructorBody ;

%%=========================================================================
%%  JLS 9  Interfaces
%%=========================================================================
%%-------------------------------------------------------------------------
%%  JLS 9.1  Interface Declarations
%%-------------------------------------------------------------------------

InterfaceDeclaration
    <-- NormalInterfaceDeclaration
    / AnnotationTypeDeclaration
    ;

NormalInterfaceDeclaration
    <-- InterfaceModifier* INTERFACE TypeIdentifier TypeParameters?
          InterfaceExtends? InterfacePermits? InterfaceBody ;

InterfaceModifier
    <-- Annotation
    / PUBLIC
    / PROTECTED
    / PRIVATE
    / ABSTRACT
    / STATIC
    / SEALED
    / NONSEALED
    / STRICTFP
    ;

InterfaceExtends
    <-- EXTENDS InterfaceTypeList ;

InterfacePermits
    <-- PERMITS ClassType (COMMA ClassType)* ;

InterfaceBody
    <-- LWING InterfaceMemberDeclaration* RWING ;

InterfaceMemberDeclaration
    <-- ConstantDeclaration
    / InterfaceMethodDeclaration
    / ClassDeclaration
    / InterfaceDeclaration
    / SEMI
    ;

%%-------------------------------------------------------------------------
%%  JLS 9.3  Field (Constant) Declarations
%%-------------------------------------------------------------------------

ConstantDeclaration
    <-- ConstantModifier* UnannType VariableDeclaratorList SEMI ;

ConstantModifier
    <-- Annotation
    / PUBLIC
    / STATIC
    / FINAL
    ;

%%-------------------------------------------------------------------------
%%  JLS 9.4  Method Declarations
%%-------------------------------------------------------------------------

InterfaceMethodDeclaration
    <-- InterfaceMethodModifier* MethodHeader MethodBody ;

InterfaceMethodModifier
    <-- Annotation
    / PUBLIC
    / PRIVATE
    / ABSTRACT
    / DEFAULT
    / STATIC
    / STRICTFP
    ;

%%-------------------------------------------------------------------------
%%  JLS 9.6  Annotation types
%%-------------------------------------------------------------------------

AnnotationTypeDeclaration
    <-- InterfaceModifier* AT INTERFACE TypeIdentifier AnnotationTypeBody ;

AnnotationTypeBody
    <-- LWING AnnotationTypeMemberDeclaration* RWING ;

AnnotationTypeMemberDeclaration
    <-- AnnotationTypeElementDeclaration
    / ConstantDeclaration
    / ClassDeclaration
    / InterfaceDeclaration
    / SEMI
    ;

AnnotationTypeElementDeclaration
    <-- AnnotationTypeElementModifier* UnannType Identifier LPAR RPAR Dims*
         DefaultValue? SEMI ;

AnnotationTypeElementModifier
    <-- Annotation
    / PUBLIC
    / ABSTRACT
    ;

DefaultValue
    <-- DEFAULT ElementValue ;

%%-------------------------------------------------------------------------
%%  JLS 9.7  Annotations
%%-------------------------------------------------------------------------

Annotation
    <-- NormalAnnotation
    / SingleElementAnnotation
    / MarkerAnnotation
    ;

NormalAnnotation
    <-- AT TypeName LPAR ElementValuePairList? RPAR ;

ElementValuePairList
    <-- ElementValuePair (COMMA ElementValuePair)* ;

ElementValuePair
    <-- Identifier EQU ElementValue ;

ElementValue
    <-- ConditionalExpression
    / ElementValueArrayInitializer
    / Annotation
    ;

ElementValueArrayInitializer
    <-- LWING ElementValueList? COMMA? RWING ;

ElementValueList
    <-- ElementValue (COMMA ElementValue)* ;

MarkerAnnotation
    <-- AT TypeName ;

SingleElementAnnotation
    <-- AT TypeName LPAR ElementValue RPAR ;

%%=========================================================================
%%  JLS 10  Arrays
%%=========================================================================
%%-------------------------------------------------------------------------
%%  JLS 10.6  Array Initializers
%%-------------------------------------------------------------------------

ArrayInitializer
    <-- LWING VariableInitializerList? COMMA? RWING ;

VariableInitializerList
    <-- VariableInitializer (COMMA VariableInitializer)* ;

%%=========================================================================
%%  JLS 14  Blocks and Statements
%%=========================================================================
%%-------------------------------------------------------------------------
%%  JLS 14.2 Blocks
%%-------------------------------------------------------------------------

Block
    <-- LWING BlockStatements? RWING ;

BlockStatements
    <-- BlockStatement BlockStatement* ;

BlockStatement
    <-- LocalVariableDeclarationStatement
    / LocalClassOrInterfaceDeclaration
    / Statement
    ;

LocalClassOrInterfaceDeclaration
    <-- ClassDeclaration
    / NormalInterfaceDeclaration
    ;

%%-------------------------------------------------------------------------
%%  JLS 14.4  Local Variable Declaration Statements
%%-------------------------------------------------------------------------

LocalVariableDeclarationStatement
    <-- LocalVariableDeclaration SEMI ;

LocalVariableDeclaration
    <-- VariableModifier* LocalVariableType VariableDeclaratorList ;

LocalVariableType
    <-- UnannType
    / VAR
    ;

%%-------------------------------------------------------------------------
%%  JLS 14.5  Statements
%%-------------------------------------------------------------------------

Statement
    <-- Block
%%    / EmptyStatement
%%    / ExpressionStatement
%%    / AssertStatement
%%   / SwitchStatement
%%    / LabeledStatement
%%    / IfThenElseStatement
%%    / IfThenStatement
%%    / WhileStatement
%%    / ForStatement
%%    / DoStatement
%%    / BreakStatement
%%    / ContinueStatement
%%    / ReturnStatement
%%    / SynchronizedStatement
%%    / ThrowStatement
%%    / TryStatement /
    YieldStatement
    ;

%%-------------------------------------------------------------------------
%%  JLS 14.6  The Empty Statement
%%-------------------------------------------------------------------------

EmptyStatement
    <-- SEMI ;

%%-------------------------------------------------------------------------
%%  JLS 14.7  Labeled Statement
%%-------------------------------------------------------------------------

LabeledStatement
    <-- Identifier COLON Statement ;

%%-------------------------------------------------------------------------
%%  JLS 14.8  Expression Statements
%%-------------------------------------------------------------------------

ExpressionStatement
    <-- '0'; %% StatementExpression SEMI ;

StatementExpression
    <-- '0' %% Assignment
%%    / PreIncrementExpression
%%    / PreDecrementExpression
%%    / PostfixExpression
%%    / MethodInvocation
%%    / ClassInstanceCreationExpression
    ;

%%-------------------------------------------------------------------------
%%  JLS 14.9  The If Statement
%%-------------------------------------------------------------------------

IfThenStatement
    <-- IF LPAR Expression RPAR Statement ;

IfThenElseStatement
    <-- IF LPAR Expression RPAR Statement ELSE Statement ;

%%-------------------------------------------------------------------------
%%  JLS 14.10  The Assert Statement
%%-------------------------------------------------------------------------

AssertStatement
    <-- ASSERT Expression SEMI
    / ASSERT Expression COLON Expression SEMI
    ;

%%-------------------------------------------------------------------------
%%  JLS 14.11  The SWITCH Statement
%%-------------------------------------------------------------------------

SwitchStatement
    <-- SWITCH LPAR Expression RPAR SwitchBlock ;

SwitchBlock
    <-- LWING SwitchRule+ RWING
    / LWING SwitchBlockStatementGroup* RWING 
    ;

SwitchRule 
    <-- SwitchLabel ARROW Expression SEMI
    / SwitchLabel ARROW Block
    / SwitchLabel ARROW ThrowStatement
    ;

SwitchBlockStatementGroup
    <-- (SwitchLabel COLON)+ BlockStatements? ;

SwitchLabel
    <-- CASE CaseConstant (COMMA CaseConstant)* 
    / DEFAULT
    ;
    
CaseConstant
    <-- ConditionalExpression;

%%-------------------------------------------------------------------------
%%  JLS 14.12  The WHILE Statement
%%-------------------------------------------------------------------------

WhileStatement
    <-- WHILE LPAR Expression RPAR Statement ;

%%-------------------------------------------------------------------------
%%  JLS 14.13  The DO Statement
%%-------------------------------------------------------------------------

DoStatement
    <-- DO Statement WHILE LPAR Expression RPAR SEMI;

%%-------------------------------------------------------------------------
%%  JLS 14.14  The FOR Statement
%%-------------------------------------------------------------------------

ForStatement
    <-- BasicForStatement
    / EnhancedForStatement
    ;

BasicForStatement
    <-- FOR LPAR ForInit? SEMI Expression? SEMI ForUpdate? RPAR Statement ;

ForInit
    <-- LocalVariableDeclaration
    / StatementExpressionList
    ;

ForUpdate
    <-- StatementExpressionList ;

StatementExpressionList
    <-- StatementExpression (COMMA  StatementExpression)* ;

EnhancedForStatement
    <-- FOR LPAR VariableModifier* LocalVariableType VariableDeclaratorId COLON
          Expression RPAR Statement ;

%%-------------------------------------------------------------------------
%%  JLS 14.15  The BREAK Statement
%%-------------------------------------------------------------------------

BreakStatement
    <-- BREAK Identifier? SEMI ;

%%-------------------------------------------------------------------------
%%  JLS 14.16  The CONTINUE Statement
%%-------------------------------------------------------------------------

ContinueStatement
    <-- CONTINUE Identifier? SEMI ;

%%-------------------------------------------------------------------------
%%  JLS 14.17  The RETURN Statement
%%-------------------------------------------------------------------------

ReturnStatement
    <-- RETURN Expression? SEMI ;

%%-------------------------------------------------------------------------
%%  JLS 14.18  The THROW Statement
%%-------------------------------------------------------------------------

ThrowStatement
    <-- THROW Expression SEMI ;

%%-------------------------------------------------------------------------
%%  JLS 14.19  The SYNCHRONIZED Statement
%%-------------------------------------------------------------------------

SynchronizedStatement
    <-- SYNCHRONIZED LPAR Expression RPAR Block ;

%%-------------------------------------------------------------------------
%%  JLS 14.20  The TRY Statement
%%-------------------------------------------------------------------------

%%TryStatement
%%    <-- TRY Block Catches? Finally
%%    / TRY Block Catches
%%    / TryWithResourcesStatement
%%    ;

TryStatement
    <-- TRY Block TryFinals 
    / TRY Block Catches
    / TryWithResourcesStatement
    ;

TryFinals <-- Catches Finally?
            / Finally
             ;
Catches
    <-- CatchClause CatchClause* ;

CatchClause
    <-- CATCH LPAR CatchFormalParameter RPAR Block ;

CatchFormalParameter
    <-- VariableModifier* CatchType VariableDeclaratorId ;

CatchType
    <-- UnannClassType (OR ClassType)* ;

Finally
    <-- FINALLY Block ;

TryWithResourcesStatement
    <-- TRY ResourceSpecification Block Catches? Finally? ;

ResourceSpecification
    <-- LPAR ResourceList SEMI? RPAR ;

ResourceList
    <-- Resource (SEMI Resource)* ;

Resource
    <-- VariableModifier* LocalVariableType Identifier EQU Expression
    / VariableAccess
    ;

VariableAccess
    <-- Name
    / FieldAccess
    ;

%%-------------------------------------------------------------------------
%%  JLS 14.21  The YIELD Statement
%%-------------------------------------------------------------------------

YieldStatement
    <-- YIELD Expression SEMI ;

%%-------------------------------------------------------------------------
%%  JLS 14.30  Pattern
%%-------------------------------------------------------------------------

Pattern
    <-- VariableModifier* UnannReferenceType Identifier ;

%%=========================================================================
%%  JLS 15  Expressions
%%=========================================================================
%%-------------------------------------------------------------------------
%%  JLS 15.2  Forms of Expression
%%-------------------------------------------------------------------------

Expression
    <-- LambdaExpression
    / Assignment
    / ConditionalExpression
    ;

%%-------------------------------------------------------------------------
%%  JLS 15.8  Primary Expression
%%-------------------------------------------------------------------------

Primary <-- Literal PrimaryR
          / ClassLiteral PrimaryR
          / THIS PrimaryR
          / TypeName DOT TypeNamePrimSeq PrimaryR
          / LPAR Expression RPAR PrimaryR
        %%/ ArrayOrFieldAccess PrimaryR
          / ArrayCreationExpressionWithInitializer LBRK Expression RBRK PrimaryR %% From ArrayAcces
          / SUPER  SuperPrimSeq  PrimaryR          %% From FieldAccess
          / MethodInvocation PrimaryR
          / ArrayCreationExpression PrimaryR
          / Name NamePrimSeq PrimaryR
          / ReferenceType COLONCOLON TypeArguments?  Identifier PrimaryR
          / ArrayType COLONCOLON NEW PrimaryR 
          / ClassType COLONCOLON TypeArguments?  NEW PrimaryR
          / UnqualifiedClassInstanceCreationExpression PrimaryR
          ; 

SuperPrimSeq  <--  DOT Identifier  
                /  COLONCOLON TypeArguments?  Identifier PrimaryR
                ;
TypeNamePrimSeq <-- THIS PrimaryR
                  / SUPER COLONCOLON TypeArguments?  Identifier
                  / SUPER DOT Identifier
                  ;

NamePrimSeq <-- DOT UnqualifiedClassInstanceCreationExpression
               / COLONCOLON TypeArguments?  Identifier
               / LBRK Expression RBRK
               ;

PrimaryR <-- DOT UnqualifiedClassInstanceCreationExpression 
     /  COLONCOLON TypeArguments?  Identifier PrimaryR
     / ( LBRK Expression RBRK /  DOT Identifier) PrimaryR
     / epsilon
    ;

ClassLiteral
    <-- TypeName (LBRK RBRK)* DOT CLASS
    / NumericType (LBRK RBRK)* DOT CLASS
    / BOOLEAN (LBRK RBRK)* DOT CLASS
    / VOID DOT CLASS
    ;


%%-------------------------------------------------------------------------
%%  JLS 15.9  Class Instance Creation Expressions
%%-------------------------------------------------------------------------

ClassInstanceCreationExpression
    <-- Primary DOT UnqualifiedClassInstanceCreationExpression
    / Name DOT UnqualifiedClassInstanceCreationExpression
    / UnqualifiedClassInstanceCreationExpression
    ;

UnqualifiedClassInstanceCreationExpression
    <-- NEW TypeArguments? ClassOrInterfaceTypeToInstantiate LPAR ArgumentList* RPAR ClassBody? ;

ClassOrInterfaceTypeToInstantiate
    <-- Annotation* Identifier TypeArgumentsOrDiamond? 
        (DOT Annotation* Identifier TypeArgumentsOrDiamond?)* 
    ;

TypeArgumentsOrDiamond
    <-- TypeArguments
    / LPOINT RPOINT &LPAR  %% Must be last before LPAR
    ;

%%-------------------------------------------------------------------------
%%  JLS 15.10  Array Creation and Access Expressions
%%-------------------------------------------------------------------------


ArrayCreationExpression
    <-- ArrayCreationExpressionWithoutInitializer
    / ArrayCreationExpressionWithInitializer
    ;

ArrayCreationExpressionWithoutInitializer
    <-- NEW PrimitiveType DimExprs Dims?
    / NEW ClassType DimExprs Dims?
    ;
    
ArrayCreationExpressionWithInitializer    
    <-- NEW PrimitiveType Dims ArrayInitializer
    / NEW ClassType Dims ArrayInitializer
    ;

DimExprs
    <-- DimExpr DimExpr* ;

DimExpr
    <-- Annotation* LBRK Expression RBRK ;

ArrayAccess                       %% Is embedded in ArrayOrFieldAccess
    <-- Primary LBRK Expression RBRK
    / Name LBRK Expression RBRK
    / ArrayCreationExpressionWithInitializer LBRK Expression RBRK
    ;

%%-------------------------------------------------------------------------
%%  JLS 15.11  Field Access Expressions
%%-------------------------------------------------------------------------

FieldAccess
    <-- Primary DOT Identifier
    / SUPER DOT Identifier
    / TypeName DOT SUPER DOT Identifier
    ;


%%-------------------------------------------------------------------------
%%  JLS 15.12  Method Invocation Expressions
%%-------------------------------------------------------------------------

MethodInvocation
    <-- '0'
    ;

ArgumentList
    <-- Expression (COMMA Expression)*;

%%-------------------------------------------------------------------------
%%  JLS 15.13  Method Reference Expression
%%-------------------------------------------------------------------------

MethodReference
    <-- Primary COLONCOLON TypeArguments?  Identifier
    / Name COLONCOLON TypeArguments?  Identifier
    / ReferenceType COLONCOLON TypeArguments?  Identifier
    / SUPER COLONCOLON TypeArguments?  Identifier
    / TypeName DOT SUPER COLONCOLON TypeArguments?  Identifier
    / ArrayType COLONCOLON NEW
    / ClassType COLONCOLON TypeArguments?  NEW
    ;

%%-------------------------------------------------------------------------
%%  JLS 15.14  Postfix Expressions
%%-------------------------------------------------------------------------

PostfixExpression
    <-- Primary (INC / DEC)?
    / Name (INC / DEC)?
    ;

%%-------------------------------------------------------------------------
%%  JLS 15.15  Unary Operators
%%-------------------------------------------------------------------------

UnaryExpression
    <-- PreIncrementExpression
    / PreDecrementExpression
    / PLUS UnaryExpression
    / MINUS UnaryExpression
    / UnaryExpressionNotPlusMinus
    ;


PreIncrementExpression
    <-- INC UnaryExpression ;

PreDecrementExpression
    <-- DEC UnaryExpression ;

UnaryExpressionNotPlusMinus
    <-- CastExpression
    / PostfixExpression
    / TILDE UnaryExpression
    / BANG UnaryExpression
    / SwitchExpression
    ;

%%-------------------------------------------------------------------------
%%  JLS 15.16  Cast Expressions
%%-------------------------------------------------------------------------

CastExpression
    <-- LPAR ReferenceType AdditionalBound* RPAR LambdaExpression
    / LPAR ReferenceType AdditionalBound* RPAR UnaryExpressionNotPlusMinus
    / LPAR PrimitiveType RPAR UnaryExpression
    ;

%%-------------------------------------------------------------------------
%%  JLS 15.17-24 Infix Expression
%%-------------------------------------------------------------------------

InfixExpression
    <-- UnaryExpression
          ((InfixOperator UnaryExpression) / (INSTANCEOF (Pattern/ReferenceType)))* ;

%%-------------------------------------------------------------------------
%%  JLS 15.25  Conditional Operator ? :
%%-------------------------------------------------------------------------

ConditionalExpression
    <-- InfixExpression (QUERY Expression COLON (LambdaExpression / ConditionalExpression))?
    ;

%%-------------------------------------------------------------------------
%%  JLS 15.26  Assignment Operators
%%-------------------------------------------------------------------------

Assignment
    <-- LeftHandSide AssignmentOperator Expression ;

LeftHandSide
    <--  ArrayOrFieldAccess
    / Name
    / LPAR LeftHandSide RPAR
    ;

ArrayOrFieldAccess
    <-- Primary
      ( LBRK Expression RBRK            %% From ArrayAccess
        /  DOT Identifier)                %% From FieldAccess
    / Name LBRK Expression RBRK         %% From ArrayAccess
    / ArrayCreationExpressionWithInitializer LBRK Expression RBRK %% From ArrayAcces
    / SUPER DOT Identifier              %% From FieldAccess
    / TypeName DOT SUPER DOT Identifier %% From FieldAccess
    ;

%%-------------------------------------------------------------------------
%%  JLS 15.27  Lambda Expressions
%%-------------------------------------------------------------------------

LambdaExpression
    <-- LambdaParameters ARROW LambdaBody ;

LambdaParameters
    <-- LPAR LambdaParameterList? RPAR
    / Identifier
    ;

LambdaParameterList
    <-- LambdaParameter (COMMA LambdaParameter)* (COMMA VariableArityParameter)?
    / VariableArityParameter
    / Identifier (COMMA Identifier)*
    ;

LambdaParameter
    <-- VariableModifier* LambdaParameterType VariableDeclaratorId ;

LambdaParameterType
    <-- UnannType
    / VAR;

LambdaBody
    <--Expression
    / Block
    ;

%%-------------------------------------------------------------------------
%%  JLS 15.28  Switch Expressions
%%-------------------------------------------------------------------------

SwitchExpression 
    <-- SWITCH LPAR Expression RPAR SwitchBlock ; 

%%-------------------------------------------------------------------------
%%  JLS 15.29  Constant Expressions
%%-------------------------------------------------------------------------

ConstantExpression <-- Expression ;

start: Compilation