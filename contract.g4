
// Based on solidity version 0.8.15


grammar contract;

sourceUnit
  : '// SPDX-License-Identifier: GPL-3.0' '\n' pragmaDirective (contractDefinition | libraryDefinition | interfaceDefinition )* EOF;

pragmaDirective
  : 'pragma solidity ^0.8.15'   ';' ;

versionConstraint
  : versionOperator | VersionLiteral ;

versionOperator
  : '^' ;

VersionLiteral
  : [0-9]+ '.' [0-9]+ '.' [0-9]+ ;


contractDefinition
  : ('\n'  'abstract' 'contract'  identifier '{' '\n' (contractPart1'\n')*  ('\n' fallbackDefinition)? ('\n' receiveDefinition)?  ('\n' constructorDefinition1)? '}') 
  | ('\n'   'contract'  identifier '{' '\n' (contractPart2'\n')* ('\n' fallbackDefinition2)? ('\n' receiveDefinition2)? ('\n' constructorDefinition2)? '}')  ; 


interfaceDefinition
 : '\n'  'interface' identifier '{' '\n' (interfacePart'\n')*   ('\n' interfacefallbackDefinition)?  ('\n' interfacereceiveDefinition)? '\n' '}' ;

interfacePart
  : interfacefunctionDefinition 
  | structDefinition
  | enumDefinition
  | usertypeDefinition
  | eventDefinition  
  | errorDefinition   ;


interfacestateVariableDeclaration
  : typeName (ConstantKeyword | PublicKeyword | InternalKeyword | PrivateKeyword | ImmutableKeyword) identifier  ('=' expression)? ';' ;

interfacereceiveDefinition
  : ReceiveKeyword  '(' ')'  ExternalKeyword PayableKeyword  VirtualKeyword?  ';'  ;

interfacefallbackDefinition 
  : FallbackKeyword  '(' parameterList? ')' ExternalKeyword  PayableKeyword?  VirtualKeyword?   returnParameters?  ';'   ;

interfacefunctionDefinition
  : 'function' ( identifier | ReceiveKeyword | FallbackKeyword )  '(' parameterList? ')'  'external' stateMutability?  VirtualKeyword?  returnParameters? ';'  ;

libraryDefinition
  : '\n'  'library' identifier '{' '\n' (libraryPart'\n')*  '}' ;

libraryfunctionDefinition
  : 'function' ( identifier | ReceiveKeyword | FallbackKeyword )  '(' parameterList? ')'  visibility stateMutability1? modifierInvocation?  ( returnParameters | block ) ;

librarymodifierDefinition 
  :  'modifier' identifier  ( '(' parameterList? ')' )?   modifierblock ;

libraryPart
  | libraryfunctionDefinition 
  | librarymodifierDefinition 
  | structDefinition
  | enumDefinition
  | usertypeDefinition
  | librarystateVariableDeclaration  
  | eventDefinition  
  | errorDefinition  ;


librarystateVariableDeclaration
  : typeName ConstantKeyword (PublicKeyword | InternalKeyword | PrivateKeyword) identifier  '=' expression ';' ;


contractPart1
  : functionDefinition1 
  | modifierDefinition1 
  | structDefinition
  | enumDefinition
  | usertypeDefinition
  | stateVariableDeclaration  
  | eventDefinition  
  | errorDefinition  ;

contractPart2
  : functionDefinition2
  | modifierDefinition2 
  | structDefinition
  | enumDefinition
  | usertypeDefinition
  | stateVariableDeclaration  
  | eventDefinition  
  | errorDefinition  ;



usingDefinition
  : 'using'  (identifierPath | ('{' identifierPath (',' identifierPath)* '}') )  'for' ('*' | typeName)  'global'? ';' ;

errorDefinition
  : 'error' identifier '(' errorParameterList? ')'  ';' ;

errorParameterList
  : errorParameter (',' errorParameter)*  ;

errorParameter
  : typeName identifier? ;


eventDefinition
  : 'event' identifier '(' eventParameterList? ')' AnonymousKeyword? ';' ;

eventParameterList
  : eventParameter (',' eventParameter)*  ;

eventParameter
  : typeName IndexedKeyword? identifier? ;

enumDefinition
  : 'enum' identifier '{' enumValue (',' enumValue)* '}' ;

enumValue
  : identifier ;


receiveDefinition
: receiveDefinition1 | receiveDefinition2 ;

receiveDefinition1
  : ReceiveKeyword  '(' ')'  ExternalKeyword PayableKeyword  VirtualKeyword ';'  ;

receiveDefinition2
  : ReceiveKeyword  '(' ')'  ExternalKeyword PayableKeyword  VirtualKeyword? modifierInvocation? block  ;

fallbackDefinition
  : fallbackDefinition1 | fallbackDefinition2 ;

fallbackDefinition1
  : FallbackKeyword  '(' parameterList? ')' ExternalKeyword PayableKeyword?  returnParameters?  VirtualKeyword ';' ;

fallbackDefinition2
  : FallbackKeyword  '(' parameterList? ')' ExternalKeyword PayableKeyword?  returnParameters?  VirtualKeyword? modifierInvocation?  block ;



functionDefinition
  : functionDefinition1|functionDefinition2 ;

functionDefinitionOrigin
  : 'function' ( identifier | ReceiveKeyword | FallbackKeyword )  '(' parameterList? ')'  ( (visibilityN VirtualKeyword?)|'private' )  stateMutability?  returnParameters? ( ';' | modifierInvocation? block ) ;

functionDefinition1
  : 'function' ( identifier | ReceiveKeyword | FallbackKeyword )  '(' parameterList? ')'  ((visibilityN VirtualKeyword stateMutability1?)  | (visibilityM VirtualKeyword stateMutability?))  
  returnParameters? ';' ;

functionDefinition2
  : 'function' ( identifier | ReceiveKeyword | FallbackKeyword )  '(' parameterList? ')'  (( visibilityN VirtualKeyword?  stateMutability1? ) | ('private' stateMutability1? ) | (visibilityM VirtualKeyword? stateMutability?))   
  returnParameters? modifierInvocation? block ;


visibilityM
  : 'external' |'public' ;

stateMutability1
  : 'pure' | 'view' ;

constructorDefinition
  : 'constructor' '(' parameterList? ')'  modifierInvocation? PayableKeyword? (InternalKeyword | PublicKeyword)? block ;

constructorDefinition1
  : 'constructor' '(' parameterList? ')'  modifierInvocation? PayableKeyword? InternalKeyword? block ;

constructorDefinition2
  : 'constructor' '(' parameterList? ')'  modifierInvocation? PayableKeyword? PublicKeyword? block ;

usertypeDefinitionOrigin
  : 'type' identifier 'is' elementaryTypeName ';' ;

usertypeDefinition
  : 'type' identifier 'is' elementaryTypeNameN ';' ;

elementaryTypeNameN
  : 'address' PayableKeyword? | 'bool' | Int | Uint | Byte | 'fixed' | 'ufixed'  ;

block
  : '{' (statement uncheckedblock?)? '}' ;

modifierblock
  : '{' (statement uncheckedblock?)?  '_;' '}' ;  


block725
  : '{' ( statement | uncheckedblock )? '}' ;

uncheckedblock
  : 'unchecked' block ;

structDefinition
  : 'struct' identifier  '{' structMember structMember*  '}' ;

structMember
  : typeName identifier ';' ;


statement
  : block
  | variableDeclarationStatement
  | expressionStatement
  | ifStatement
  | forStatement
  | whileStatement
  | doWhileStatement
  | continueStatement
  | breakStatement
  | tryStatement 
  | returnStatement
  | emitStatement
  | revertStatement ;

revertStatement
:  'revert' expression callArgumentList ';' ;

emitStatement
  : 'emit' expression callArgumentList ';' ;

callArgumentList
: '(' ('{' nameValueList? '}' | expressionList? ) ')' ;

tryStatement : 'try' expression returnParameters? block catchClause+ ;

catchClause : 'catch' ( identifier? '(' parameterList ')' )? block ;

continueStatement
  : 'continue' ';' ;

breakStatement
  : 'break' ';' ;

doWhileStatement
  : 'do' statement 'while' '(' expression ')' ';' ;

whileStatement
  : 'while' '(' expression ')' statement ;

expressionStatement
: expression ';' ;

forStatement
  : 'for' '(' ( simpleStatement | ';' ) ( expressionStatement | ';' ) expression? ')' statement ;

simpleStatement
  : ( variableDeclarationStatement | expressionStatement ) ;

variableDeclarationStatement
: variableDeclaration ('=' expression)? ';' ;

variableDeclaration
: typeName dataLocation? identifier ;

ifStatement
  : 'if' '(' expression ')' statement ( 'else' statement )? ;

returnStatement
  : 'return'  ';' ;




stateVariableDeclaration
  : stateVariableDeclaration1 | stateVariableDeclaration2 ;


stateVariableDeclaration1
  : typeName (ConstantKeyword | ImmutableKeyword) ( PublicKeyword | InternalKeyword | PrivateKeyword )
    identifier  ('=' expression) ';' ;

stateVariableDeclaration2
  : typeName ( PublicKeyword | InternalKeyword | PrivateKeyword )
    identifier  ('=' expression)? ';' ;



typeName 
  : elementaryTypeName
  | mapping 
  | functionTypeName 
  | typeName '[' expression? ']' ;




functionTypeName
  : 'function' '(' parameterList? ')' (visibility | stateMutability) returnParameters?  ;

visibility
  : 'internal' | 'external' | 'private' | 'public' ;

visibilityN
  : 'internal' | 'external' |'public' ;

stateMutability
  : 'pure' | 'view' | 'payable' ;
 

returnParameters
  : 'returns' '(' parameterList ')'  ;


parameterList
  : parameter (',' parameter)*  ;

parameter
  : typeName dataLocation? identifier? ;

parameterListc
  : parameterc (',' parameterc)*   ;

parameterc
  : typeName dataLocation identifier ;

constructorLocation
  : 'memory' | 'storage' ;

dataLocation
  : 'memory' | 'storage' | 'calldata';

modifierDefinition1
  : 'modifier' identifier  ('(' parameterList? ')' )? VirtualKeyword  ';'  ;

modifierDefinition2
  : 'modifier' identifier  ('(' parameterList? ')' )? VirtualKeyword?  modifierblock  ;



modifierDefinitionOrigin
  : 'modifier' identifier  ('(' parameterList? ')' )? ( VirtualKeyword  )? ( ';' | block ) ;

modifierListOrigin
  : ( stateMutability | ExternalKeyword
    | PublicKeyword | InternalKeyword | PrivateKeyword | VirtualKeyword | overrideSpecifier )* ;

modifierList
  : ( stateMutability | ExternalKeyword
    | PublicKeyword | InternalKeyword | PrivateKeyword | VirtualKeyword | overrideSpecifier )* ;


modifierInvocation
  :  ;

modifierInvocationOrigin
  : identifier ;


overrideSpecifier : 'override' ( '(' identifierPath (','identifierPath)* ')' )? ;

inheritanceSpecifier
  : identifierPath ( '(' expressionList? ')' )? ;

//identifier-path
identifierPath
  : identifier('.'identifier )* ;


elementaryTypeName
  : 'address' PayableKeyword? | 'bool' | 'string' | 'bytes' | Int | Uint | Byte | 'fixed' | 'ufixed'  ;

mapping
  : 'mapping' '(' (elementaryTypeName) '=>' typeName ')' ;


Int
  : 'int' | 'int8' | 'int16' | 'int24' | 'int32' | 'int40' | 'int48' | 'int56' | 'int64' | 'int72' | 'int80' | 'int88' | 'int96' | 'int104' | 'int112' | 'int120' | 'int128' | 'int136' | 'int144' | 'int152' | 'int160' | 'int168' | 'int176' | 'int184' | 'int192' | 'int200' | 'int208' | 'int216' | 'int224' | 'int232' | 'int240' | 'int248' | 'int256' ;

Uint
  : 'uint' | 'uint8' | 'uint16' | 'uint24' | 'uint32' | 'uint40' | 'uint48' | 'uint56' | 'uint64' | 'uint72' | 'uint80' | 'uint88' | 'uint96' | 'uint104' | 'uint112' | 'uint120' | 'uint128' | 'uint136' | 'uint144' | 'uint152' | 'uint160' | 'uint168' | 'uint176' | 'uint184' | 'uint192' | 'uint200' | 'uint208' | 'uint216' | 'uint224' | 'uint232' | 'uint240' | 'uint248' | 'uint256' ;

Byte
  : 'bytes1' | 'bytes2' | 'bytes3' | 'bytes4' | 'bytes5' | 'bytes6' | 'bytes7' | 'bytes8' | 'bytes9' | 'bytes10' | 'bytes11' | 'bytes12' | 'bytes13' | 'bytes14' | 'bytes15' | 'bytes16' | 'bytes17' | 'bytes18' | 'bytes19' | 'bytes20' | 'bytes21' | 'bytes22' | 'bytes23' | 'bytes24' | 'bytes25' | 'bytes26' | 'bytes27' | 'bytes28' | 'bytes29' | 'bytes30' | 'bytes31' | 'bytes32' ;



expression
  : expression '[' expression? ']'
  | expression '[' expression? ':' expression? ']'
  | expression '.' (identifier)|('address')
  | expression '{' nameValueList '}'
  | expression callArgumentList
  | 'payable' callArgumentList
  | 'type' '(' typeName ')'
  |  ('++' | '--' | '!' | '~' | 'delete' | '-' ) expression
  | expression  ('++' | '--')
  | expression '**' expression
  | expression ('*' | '/' | '%') expression
  | expression ('+' | '-') expression
  | expression ('<<' | '>>' | '>>>' ) expression
  | expression '&' expression
  | expression '^' expression
  | expression '|' expression
  | expression ('<' | '>' | '<=' | '>=') expression
  | expression ('==' | '!=') expression
  | expression '&&' expression
  | expression '||' expression
  | expression '?' expression ':' expression
  | expression ('=' | '|=' | '^=' | '&=' | '<<=' | '>>=' | '>>>=' | '+=' | '-=' | '*=' | '/=' | '%=') expression
  | 'new' typeName
  | tupleExpression
  | inlineArrayExpression
  | identifier
  | literal 
  | elementaryTypeName ;


literal
  : numberLiteral
  | hexLiteral
  | BooleanLiteral
  | stringLiteral 
  | hexLiteral ;


primaryExpression
  : BooleanLiteral
  | numberLiteral
  | hexLiteral
  | stringLiteral
  | identifier ('[' ']')?
  | TypeKeyword
  | tupleExpression
  | typeNameExpression ('[' ']')? ;


tupleExpression
  : '(' expression? ( ',' expression? )* ')' ;


inlineArrayExpression
  : '[' ( expression ( ',' expression )* )? ']'  ;

typeNameExpression
  : elementaryTypeName
  | identifierPath ;



BooleanLiteral
  : 'true' | 'false' ;


numberLiteral
  : (DecimalNumber | HexNumber) NumberUnit? ;

DecimalNumber
  : ( DecimalDigits | (DecimalDigits? '.' DecimalDigits) ) ( [eE] '-'? DecimalDigits )? ;

fragment
DecimalDigits
  : [0-9] ( '_'? [0-9] )* ;

HexNumber
  : '0' 'x' HexDigits  ;

fragment
HexDigits
  : HexCharacter ( '_'? HexCharacter )* ;

NumberUnit
  : 'wei' | 'szabo' | 'finney' | 'ether'
  | 'seconds' | 'minutes' | 'hours' | 'days' | 'weeks' | 'years' ;

HexLiteralFragment
  : 'hex' (('"' HexDigits? '"') | ('\'' HexDigits? '\'')) ;

hexLiteral : HexLiteralFragment+ ;

fragment
HexPair
  : HexCharacter HexCharacter ;

fragment
HexCharacter
  : [0-9A-Fa-f] ;


stringLiteral
  : StringLiteralFragment+ ;

StringLiteralFragment
  : '"' DoubleQuotedStringCharacter* '"'
  | '\'' SingleQuotedStringCharacter* '\'' ;

fragment
DoubleQuotedStringCharacter
  : ~["\r\n\\] | ('\\' .) ;

fragment
SingleQuotedStringCharacter
  : ~['\r\n\\] | ('\\' .) ;



nameValueList
  : nameValue (',' nameValue)* ','? ;

nameValue
  : identifier ':' expression ;

expressionList
  : expression (',' expression)* ;

functionCallArguments
  : '{' nameValueList? '}'
  | expressionList? ;

AnonymousKeyword : 'anonymous' ;
BreakKeyword : 'break' ;
ConstantKeyword : 'constant' ;
ImmutableKeyword : 'immutable' ;
ContinueKeyword : 'continue' ;
LeaveKeyword : 'leave' ;
ExternalKeyword : 'external' ;
IndexedKeyword : 'indexed' ;
InternalKeyword : 'internal' ;
PayableKeyword : 'payable' ;
PrivateKeyword : 'private' ;
PublicKeyword : 'public' ;
VirtualKeyword : 'virtual' ;
PureKeyword : 'pure' ;
TypeKeyword : 'type' ;
ViewKeyword : 'view' ;
ConstructorKeyword : 'constructor' ;
FallbackKeyword : 'fallback' ;
ReceiveKeyword : 'receive' ;


identifierOrigin
  : Identifier ;

identifier
  : ('from' | 'error' | 'revert' |  'global' | Identifier) ;

OneWordidentifier
  : IdentifierStart ;

IdentifierString
  : '"' IdentifierStart IdentifierPart* '"' ;

Identifier
  : IdentifierStart IdentifierPart* ;

fragment
IdentifierStart
  : [a-zA-Z$_] ;

fragment
IdentifierPart
  : [a-zA-Z0-9$_] ;

