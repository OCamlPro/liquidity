%{
(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, INRIA & OCamlPro SAS <fabrice@lefessant.net>     *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

    open SolTypes;;

    let import import_from import_list = { import_from; import_list }

    let rec expression_of_identifiers = function
      | [] -> assert false
      | [x] -> IdentifierExpression x
      | x :: y -> FieldExpression (expression_of_identifiers y,x)


    let rec expression_of_ambiguity = function
      | AmbigiousIdentifier l ->
         expression_of_identifiers (List.rev l)
      | AmbiguousArray (a, expo)  ->
         let a = expression_of_ambiguity a in
         ArrayAccess (a, expo)

    let rec type_name_of_ambiguity = function
      | AmbigiousIdentifier l -> UserDefinedTypeName l
      | AmbiguousArray (a, expo) ->
         ArrayTypeName (type_name_of_ambiguity a, expo)


%}


/* Keywords */

%token <string * string> PRAGMA
%token IMPORT
%token AS
%token FROM
%token CONTRACT
%token LIBRARY
%token INTERFACE
%token IS
%token FUNCTION
%token CONSTANT
%token PAYABLE
%token EXTERNAL
%token PUBLIC
%token INTERNAL
%token PRIVATE
%token RETURNS
%token ADDRESS
%token BOOL
%token STRING
%token VAR
%token <string>INT
%token <string>UINT
%token <string>BYTE
%token <string>FIXED
%token <string>UFIXED
%token CONTINUE
%token RETURN
%token BREAK
%token THROW
%token EQUAL
%token MEMORY
%token STORAGE
%token DELETE
%token EVENT
%token INDEXED
%token ANONYMOUS
%token IF
%token ELSE
%token WHILE
%token FOR
%token NEW
%token MAPPING
%token EQUALGREATER
%token USING
%token MODIFIER
%token STRUCT
%token ENUM
%token <bool> BOOLEANLITERAL
%token <SolTypes.number_unit> NUMBERUNIT
%token DO

/* ponctuation */
%token LBRACE
%token RBRACE
%token SEMI
%token LPAREN
%token RPAREN
%token COMMA
%token DOT
%token STARSTAR
%token STAR
%token DIV
%token PERCENT
%token PLUSPLUS
%token MINUSMINUS
%token PLUS
%token MINUS
%token COLON
%token EQUALEQUAL
%token BANGEQUAL
%token PIPEPIPE
%token AMPERAMPER
%token GREATERGREATER
%token LESSLESS
%token GREATER
%token GREATEREQUAL
%token LESS
%token LESSEQUAL
%token PIPE
%token AMPER
%token XOR
%token BANG
%token NOT
%token LBRACKET
%token RBRACKET
%token QUESTION
%token PLUSEQUAL
%token MINUSEQUAL
%token STAREQUAL
%token DIVEQUAL
%token PERCENTEQUAL
%token PIPEEQUAL
%token AMPEREQUAL
%token XOREQUAL
%token LESSLESSEQUAL
%token GREATERGREATEREQUAL
%token UNDERSCORE
%token HEX
%token <string> HEXSTRING

%token <string> HEXNUMBER
%token <string> DECIMALNUMBER
%token <string> STRINGLITERAL
%token <string> IDENTIFIER
%token EOF


/*
The precedences must be listed from low to high.
*/


%nonassoc EQUAL PIPEEQUAL XOREQUAL AMPEREQUAL LESSLESSEQUAL GREATERGREATEREQUAL PLUSEQUAL MINUSEQUAL STAREQUAL DIVEQUAL PERCENTEQUAL
%nonassoc QUESTION COLON
%left PIPEPIPE
%left AMPERAMPER
%nonassoc EQUALEQUAL BANGEQUAL
%nonassoc GREATER LESS GREATEREQUAL LESSEQUAL
%left PIPE
%left XOR
%left AMPER
%nonassoc LESSLESS GREATERGREATER
%left PLUS MINUS
%left STAR DIV PERCENT
%left STARSTAR
%nonassoc BANG NOT DELETE
%nonassoc DOT
%nonassoc PLUSPLUS MINUSMINUS




%type < SolTypes.contract > contracts
%start contracts

%%

contracts:
  | EOF {  [] }
  | PRAGMA contracts { Pragma $1 :: $2 }
  | import SEMI contracts { Import $1 :: $3 }
  | contract contracts { ContractDefinition $1 :: $2 }
;;

  import:
    | IMPORT STRINGLITERAL maybe_as_identifier {
               import $2 [ ImportAll false, $3 ]
             }
    | IMPORT STAR maybe_as_identifier FROM STRINGLITERAL {
               import $5 [ ImportAll true, $3 ]
             }
    | IMPORT identifier maybe_as_identifier FROM STRINGLITERAL {
               import $5 [ ImportIdent $2, $3 ]
             }
    | IMPORT LBRACE import_identifiers RBRACE FROM STRINGLITERAL {
               import $6 $3
             }
;;

  identifier:
    | IDENTIFIER { $1 }
    | FROM { "from" }
;;

  import_identifiers:
    | identifier maybe_as_identifier { [ ImportIdent $1, $2 ] }
    | identifier maybe_as_identifier COMMA import_identifiers {
                   (ImportIdent $1, $2) :: $4 }
;;

  maybe_as_identifier :
    { None }
    | AS identifier { Some $2 }
;;

  contract:
    | contract_kind identifier
                    maybe_contract_inheritance
                    LBRACE contract_parts RBRACE
                    { {
                        contract_kind = $1;
                        contract_name = $2;
                        contract_inheritance = $3;
                        contract_parts = $5;
                      }
                    }
;;

  contract_kind:
    | CONTRACT { KindContract }
    | LIBRARY { KindLibrary }
    | INTERFACE { KindInterface }
;;

  maybe_contract_inheritance:
    |    { [] }
    | IS inheritance_specifiers { $2 }
;;

  inheritance_specifiers:
    inheritance_specifier COMMA inheritance_specifiers { $1 :: $3 }
  | inheritance_specifier { [ $1] }
;;

  user_defined_type_name:
    | identifier DOT user_defined_type_name { $1 :: $3 }
    | identifier { [$1] }
;;

  inheritance_specifier:
    | user_defined_type_name expression_comma_list { $1,$2 }
    | user_defined_type_name { $1,[] }
;;

  maybe_expression_list:
    | { [] }
    | expression_list { $1 }
;;

  expression_comma_list:
    expression { [$1] }
  | expression COMMA expression_comma_list { $1 :: $3 }
;;

  contract_parts :
    | contract_part contract_parts    { $1 :: $2 }
    | { [ ] }
;;

  contract_part:
    | function_definition { FunctionDefinition $1 }
    | EVENT identifier LPAREN maybe_indexed_parameter_list RPAREN maybe_anonymous SEMI
          { EventDefinition ($2, $4, $6) }
    | USING identifier FOR STAR SEMI { UsingForDeclaration($2, None) }
    | USING identifier FOR type_name SEMI { UsingForDeclaration($2, Some $4) }

    | type_name declaration_modifiers identifier maybe_equal_expression SEMI
                { StateVariableDeclaration ($1,$2,$3,$4) }
    | MODIFIER identifier maybe_parameter_list block
               { ModifierDefinition ($2,$3,$4) }
    | STRUCT identifier LBRACE variable_declaration_list RBRACE
             { StructDefinition($2, $4) }
    | ENUM identifier LBRACE maybe_enum_value_list RBRACE
               { EnumDefinition ($2,$4) }
;;

  declaration_modifiers:
    | { [] }
    | declaration_modifier declaration_modifiers { $1 :: $2 }
;;

  maybe_parameter_list:
    | { None }
    | parameter_list { Some $1 }
;;

  enum_value_list:
    | enum_value { [ $1 ] }
    | enum_value COMMA enum_value_list { $1 :: $3 }
;;

  enum_value:
    | identifier { $1 }
;;

  maybe_enum_value_list:
    | { [] }
    | enum_value_list { $1 }
;;


  variable_declaration_list:
    | { [] }
    | variable_declaration SEMI variable_declaration_list { $1 :: $3 }
;;

  maybe_anonymous:
    | { NotAnonymous }
    | ANONYMOUS { Anonymous }
;;

  maybe_indexed_parameter_list:
    | { [] }
    | indexed_parameter_list { $1 }
;;

  indexed_parameter_list:
    | type_name maybe_INDEXED maybe_identifier { [$1,$2,$3] }
    | type_name maybe_INDEXED maybe_identifier COMMA indexed_parameter_list
                { ($1,$2,$3) :: $5 }
;;

  maybe_INDEXED:
| { NotIndexed }
  | INDEXED { Indexed }

function_definition:
  | FUNCTION maybe_identifier parameter_list
             function_modifiers maybe_return maybe_function_body
             { {
                 fun_name = $2;
                 fun_params = $3;
                 fun_modifiers = $4;
                 fun_return = $5;
                 fun_body = $6;
             }}
;;

  maybe_identifier:
    |    { None }
    | identifier { Some $1 }
;;

  parameter_list:
    | LPAREN RPAREN   { [] }
    | LPAREN parameter_comma_list RPAREN   { $2 }
;;

  parameter_comma_list:
    | type_name maybe_identifier { [$1,$2] }
    | type_name maybe_identifier COMMA parameter_comma_list { ($1,$2) :: $4 }
;;

  function_modifiers:
    | function_modifier function_modifiers { $1 :: $2 }
    |    { [] }
;;

  function_modifier:
    | modifier_invocation { $1 }
    | declaration_modifier { $1 }
;;

  declaration_modifier:
    | CONSTANT { ModifierConstant }
    | PAYABLE { ModifierPayable }
    | EXTERNAL { ModifierExternal }
    | PUBLIC { ModifierPublic }
    | INTERNAL { ModifierInternal }
    | PRIVATE { ModifierPrivate }
;;

  modifier_invocation:
    | identifier LPAREN maybe_expression_list RPAREN
                 { ModifierInvocation ($1, Some $3) }
    | identifier { ModifierInvocation ($1, None) }
;;

  maybe_return:
                | { None }
                | RETURNS parameter_list { Some $2 }
;;

  maybe_function_body:
                       | SEMI { None }
                       | block { Some $1 }
;;

  type_name:
    | non_ambiguous_type_name { $1 }
    | ambiguous_type_name_or_expression {
          type_name_of_ambiguity $1 }
;;

  non_ambiguous_type_name:
    | non_ambiguous_type_name LBRACKET maybe_expression RBRACKET
                              { ArrayTypeName($1, $3) }
    | elementary_type_name { ElementaryTypeName $1 }
    | MAPPING LPAREN elementary_type_name EQUALGREATER type_name RPAREN
              { Mapping ($3, $5) }
              /*
    | FUNCTION type_name_list function_modifiers
               { FunctionTypeName ($2,$3,None) }
    | FUNCTION type_name_list function_modifiers
               RETURNS type_name_list
               { FunctionTypeName ($2,$3,Some $5) }
               */
;;

  ambiguous_type_name_or_expression:
    | user_defined_type_name { AmbigiousIdentifier $1 }
    | ambiguous_type_name_or_expression
        LBRACKET maybe_expression RBRACKET {
          AmbiguousArray ($1,$3) }
;;

  type_name_list:
    | LPAREN maybe_type_name_comma_list RPAREN { $2 }
;;
  maybe_type_name_comma_list:
    | { [] }
    | type_name_comma_list { $1 }
;;

  type_name_comma_list:
    | type_name { [ $1 ] }
    | type_name COMMA type_name_comma_list { $1 :: $3 }
;;

  elementary_type_name:
    | ADDRESS { TypeAddress }
    | BOOL { TypeBool }
    | STRING { TypeString }
    | VAR { TypeVar }
    | INT { TypeInt $1 }
    | UINT { TypeUint $1 }
    | BYTE { TypeByte $1 }
    | FIXED { TypeFixed $1 }
    | UFIXED { TypeUfixed $1 }
;;

  block:
    | LBRACE statements RBRACE { $2 }
;;

  statements:
    | statement statements { $1 :: $2 }
    | statement_before_semi { [$1] }
    | { [] }
;;

  statement:
    | statement_no_semi { $1 }
      /*    | inline_assembly_statement { $1 } */
    | statement_opt_semi opt_semi { $1 }
    | statement_before_semi SEMI { $1 }
;;

  opt_semi:
    | SEMI { () }
    | { () }
;;

  statement_no_semi:
    | if_statement { $1 }
    | while_statement { $1 }
    | for_statement { $1 }
    | block { Block $1 }
;;

  statement_opt_semi:
    | UNDERSCORE { PlaceholderStatement }
;;

  statement_before_semi:
    | do_while_statement { $1 }
    | CONTINUE { Continue }
    | BREAK { Break }
    | RETURN maybe_expression { Return $2 }
    | THROW { Throw }
    | simple_statement { $1 }
;;

  if_statement:
    | IF LPAREN expression RPAREN statement ELSE statement
         { IfStatement ($3, $5, Some $7) }
    | IF LPAREN expression RPAREN statement
         { IfStatement ($3, $5, None) }
;;
  while_statement:
    | WHILE LPAREN expression RPAREN statement
            { WhileStatement ($3, $5) }
;;
  for_statement:
    FOR LPAREN maybe_simple_statement SEMI maybe_expression SEMI maybe_expression_statement RPAREN statement
        { ForStatement($3,$5,$7,$9) }
;;
   inline_assembly_statement: /* TODO */
                              | { InlineAssemblyStatement }
;;
   do_while_statement:
     | DO statement WHILE LPAREN expression RPAREN
          { DoWhileStatement($2,$5) }
;;
   simple_statement:
| variable_definition { SimpleStatement (VariableDefinition $1) }
| expression_statement { SimpleStatement (ExpressionStatement $1) }
;;

  maybe_simple_statement:
    | simple_statement { Some $1 }
    | { None }
;;

  maybe_expression_statement:
    | expression_statement { Some $1 }
    | { None }
;;

  expression_statement:
    | expression { $1 }
;;

  variable_definition:
    | VAR identifier_list maybe_equal_expression { VarInfer ($2,$3) }
    | variable_declaration maybe_equal_expression { VarType ($1,$2) }
;;

  maybe_equal_expression:
    | EQUAL expression { Some $2 }
    | { None }
;;
  variable_declaration:
    | type_name maybe_storage_location identifier { $1,$2,$3 }
;;

  maybe_storage_location:
    | { None }
    | storage_location { Some $1 }
;;

  storage_location:
    | MEMORY { Memory }
    | STORAGE { Storage }
;;

  identifier_list:
    | LPAREN identifier_comma_list RPAREN { $2 }
;;

  identifier_comma_list:
    | COMMA identifier_comma_list { $2 }
    | COMMA identifier identifier_comma_list { $2 :: $3 }
    | { [] }
;;










maybe_expression :
        { None }
        | expression { Some $1 }
;;

  expression:
    | non_ambiguous_expression { $1 }
    | ambiguous_type_name_or_expression {
          expression_of_ambiguity $1
        }
;;

  non_ambiguous_expression:
    | primary_expression { $1 }
    | expression PLUSPLUS { SuffixExpression ($1, "++") }
    | expression MINUSMINUS { SuffixExpression ($1, "--") }
    | NEW type_name { NewExpression $2 }
    | expression DOT identifier { FieldExpression ($1, $3) }
    | expression LBRACKET maybe_expression RBRACKET {
                   ArrayAccess ($1,$3) }
    | function_call { $1 }
    | LPAREN expression RPAREN { $2 }
    | BANG expression { PrefixExpression ("!", $2) }
    | DELETE expression { PrefixExpression ("delete", $2) }
    | NOT expression { PrefixExpression ("~", $2) }
    | PLUS expression { PrefixExpression ("+", $2) }
    | MINUS expression { PrefixExpression ("-", $2) }
    | PLUSPLUS expression { PrefixExpression ("++", $2) }
    | MINUSMINUS expression { PrefixExpression ("--", $2) }
    | expression STARSTAR expression { BinaryExpression ($1, "**", $3) }
    | expression STAR expression { BinaryExpression ($1, "*", $3) }
    | expression DIV expression { BinaryExpression ($1, "/", $3) }
    | expression PERCENT expression { BinaryExpression ($1, "%", $3) }
    | expression PLUS expression { BinaryExpression ($1, "+", $3) }
    | expression MINUS expression { BinaryExpression ($1, "-", $3) }
    | expression LESSLESS expression { BinaryExpression ($1, "<<", $3) }
    | expression GREATERGREATER expression { BinaryExpression ($1, ">>", $3) }
    | expression AMPER expression { BinaryExpression ($1, "&", $3) }
    | expression XOR expression { BinaryExpression ($1, "^", $3) }
    | expression PIPE expression { BinaryExpression ($1, "|", $3) }
    | expression LESS expression { BinaryExpression ($1, "<", $3) }
    | expression GREATER expression { BinaryExpression ($1, ">", $3) }
    | expression LESSEQUAL expression { BinaryExpression ($1, "<=", $3) }
    | expression GREATEREQUAL expression { BinaryExpression ($1, ">=", $3) }
    | expression EQUALEQUAL expression { BinaryExpression ($1, "==", $3) }
    | expression BANGEQUAL expression { BinaryExpression ($1, "!=", $3) }
    | expression AMPERAMPER expression { BinaryExpression ($1, "&&", $3) }
    | expression PIPEPIPE expression { BinaryExpression ($1, "||", $3) }
    | expression QUESTION expression COLON expression
                 { IfExpression ($1,$3,$5) }
    | expression EQUAL expression { AssignExpression($1, "=", $3) }
    | expression PIPEEQUAL expression { AssignExpression($1, "|=", $3) }
    | expression XOREQUAL expression { AssignExpression($1, "^=", $3) }
    | expression AMPEREQUAL expression { AssignExpression($1, "&=", $3) }
    | expression LESSLESSEQUAL expression { AssignExpression($1, "<<=", $3) }
    | expression GREATERGREATEREQUAL expression { AssignExpression($1, ">>=", $3) }
    | expression PLUSEQUAL expression { AssignExpression($1, "+=", $3) }
    | expression MINUSEQUAL expression { AssignExpression($1, "-=", $3) }
    | expression STAREQUAL expression { AssignExpression($1, "*=", $3) }
    | expression DIVEQUAL expression { AssignExpression($1, "/=", $3) }
    | expression PERCENTEQUAL expression { AssignExpression($1, "%=", $3) }

;;

primary_expression:
  | number_literal maybe_unit { NumberLiteral ($1,$2) }
  | STRINGLITERAL { StringLiteral $1 }
  | BOOLEANLITERAL { BooleanLiteral $1 }
  | elementary_type_name { ElementaryTypeNameExpression $1 }
  | HEX STRINGLITERAL { HexLiteral $2 }
  | HEX HEXSTRING { HexLiteral $2 }
  | tuple_expression { $1 }
;;

  tuple_expression:
    | LPAREN maybe_expression_list RPAREN { TupleExpression $2 }
;;

  number_literal:
    HEXNUMBER { $1 }
    | DECIMALNUMBER { $1 }
;;

  maybe_unit:
    | { None }
    | NUMBERUNIT { Some $1 }
;;

  function_call:
    expression LPAREN function_call_arguments RPAREN {
                 FunctionCallExpression ($1, $3)
               }
;;

  function_call_arguments:
    | maybe_expression_list { ExpressionList $1 }
    | LBRACE name_value_list RBRACE { NameValueList $2 }
;;

  expression_list:
    expression_comma_list { $1 }
;;

  name_value_list:
    | identifier COLON expression { [$1,$3] }
    | identifier COLON expression COMMA name_value_list { ($1,$3) :: $5 }
;;

  %%
