(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

exception SyntaxError of string

type contract = source_unit list

 and source_unit =
   | Pragma of (string * string)
   | Import of import_directive
   | ContractDefinition of contract_definition

 and import_directive = {
     import_from : string;
     import_list : (import_item * string option) list;
   }

 and import_item =
   | ImportAll of bool
   | ImportIdent of string

 and contract_definition = {
     contract_kind : contract_kind;
     contract_name : string;
     contract_inheritance : inheritance_specifier list;
     contract_parts : contract_part list;
   }

 and contract_kind =
   | KindContract
   | KindLibrary
   | KindInterface

 and inheritance_specifier =
   user_defined_type_name * expression list

 and user_defined_type_name = string list

 and contract_part =
   | FunctionDefinition of function_definition
   | UsingForDeclaration of string * type_name option
   | StateVariableDeclaration of type_name * function_modifier list *
                                   string * expression option
   | StructDefinition of string * variable_declaration list
   | ModifierDefinition of string * function_param list option * block
   | EventDefinition of string *
                          (type_name * indexed * string option) list * anonymous
   | EnumDefinition of string * string list


 and function_definition = {
     fun_name : string option;
     fun_params : function_param list;
     fun_modifiers : function_modifier list;
     fun_return : function_return option;
     fun_body : function_body option;
   }

 and function_param = type_name * string option

 and function_modifier =
   | ModifierInvocation of string * expression list option
   | ModifierConstant
   | ModifierPayable
   | ModifierExternal
   | ModifierPublic
   | ModifierInternal
   | ModifierPrivate

 and function_return = function_param list
 and function_body = block

 and type_name =
   ElementaryTypeName of elementary_type_name
 | UserDefinedTypeName of user_defined_type_name
 | Mapping of elementary_type_name * type_name
 | ArrayTypeName of type_name * expression option
 | FunctionTypeName of type_name list * function_modifier list
                       * type_name list option

 and elementary_type_name =
   | TypeAddress
   | TypeBool
   | TypeString
   | TypeVar
   | TypeInt of string
   | TypeUint of string
   | TypeByte of string
   | TypeFixed of string
   | TypeUfixed of string

 and expression =
   | ArrayAccess of expression * expression option
   | SuffixExpression of expression * string
   | BinaryExpression of expression * string * expression
   | FunctionCallExpression of expression * function_call_arguments
   | PrefixExpression of string * expression
   | BooleanLiteral of bool
   | NumberLiteral of string * number_unit option
   | HexLiteral of string
   | StringLiteral of string
   | TupleExpression of expression list
   | IdentifierExpression of string
   | ElementaryTypeNameExpression of elementary_type_name
   | FieldExpression of expression * string
   | IfExpression of expression * expression * expression
   | AssignExpression of expression * string * expression
   | NewExpression of type_name

 and function_call_arguments =
   | ExpressionList of expression list
   | NameValueList of (string * expression) list

 and block = statement list

 and statement =
   IfStatement of expression * statement * statement option
 | WhileStatement of expression * statement
 | ForStatement of statement option * expression option * expression option
                   * statement
 | Block of block
 | InlineAssemblyStatement (* TODO *)
 | DoWhileStatement of statement * expression
 | PlaceholderStatement
 | Continue
 | Break
 | Return of expression option
 | Throw
 | SimpleStatement of simple_statement

 and simple_statement =
   VariableDefinition of variable_definition
 | ExpressionStatement of expression

 and storage_location = Memory | Storage

 and indexed = Indexed | NotIndexed
 and anonymous = Anonymous | NotAnonymous

 and number_unit =
   Wei
 | Szabo
 | Finney
 | Ether
 | Seconds
 | Minutes
 | Hours
 | Days
 | Weeks
 | Years

 and variable_definition =
   | VarInfer of string list * expression option
   | VarType of variable_declaration * expression option

 and ambiguous_type_name_or_expression =
   | AmbigiousIdentifier of string list
   | AmbiguousArray of ambiguous_type_name_or_expression * expression option

 and variable_declaration = type_name * storage_location option * string

                                                                    (*

NOT IMPLEMENTED:

InlineAssemblyBlock = '{' AssemblyItem* '}'

AssemblyItem = Identifier | FunctionalAssemblyExpression | InlineAssemblyBlock | AssemblyLocalBinding | AssemblyAssignment | AssemblyLabel | NumberLiteral | StringLiteral | HexLiteral
AssemblyLocalBinding = 'let' Identifier ':=' FunctionalAssemblyExpression
AssemblyAssignment = ( Identifier ':=' FunctionalAssemblyExpression ) | ( '=:' Identifier )
AssemblyLabel = Identifier ':'
FunctionalAssemblyExpression = Identifier '(' AssemblyItem? ( ',' AssemblyItem )* ')'

                                                                     *)
