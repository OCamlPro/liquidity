(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open SolTypes

let spaces = String.make 1000 ' '
let bprint b indent s =
  Printf.bprintf b "%s%s\n" (String.sub spaces 0 indent) s

let rec bprint_contract b indent c =
  List.iter (bprint_source_unit b indent) c

and bprint_source_unit b indent = function
  | Pragma (s1,s2) ->
     bprint b indent "Pragma";
     let indent2 = indent + 2 in
     bprint b indent2 s1;
     bprint b indent2 s2
  | Import import_directive ->
     bprint b indent "Import";
     let indent2 = indent + 2 in
     bprint_import_directive b indent2 import_directive
  | ContractDefinition contract_definition ->
     bprint b indent "ContractDefinition";
     let indent2 = indent + 2 in
     bprint_contract_definition b indent2 contract_definition

and bprint_import_directive b indent import_directive =
  bprint b indent "{";
  let indent2 = indent + 2 in
  let indent4 = indent + 4 in
  bprint b indent2 import_directive.import_from;
  List.iter (fun (import_item, stringo) ->
      bprint_import_item b indent2 import_item;
      match stringo with
      | None -> ()
      | Some string ->
         bprint b indent4 string)
            import_directive.import_list;
  bprint b indent "}"

and bprint_import_item b indent = function
   | ImportAll true -> bprint b indent "ImportAll true"
   | ImportAll false -> bprint b indent "ImportAll false"
   | ImportIdent string ->
      bprint b indent (Printf.sprintf "ImportIdent %s" string)

and bprint_contract_definition b indent contract_definition =
  bprint b indent "{";
  let indent2 = indent + 2 in
  let indent4 = indent + 4 in
(*
     contract_kind : contract_kind;
     contract_name : string;
     contract_inheritance : inheritance_specifier list;
     contract_parts : contract_part list;
 *)
  bprint b indent "}"

         (*
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
                            *)

let string_of_code code =
  let b = Buffer.create 1000 in
  let indent = 0 in
  bprint_contract b indent code;
  Buffer.contents b
