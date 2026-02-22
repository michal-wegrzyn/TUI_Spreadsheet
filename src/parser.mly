%{
open Ast

let make_binary_op op left right =
  Call(Var(op), [left; right])

let make_unary_op op arg =
  Call(Var(op), [arg])

let default_if_expr = Bool true

let rec var_item_to_expr item =
  match item with
  | VarIdent name -> Var name
  | VarItem (base, index) ->
      Call(Var("__getitem__"), [var_item_to_expr base; index])

let col_of_letters s =
  let len = String.length s in
  if len = 0 then invalid_arg "empty column";
  let col = ref 0 in
  for i = 0 to len - 1 do
    let c = s.[i] in
    if c < 'A' || c > 'Z' then invalid_arg ("invalid column: " ^ s);
    col := (!col * 26) + (Char.code c - Char.code 'A' + 1)
  done;
  !col
%}

/* Tokens for values */
%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <bool> BOOL
%token <string> STRING
%token <string> IDENT
%token <string> SHEETNAME
%token TILDE
%token <int * int> CELL
%token NONE

/* Layout */
%token NEWLINE INDENT DEDENT

/* Keywords */
%token IF THEN ELSE
%token LET IN
%token WHILE FOR
%token DEF EXTEND EXTENDED RETURN
%token TRY CATCH
%token NEWTYPE DEL RAISE
%token BREAK CONTINUE
%token LAMBDA
%token AND_KW OR_KW NOT

/* Punctuation */
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LBRACE RBRACE
%token COMMA COLON BLOCKCOLON EQUALS
%token WALRUS
%token LBRACKETPIPE PIPERBRACKET

/* Operators */
%token <string> OP_ADD
%token <string> OP_MUL
%token <string> OP_POW
%token <string> OP_ATCARET
%token <string> OP_BIT
%token <string> OP_COMP
%token <string> UOP
%token <string> OPEQ

%token UPLUS

%token EOF

/* Precedence and associativity */
%left OR_KW
%left AND_KW
%left OP_COMP
%left IN
%left OP_BIT
%right OP_ATCARET
%left OP_ADD
%left OP_MUL
%right OP_POW
%right NOT UOP UPLUS

%start program

%type <Ast.def_expr> program

%%

program:
  | opt_newlines stmt_list opt_newlines EOF { Sequence $2 }

opt_newlines:
  | /* empty */ { () }
  | NEWLINE opt_newlines { () }

stmt_list:
  | /* empty */ { [] }
  | stmt stmt_list { $1 :: $2 }

one_or_more_newlines:
  | NEWLINE { () }
  | NEWLINE one_or_more_newlines { () }

stmt:
  | simple_stmt one_or_more_newlines { $1 }
  | compound_stmt { $1 }

simple_stmt:
  | assignment { $1 }
  | return_stmt { $1 }
  | newtype_stmt { $1 }
  | del_stmt { $1 }
  | raise_stmt { $1 }
  | break_stmt { $1 }
  | continue_stmt { $1 }

compound_stmt:
  | if_stmt { $1 }
  | while_stmt { $1 }
  | for_stmt { $1 }
  | def_stmt { $1 }
  | extend_stmt { $1 }
  | extended_stmt { $1 }
  | try_stmt { $1 }

assignment:
  | var_item EQUALS value_expr
      { Assign($1, $3) }
  | var_item OPEQ value_expr
    { Assign($1, make_binary_op $2 (var_item_to_expr $1) $3) }

var_item:
  | IDENT { VarIdent $1 }
  | var_item LBRACKET value_expr RBRACKET { VarItem($1, $3) }

if_stmt:
  | IF value_expr BLOCKCOLON block else_block
      { If($2, $4, $5) }

else_block:
  | ELSE BLOCKCOLON block { $3 }
  | /* empty */ { Sequence [] }

while_stmt:
  | WHILE value_expr BLOCKCOLON block
      { While($2, $4) }

for_stmt:
  | FOR IDENT IN value_expr BLOCKCOLON block
      { For($2, $4, $6) }

def_stmt:
  | DEF def_name LPAREN args RPAREN BLOCKCOLON block
      { Def($2, $4, $7) }
  | DEF def_name LPAREN RPAREN BLOCKCOLON block
      { Def($2, [], $6) }

extend_stmt:
  | EXTEND def_name LPAREN args RPAREN BLOCKCOLON block
    { Extend($2, $4, $7) }
  | EXTEND def_name LPAREN RPAREN BLOCKCOLON block
    { Extend($2, [], $6) }

extended_stmt:
  | EXTENDED def_name LPAREN args RPAREN BLOCKCOLON block
    { Extended($2, $4, $7) }
  | EXTENDED def_name LPAREN RPAREN BLOCKCOLON block
    { Extended($2, [], $6) }

def_name:
  | IDENT { $1 }
  | OP_ADD { $1 }
  | OP_MUL { $1 }
  | OP_POW { $1 }
  | OP_ATCARET { $1 }
  | OP_BIT { $1 }
  | OP_COMP { $1 }
  | UOP { $1 }

args:
  | IDENT { [$1] }
  | IDENT COMMA args { $1 :: $3 }

try_stmt:
  | TRY BLOCKCOLON block CATCH IDENT BLOCKCOLON block
      { TryCatch($3, $5, $7) }
block:
  | NEWLINE INDENT opt_newlines stmt_list opt_newlines DEDENT
      { Sequence $4 }

return_stmt:
  | RETURN value_expr
      { Return $2 }

newtype_stmt:
  | NEWTYPE IDENT COMMA IDENT newtype_tail
      { $5 $2 $4 }

newtype_tail:
  | COMMA IDENT { fun type_name ctor_name -> NewType(type_name, ctor_name, $2) }
  | /* empty */ { fun type_name ctor_name -> NewTypeSingleton(type_name, ctor_name) }

del_stmt:
  | DEL var_item
      { Del $2 }

raise_stmt:
  | RAISE value_expr
      { Raise $2 }

break_stmt:
  | BREAK { Break }

continue_stmt:
  | CONTINUE { Continue }

/* Value expressions */
value_expr:
  | or_expr { $1 }

or_expr:
  | and_expr { $1 }
  | or_expr OR_KW and_expr { Or($1, $3) }

and_expr:
  | comp_expr { $1 }
  | and_expr AND_KW comp_expr { And($1, $3) }

comp_expr:
  | in_expr { $1 }
  | in_expr OP_COMP in_expr { make_binary_op $2 $1 $3 }

in_expr:
  | bit_expr { $1 }
  | IDENT COLON CELL { CellAddressList(("", (col_of_letters $1, snd $3)), ("",$3)) }
  | IDENT COLON IDENT { IsOfType(Var $1, $3) }
  | bit_expr COLON IDENT { IsOfType($1, $3) }
  | bit_expr IN bit_expr { make_binary_op "__in__" $1 $3 }

bit_expr:
  | atcaret_expr { $1 }
  | bit_expr OP_BIT atcaret_expr { make_binary_op $2 $1 $3 }

atcaret_expr:
  | add_expr { $1 }
  | add_expr OP_ATCARET atcaret_expr { make_binary_op $2 $1 $3 }

add_expr:
  | mul_expr { $1 }
  | add_expr OP_ADD mul_expr { make_binary_op $2 $1 $3 }

mul_expr:
  | pow_expr { $1 }
  | mul_expr OP_MUL pow_expr { make_binary_op $2 $1 $3 }

pow_expr:
  | unary_expr { $1 }
  | unary_expr OP_POW pow_expr { make_binary_op $2 $1 $3 }

unary_expr:
  | postfix_expr { $1 }
  | NOT unary_expr { Not $2 }
  | UOP unary_expr { make_unary_op $1 $2 }
  | OP_ADD unary_expr %prec UPLUS {
      if $1 = "+" then make_unary_op "__pos__" $2
      else if $1 = "-" then make_unary_op "__neg__" $2
      else raise (Invalid_argument "invalid unary operator")
    }

postfix_expr:
  | primary_expr { $1 }
  | postfix_expr LBRACKET value_expr RBRACKET { Call(Var "__getitem__", [$1; $3]) }
  | IDENT LPAREN RPAREN { Call(Var $1, []) }
  | IDENT LPAREN call_args RPAREN { Call(Var $1, $3) }
  | postfix_expr LPAREN RPAREN { Call($1, []) }
  | postfix_expr LPAREN call_args RPAREN { Call($1, $3) }

call_args:
  | value_expr { [$1] }
  | value_expr COMMA call_args { $1 :: $3 }

primary_expr:
  | INT { Int $1 }
  | FLOAT { Float $1 }
  | CHAR { Char $1 }
  | BOOL { Bool $1 }
  | NONE { NNone }
  | IDENT { Var $1 }
  | STRING { List (List.init (String.length $1) (fun i -> Char $1.[i])) }
  | LPAREN value_expr RPAREN { $2 }
  | list_expr { $1 }
  | matrix_expr { $1 }
  | brace_expr { $1 }
  | if_expr { $1 }
  | let_expr { $1 }
  | lambda_expr { $1 }
  | cell_addr { CellAddress $1 }
  | cell_addr COLON cell_addr { CellAddressMatrix($1, $3) }
  | cell_addr COLON INT {
      let (sheet, (col, _row1)) = $1 in
      CellAddressList($1, (sheet, (col, $3)))
    }
  | col_ref COLON cell_addr {
      let (sheet_l, col1) = $1 in
      let (sheet_r, (col2, row)) = $3 in
      let sheet = if sheet_l = "" then sheet_r else sheet_l in
      if sheet_r <> "" && sheet_l <> "" && sheet_r <> sheet_l then
        raise (Invalid_argument "sheet mismatch in range");
      CellAddressList((sheet, (col1, row)), (sheet, (col2, row)))
    }

list_expr:
  | LBRACKET RBRACKET { List [] }
  | LBRACKET list_comp RBRACKET { $2 }
  | LBRACKET list_contents RBRACKET { List $2 }

list_comp:
  | value_expr FOR IDENT IN value_expr comp_if_opt
      { ListComprehension($1, $3, $5, $6) }

list_contents:
  | value_expr { [$1] }
  | value_expr COMMA list_contents { $1 :: $3 }

matrix_expr:
  | LBRACKETPIPE PIPERBRACKET { Matrix [] }
  | LBRACKETPIPE matrix_comp PIPERBRACKET { $2 }
  | LBRACKETPIPE matrix_contents PIPERBRACKET { Matrix $2 }

matrix_comp:
  | value_expr FOR IDENT IN value_expr comp_if_opt
      { MatrixComprehension($1, $3, $5, $6) }

matrix_contents:
  | value_expr { [$1] }
  | value_expr COMMA matrix_contents { $1 :: $3 }

brace_expr:
  | LBRACE RBRACE { Dict [] }
  | LBRACE dict_comp RBRACE { $2 }
  | LBRACE dict_contents RBRACE { Dict $2 }
  | LBRACE set_comp RBRACE { $2 }
  | LBRACE set_contents RBRACE { SSet $2 }

dict_comp:
  | value_expr WALRUS value_expr FOR IDENT IN value_expr comp_if_opt
      { DictComprehension($1, $3, $5, $7, $8) }

dict_contents:
  | dict_entry { [$1] }
  | dict_entry COMMA dict_contents { $1 :: $3 }

dict_entry:
  | IDENT EQUALS value_expr { (Var $1, $3) }
  | value_expr WALRUS value_expr { ($1, $3) }

set_comp:
  | value_expr FOR IDENT IN value_expr comp_if_opt
      { SSetComprehension($1, $3, $5, $6) }

set_contents:
  | value_expr { [$1] }
  | value_expr COMMA set_contents { $1 :: $3 }

comp_if_opt:
  | IF value_expr { $2 }
  | /* empty */ { default_if_expr }

if_expr:
  | IF value_expr THEN value_expr ELSE value_expr
      { If($2, $4, $6) }

let_expr:
  | LET IDENT EQUALS value_expr IN value_expr
      { Let($2, $4, $6) }

lambda_expr:
  | LAMBDA lambda_args COLON value_expr
      { Lambda($2, $4) }

lambda_args:
  | IDENT { [$1] }
  | IDENT COMMA lambda_args { $1 :: $3 }

cell_addr:
  | CELL { ("", $1) }
  | IDENT TILDE CELL { ($1, $3) }
  | SHEETNAME TILDE CELL { ($1, $3) }

col_ref:
  | IDENT { ("", col_of_letters $1) }
  | IDENT TILDE IDENT { ($1, col_of_letters $3) }
  | SHEETNAME TILDE IDENT { ($1, col_of_letters $3) }
