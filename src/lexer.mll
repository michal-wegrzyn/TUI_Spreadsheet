{
open Parser

exception Lexing_error of string

let unescape s =
  let buf = Buffer.create (String.length s) in
  let rec process i =
    if i >= String.length s then Buffer.contents buf
    else if s.[i] = '\\' && i + 1 < String.length s then
      begin
        match s.[i + 1] with
        | 'n' -> Buffer.add_char buf '\n'; process (i + 2)
        | '\\' -> Buffer.add_char buf '\\'; process (i + 2)
        | '"' -> Buffer.add_char buf '"'; process (i + 2)
        | 't' -> raise (Lexing_error "\\t escape is not supported")
        | c -> Buffer.add_char buf c; process (i + 2)
      end
    else begin
      Buffer.add_char buf s.[i];
      process (i + 1)
    end
  in
  process 0

let indent_stack : int list ref = ref [0]
let pending : Parser.token list ref = ref []
let at_bol = ref true
let paren_level = ref 0

let is_comp_op s =
  s = "<=" || s = ">=" || s = "==" || s = "!="

let ends_with s ch =
  let n = String.length s in
  n > 0 && s.[n - 1] = ch

let strip_last s =
  String.sub s 0 (String.length s - 1)

let classify_operator op =
  if op = "" then raise (Lexing_error "empty operator");
  if ends_with op '=' && not (is_comp_op op) then (
    if op = "=" then EQUALS
    else
      let base = strip_last op in
      if base = "" then raise (Lexing_error "invalid operator assignment")
      else OPEQ base
  ) else if is_comp_op op then OP_COMP op
  else if op.[0] = '!' then (
    UOP op
  ) else if String.length op >= 2 && op.[0] = '*' && op.[1] = '*' then OP_POW op
  else
    match op.[0] with
    | '*' | '/' | '%' -> OP_MUL op
    | '+' | '-' -> OP_ADD op
    | '@' | '^' -> OP_ATCARET op
    | '&' | '|' | '?' -> OP_BIT op
    | '<' | '>' -> OP_COMP op
    | '=' -> OP_COMP op
    | '!' -> UOP op
    | _ -> raise (Lexing_error ("Invalid operator: " ^ op))

let col_of_letters s =
  let len = String.length s in
  if len = 0 then raise (Lexing_error "Empty column")
  else
    let col = ref 0 in
    for i = 0 to len - 1 do
      let c = s.[i] in
      if c < 'A' || c > 'Z' then raise (Lexing_error ("Invalid column: " ^ s));
      col := (!col * 26) + (Char.code c - Char.code 'A' + 1)
    done;
    !col

let cell_of_lexeme s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n && s.[!i] >= 'A' && s.[!i] <= 'Z' do
    incr i
  done;
  let col_str = String.sub s 0 !i in
  let row_str = String.sub s !i (n - !i) in
  (col_of_letters col_str, int_of_string row_str)

let rec pop_to_indent target =
  match !indent_stack with
  | [] -> raise (Lexing_error "internal: empty indent stack")
  | cur :: rest ->
    if cur = target then ()
    else (
      indent_stack := rest;
      pending := DEDENT :: !pending;
      pop_to_indent target
    )

let count_dedents_to_zero () =
  let rec count = function
    | [] -> 0
    | [0] -> 0
    | _ :: rest -> 1 + count rest
  in
  count !indent_stack

let reset_lexer_state () =
  indent_stack := [0];
  pending := [];
  at_bol := true;
  paren_level := 0
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let underscore = '_'
let newline = '\n'

let op_char = ['+' '-' '*' '/' '%' '^' '<' '>' '=' '!' '&' '|' '@' '?']

let int_literal = digit+
let float_literal = digit+ '.' digit+
let identifier = (letter | underscore) (letter | digit | underscore)*
let cell_col = ['A'-'Z']+
let cell_row = digit+

let sheet_char = [^ '`' '\n']

let ws = [' ' '\t']+

rule token = parse
  | ""
      {
        match !pending with
        | tok :: rest -> pending := rest; tok
        | [] ->
          if !at_bol && !paren_level = 0 then indent lexbuf
          else (
            if !at_bol && !paren_level > 0 then at_bol := false;
            token_main lexbuf
          )
      }

and indent = parse
  | eof
      { token_main lexbuf }
  | (' ')* '\t'+
      { raise (Lexing_error "Tabs are not allowed for indentation") }
  | (' ')* as _spaces "#="
      { skip_comment lexbuf; at_bol := true; token lexbuf }
  | (' ')* as _spaces '#' [^ '\n']* newline
    { Lexing.new_line lexbuf; at_bol := true; token lexbuf }
  | (' ')* as _spaces newline
    { Lexing.new_line lexbuf; at_bol := true; token lexbuf }
  | (' ')* as spaces
      {
        let indent = String.length spaces in
        let cur = List.hd !indent_stack in
        if indent = cur then (at_bol := false; token_main lexbuf)
        else if indent > cur then (
          if indent <> cur + 4 then raise (Lexing_error "Indentation must increase by 4 spaces");
          indent_stack := indent :: !indent_stack;
          at_bol := false;
          INDENT
        ) else (
          if not (List.mem indent !indent_stack) then
            raise (Lexing_error "Unindent does not match any outer indentation level");
          pop_to_indent indent;
          at_bol := false;
          match !pending with
          | tok :: rest -> pending := rest; tok
          | [] -> token_main lexbuf
        )
      }

and token_main = parse
  (* End of file: emit pending DEDENTs first *)
  | eof
      {
        let dedents = count_dedents_to_zero () in
        if dedents > 0 then (
          let rec push = function
            | 0 -> ()
            | n -> pending := DEDENT :: !pending; push (n - 1)
          in
          push dedents;
          indent_stack := [0]
        );
        if (not !at_bol) && !paren_level = 0 then (
          at_bol := true;
          pending := !pending @ [EOF];
          NEWLINE
        ) else (
          match !pending with
          | tok :: rest -> pending := rest; tok
          | [] -> EOF
        )
      }

  (* Newlines: emit NEWLINE only when not inside (), [], {} *)
  | newline
      {
        Lexing.new_line lexbuf;
        at_bol := true;
        if !paren_level > 0 then token lexbuf else NEWLINE
      }

  | ws { token lexbuf }

  | '#' [^ '\n']* newline
      { Lexing.new_line lexbuf; at_bol := true; if !paren_level > 0 then token lexbuf else NEWLINE }
  | "#="
      { skip_comment lexbuf; token lexbuf }
  
  | ":="      { WALRUS }
  
  | "True"    { BOOL true }
  | "False"   { BOOL false }
  | "None"    { NONE }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "let"     { LET }
  | "in"      { IN }
  | "while"   { WHILE }
  | "for"     { FOR }
  | "def"     { DEF }
  | "extend"  { EXTEND }
  | "extended" { EXTENDED }
  | "return"  { RETURN }
  | "try"     { TRY }
  | "catch"   { CATCH }
  | "newtype" { NEWTYPE }
  | "del"     { DEL }
  | "raise"   { RAISE }
  | "break"   { BREAK }
  | "continue" { CONTINUE }
  | "lambda"  { LAMBDA }
  | "and"     { AND_KW }
  | "or"      { OR_KW }
  | "not"     { NOT }
  
  | "[|"      { paren_level := !paren_level + 1; LBRACKETPIPE }
  | "|]"      { paren_level := max 0 (!paren_level - 1); PIPERBRACKET }
  | '('       { paren_level := !paren_level + 1; LPAREN }
  | ')'       { paren_level := max 0 (!paren_level - 1); RPAREN }
  | '['       { paren_level := !paren_level + 1; LBRACKET }
  | ']'       { paren_level := max 0 (!paren_level - 1); RBRACKET }
  | '{'       { paren_level := !paren_level + 1; LBRACE }
  | '}'       { paren_level := max 0 (!paren_level - 1); RBRACE }
  | ','       { COMMA }
  | ':' [' ' '\t']* newline
      {
        Lexing.new_line lexbuf;
        at_bol := true;
        pending := NEWLINE :: !pending;
        BLOCKCOLON
      }
  | ':'       { COLON }
  | '='       { EQUALS }
  | '~'       { TILDE }
  
  (* String literals *)
  | '"' ([^ '"' '\\'] | '\\' _)* '"' as str
      {
        let content = String.sub str 1 (String.length str - 2) in
        let unescaped = unescape content in
        STRING unescaped
      }
  
  (* Character literal *)
  | '\'' ([^ '\'' '\\'] | '\\' _) '\'' as ch
      {
        let content = String.sub ch 1 (String.length ch - 2) in
        let unescaped = unescape content in
        if String.length unescaped = 1 then
          CHAR unescaped.[0]
        else
          raise (Lexing_error ("Invalid character literal: " ^ ch))
      }
  
  | float_literal as f
      { FLOAT (float_of_string f) }
  
  | int_literal as i
      { INT (int_of_string i) }

  (* Backticked sheet name *)
  | '`' sheet_char+ '`' as s
      {
        let name = String.sub s 1 (String.length s - 2) in
        SHEETNAME name
      }

  | cell_col cell_row as a
      { CELL (cell_of_lexeme a) }
  

  | op_char+ as op
      { classify_operator op }
  
  | identifier as id
      { IDENT id }
  
    | _
      { raise (Lexing_error ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }

and skip_comment = parse
  | "=#"    { () }
  | newline { Lexing.new_line lexbuf; at_bol := true; skip_comment lexbuf }
  | _       { skip_comment lexbuf }
  | eof     { raise (Lexing_error "Unterminated comment") }

and skip_line_comment = parse
  | [^ '\n']* newline { Lexing.new_line lexbuf; at_bol := true }
  | eof { () }

