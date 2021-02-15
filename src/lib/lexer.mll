{
  module L = Lexing

  type token = [%import: Parser.token] [@@deriving show]

  let illegal_character loc char =
    Error.error loc "illegal character '%c'" char
}

let spaces = [' ' '\t']+
let digit = ['0'-'9']
let integer = digit+
<<<<<<< HEAD
let letter = ['a'-'z' 'A'-'Z']
let underline = '_'
let identifier = letter (letter | digit | underline)*
=======
let id = ['a'-'z' 'A'-'Z']['0'-'9' 'a'-'z' 'A'-'Z' '_']*
(* add other definitions, if needed *)
>>>>>>> add: tokens

rule token = parse
  | spaces            { token lexbuf }
  | '\n'              { L.new_line lexbuf; token lexbuf }
  | integer as lxm    { LITINT (int_of_string lxm) }
  | '+'               { PLUS }
<<<<<<< HEAD
  | '<'               { LT }
  | '='               { EQ }
  | ','               { COMMA }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | "int"             { INT }
  | "bool"            { BOOL }
  | "if"              { IF }
  | "then"            { THEN }
  | "else"            { ELSE }
  | "let"             { LET }
  | "in"              { IN }
  | identifier as lxm { ID (Symbol.symbol lxm) }
=======
  | "int"             { INT }
  | "bool"            { BOOL }
  | "let"             { LET }
  | "in"              { IN }
  | "if"              { IF }
  | "then"            { THEN }
  | "else"            { ELSE }          
  | '<'               { LT }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | ','               { COMMA }
  | '='               { EQ }
  | id as lxm         { ID (Symbol.symbol lxm)}

  (* add other lexical rules *)

>>>>>>> add: tokens
  | eof               { EOF }
  | _                 { illegal_character (Location.curr_loc lexbuf) (L.lexeme_char lexbuf 0) }
