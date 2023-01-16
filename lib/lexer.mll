(* Lexer *)

{
  open Parser
}
(** util **)
(*** character ***)

let lower = ['a' - 'z']
let upper = ['A' - 'Z']
let digit = ['0' - '9']
let space = [' ' '\t']
let underscore = '_'

let end_line = '\n'
let atom_quote = '''
let string_quote = '"'

let alpha = lower | upper
let alphanum = alpha | underscore | digit

let digits = digit +

(** atom **)

let atom = lower alphanum *


(** variable **)

let variable = upper alphanum *

(** **)

rule token = parse
    (*** escape ***)
    | [' ' '\t' '\n'] { token lexbuf }

    (*** symbols ***)
    | "("
        { LPAREN }
    | ")"
        { RPAREN }
    | ","
        { COMMA }
    | "."
        { PERIOD }
    | ":-"
        { RULE }
    | "?-"
        { QUERY }

    (*** atom ***)
    | atom as a
        { ATOM a }
    | atom_quote
        { atoms "" lexbuf }

    (*** strings ***)
    | string_quote
        { strings "" lexbuf}

    (*** variables ***)
    | variable as v
        { VAR v }

and atoms acc = parse 
    | atom_quote
        { ATOM acc }

and strings acc = parse
    | string_quote space * string_quote
        { STRING acc }

