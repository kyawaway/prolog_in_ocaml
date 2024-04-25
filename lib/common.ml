open Ast
open Parser


let parse s =
    let lexbuf = Lexing.from_string s in
        let ast = clause Lexer.token lexbuf in
            ast

let try_parse s =
    try Some (parse s) with
    | Error -> None

let string_of_token t =
    match t with
    | STRING s -> "STRING \"" ^ String.escaped s ^ "\""
    | ATOM   a -> "ATOM \""   ^ String.escaped a ^ "\""
    | VAR    v -> "VAR \""    ^ v ^ "\""
    | RULE     -> "RULE"
    | QUERY    -> "QUERY"
    | PERIOD   -> "PERIOD"
    | LPAREN   -> "LPAREN"
    | RPAREN   -> "RPAREN"
    | COMMA    -> "COMMA"

let string_of_token_list tl =
    "[" ^ (String.concat "; " (List.map string_of_token tl)) ^ "]"

let string_of_const c =
    match c with
    | String s -> "StringConst \"" ^ String.escaped s ^ "\""

let rec string_of_exp e =
    match e with
    | VarExp v   -> "VarExp \"" ^ v ^ "\""
    | ConstExp c -> "ConstExp (" ^ (string_of_const c) ^ ")"
    | TermExp (f, args) ->
        let func = String.escaped f in
            "TermExp (\"" ^ func ^ "\", [" ^
                (String.concat "; " (List.map string_of_exp args)) ^ "])"


let string_of_exp_list g =
    "[" ^ (String.concat "; " (List.map string_of_exp g)) ^ "]"

let string_of_dec d =
    match d with
    | Clause (e1, g) ->
        "Clause (" ^ (string_of_exp e1) ^ ", " ^
            (string_of_exp_list g) ^ ")"
    | Query g -> "Query (" ^ (string_of_exp_list g) ^ ")"

let string_of_db db =
    "[" ^ (String.concat "; " (List.map string_of_dec db)) ^ "]"


let string_of_subs s =
    "[" ^ (
        String.concat "; " (
            List.map (
                fun (x,y) ->
                    "(" ^ (string_of_exp x) ^ ", " ^ (string_of_exp y) ^ ")"
            )
            s
        )
    ) ^ "]"

let string_of_unify_res s =
    match s with
    | None   -> "None"
    | Some l -> string_of_subs l

let readable_string_of_const c =
    match c with
    | String s -> "\"" ^ String.escaped s ^ "\""

let rec readable_string_of_exp e =
    match e with
    | VarExp   v     -> v
    | ConstExp c     -> readable_string_of_const c
    | TermExp (s, l) ->
        s ^ (
            if List.length l > 0
            then "(" ^ (
                String.concat ", " (
                    List.map readable_string_of_exp l
                )
            ) ^ ")"
            else ""
        )

let print_db db =
    print_endline (string_of_db db)
