open Ast
open Lexer
open Parser
open Eval


let repl = 
    print_string "\n********** Welcoml to my Interpreter! **********\n";
    let rec loop db = (
        try (
            let lexbuf = Lexing.from_channel stdin
            in (
                print_string "> "; flush stdout
            );
            let dec = clause (
                fun lb -> (
                    match Lexer.token lb with
                    | r -> r
                )
            ) lexbuf
            in let newdb = eval_dec (dec, db)
            in loop newdb 
        )
        with
        | Failure s -> (
            print_newline();
            print_endline s;
            print_newline();
            loop db
        )
        | Parser.Error -> (
            print_string "\n*****Parsing Error*****\n";
            loop db
        )
        | _ -> (
            print_string "\n*****Unrecognized Error*****\n";
        )
    )
    in (loop [])

