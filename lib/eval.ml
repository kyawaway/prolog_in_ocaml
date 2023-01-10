open Ast

let eval = eval_dec 


let eval_dec (dec, db) =
    match dec with
    | Clause (h, b) -> add_db (dec, db)
    | Query b -> (
        let result = eval_query (b, db, []) in
            print_string ( result_to_string (result));

        reset ();
        db
    )


let add_db (dec, db) = 
    match dec with 
    | Clause (h, b) -> (
        match h with
        | TermExp ("true", _) -> 
            print_string "Error\n"; db
        | _ -> dec :: db
    )
    | Query (b) -> (
        dec :: db
    )


let rec eval_query (questoion, db, env) = match question with
      [] -> [env]
    | (goal :: goals) -> (
        match goals with 
        | TermExp ("true", []) -> (
            eval_query (goals, dv, env)
        )
        | TermExp (_,_) -> (
        )
        | _ -> eval_query (goals, dv, env)


let unify = ()


let rec occur n t =
    match t with 
    | VarExp m -> n = m
    | TermExp (st, el) ->
        List.fold_left (fun acc v -> acc || (occur n v)) false el
    | _ -> false


let rename_vars d = 
    match d with 
    | Clause (h, b) -> ()
    | Query (b) -> ()



let string_of_result = ()


