open Ast


let reset = 
    let next = ref 0 in
    let f () = (next := ! next + 1; string_of_int (!next)) in
    let r = next := 0 in
    (f, r)


let rec find_vars query = 
    match query with
    | [] -> []
    | (x :: xs) -> (
            match x with
            | VarExp v -> x :: (find_vars xs)
            | ConstExp c -> (find_vars xs)
            | TermExp (sym, exp) -> (find_vars exp) @ (find_vars xs)
    )


let uniq l =
    let rec tail_uniq a l =
        match l with
        | [] -> a
        | hd :: tl ->
            tail_uniq (hd :: a) (List.filter (fun x -> (x <> hd) ) tl) in
            tail_uniq [] l
    

let add_db (dec, db) = dec :: db


let rec sub_lift_goal sub goal = 
    match goal with 
    | VarExp v -> (
        try let i = List.assoc goal sub in i
        with Not_found -> VarExp v
    )
    | TermExp (s, el) -> 
            TermExp (s, List.map (fun goal -> sub_lift_goal sub goal) el)
    | _ -> goal


let sub_lift_goals sub goal = 
    List.map (fun goal -> sub_lift_goal sub goal) goal


let rec replace c sub = 
    match c with
    | [] -> []
    | ((s, t):: xs) -> 
            (sub_lift_goal sub s, sub_lift_goal sub t) :: (replace xs sub)

let rec occur_check n t =
    match t with 
    | VarExp m -> n = m
    | TermExp (st, el) ->
        List.fold_left (fun acc v -> acc || (occur_check n v)) false el
    | _ -> false

let rec pairandcat sargs targs c =
    match sargs with
    | [] -> (
        if (List.length targs = 0)
        then c
        else raise (Failure "sargs and targs should be the same length")
    )
    | (s :: ss) -> (
        match targs with
        | (t :: ts) -> pairandcat ss ts ((s, t) :: c)
        |  _ -> raise (Failure "sargs and targs should be the same length")
    )

let rec unify constraints =
    match constraints with
    | [] -> Some []
    | ((s, t) :: cdr) ->
            (* (c) *)
            if s = t
            then unify cdr
            else (
                match s with
                | VarExp(n) -> 
                        (* (e) *)
                        if (occur_check n t)
                        then None
                        else let sub = [(s,t)] in
                        let cdr' = replace cdr sub in
                        let phi = unify cdr' in (
                            match phi with
                            | None -> None
                            | Some l -> Some ((s, sub_lift_goal l t) :: l)
                        )
                | TermExp (sname, sargs) -> (
                    match t with
                    (* (d) *)
                    | VarExp k -> unify ((t, s) :: cdr)
                    | TermExp (tname, targs) -> 
                            (* (b) *)
                            if (tname = sname && List.length targs = List.length sargs)
                            then unify (pairandcat sargs targs cdr)
                            else None 
                    | _ -> None 
                )
                | _ -> (
                    match t with
                    (* (d) *)
                    | VarExp k -> unify ((t, s) :: cdr)
                    | _ -> None
                )
            )


let rec eval_query (query, db, subs) =
    match query with
    | [] -> ([subs])
    | (goal::goals) ->
            match goal with
                | TermExp("true", []) -> (
                    eval_query (goals, db, subs)
                )
                | TermExp(_,_) -> (
                    let onstep _ clause = match clause with
                    | Clause (head, body) -> (
                        match unify [(goal, head)] with
                        | Some subs2 -> (
                            (eval_query ((sub_lift_goals subs2 body)@(sub_lift_goals subs2 goals), db, subs2))
                        )
                        | _ -> ([])
                    )
                    | _ -> ([])
                    in List.fold_left onstep [] db
                )
                | _ -> eval_query (goals, db, subs)


let string_of_result result vars = "success\n"

let eval_dec (dec, db) =
    match dec with
    | Clause (head, body) -> add_db (dec, db)
    | Query body -> (
        let result = eval_query (body, db, []) in
        let vars = uniq (find_vars body) in
        print_string (string_of_result result vars);
        db
    )

let eval = eval_dec 
