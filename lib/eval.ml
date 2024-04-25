open Ast
open Common

let reset =
  let next = ref 0 in
  let r = next := 0 in
  r

let fresh =
  let next = ref 0 in
  let f () =
    next := !next + 1;
    string_of_int !next
  in
  f

let rec find_vars query =
  match query with
  | [] -> []
  | x :: xs -> (
      match x with
      | VarExp v -> x :: find_vars xs
      | ConstExp c -> find_vars xs
      | TermExp (sym, exp) -> find_vars exp @ find_vars xs)

let uniq l =
  let rec tail_uniq a l =
    match l with
    | [] -> a
    | hd :: tl -> tail_uniq (hd :: a) (List.filter (fun x -> x <> hd) tl)
  in
  tail_uniq [] l

let add_db (dec, db) = dec :: db

let rec sub_lift_goal sub goal =
  match goal with
  | VarExp v -> (
      try
        let i = List.assoc goal sub in
        i
      with Not_found -> VarExp v)
  | TermExp (s, el) ->
      TermExp (s, List.map (fun goal -> sub_lift_goal sub goal) el)
  | _ -> goal

let sub_lift_goals sub goal = List.map (fun goal -> sub_lift_goal sub goal) goal

let rec replace c sub =
  match c with
  | [] -> []
  | (s, t) :: xs -> (sub_lift_goal sub s, sub_lift_goal sub t) :: replace xs sub

let rec occur_check n t =
  match t with
  | VarExp m -> n = m
  | TermExp (st, el) ->
      List.fold_left (fun acc v -> acc || occur_check n v) false el
  | _ -> false

let rec pairandcat sargs targs c =
  match sargs with
  | [] ->
      if List.length targs = 0 then c
      else raise (Failure "sargs and targs should be the same length")
  | s :: ss -> (
      match targs with
      | t :: ts -> pairandcat ss ts ((s, t) :: c)
      | _ -> raise (Failure "sargs and targs should be the same length"))

let rec unify constraints =
  match constraints with
  | [] -> Some []
  | (s, t) :: cdr -> (
      if (* (c) *)
         s = t then unify cdr
      else
        match s with
        | VarExp n -> (
            if (* (e) *)
               occur_check n t then None
            else
              let sub = [ (s, t) ] in
              let cdr' = replace cdr sub in
              let phi = unify cdr' in
              match phi with
              | None -> None
              | Some l -> Some ((s, sub_lift_goal l t) :: l))
        | TermExp (sname, sargs) -> (
            match t with
            (* (d) *)
            | VarExp k -> unify ((t, s) :: cdr)
            | TermExp (tname, targs) ->
                (* (b) *)
                if tname = sname && List.length targs = List.length sargs then
                  unify (pairandcat sargs targs cdr)
                else None
            | _ -> None)
        | _ -> (
            match t with
            (* (d) *)
            | VarExp k -> unify ((t, s) :: cdr)
            | _ -> None))

let rec find_vars_string q =
  match q with
  | [] -> []
  | x :: xs -> (
      match x with
      | VarExp v -> v :: find_vars_string xs
      | ConstExp c -> find_vars_string xs
      | TermExp (s, el) -> find_vars_string el @ find_vars_string xs)

let rec rename_vars_in_dec d =
  match d with
  | Clause (h, b) ->
      let head_vars = find_vars [ h ] in
      let body_vars = find_vars b in
      let vars = uniq (head_vars @ body_vars) in
      let sub = List.map (fun x -> (x, VarExp (fresh ()))) vars in
      Clause (sub_lift_goal sub h, sub_lift_goals sub b)
  | Query b ->
      let body_vars = find_vars b in
      let vars = uniq body_vars in
      let sub = List.map (fun x -> (x, VarExp (fresh ()))) vars in
      Query (sub_lift_goals sub b)

let rec eval_query (query, db, subs) =
  match query with
  | [] -> [ subs ]
  | goal :: goals -> (
      let string_vars_list = find_vars_string query |> uniq in
      let subs =
        List.filter
          (fun (v, _) ->
            match v with
            | VarExp elt ->
                List.exists (fun a -> String.equal elt a) string_vars_list
            | _ -> false)
          subs
      in
      match goal with
      | TermExp ("true", []) -> eval_query (goals, db, subs)
      | TermExp (_, _) ->
          (* let onstep _ clause =
                   match clause with
                   | Clause (head, body) -> (
                       match unify [ (goal, head) ] with
                       | Some subs2 ->
                           eval_query
                             ( sub_lift_goals subs2 body @ sub_lift_goals subs2 goals,
                               db,
                               subs2 )
                       | _ -> [])
                   | _ -> []
                 in
                 List.fold_left onstep [] db
             | _ -> eval_query (goals, db, subs))
          *)
          List.fold_right
            (fun rule r ->
              match rule with
              | Clause (h, b) -> (
                  match unify [ (goal, h) ] with
                  | Some s -> (
                      match unify (s @ subs) with
                      | Some env2 ->
                          if List.length b = 1 then
                            match b with
                            | TermExp ("true", _) :: ys ->
                                eval_query (sub_lift_goals s goals, db, env2)
                                @ r
                            | _ ->
                                eval_query
                                  ( sub_lift_goals s b @ sub_lift_goals s goals,
                                    db,
                                    env2 )
                                @ r
                          else
                            eval_query
                              ( sub_lift_goals s b @ sub_lift_goals s goals,
                                db,
                                env2 )
                            @ r
                      | _ -> r)
                  | _ -> r)
              | _ -> r)
            db []
      | _ -> eval_query (goals, db, subs))

let string_of_result e vars vars_num =
  List.fold_left
    (fun res subs ->
      if vars_num > 0 then
        List.fold_left
          (fun r d ->
            match d with
            | VarExp v -> (
                try
                  let f = List.assoc (VarExp v) subs in
                  match f with
                  | VarExp _ -> (v ^ " is free\n") ^ r
                  | _ -> (v ^ " = " ^ readable_string_of_exp f ^ "\n") ^ r
                with Not_found -> (v ^ " is free\n") ^ r)
            | _ -> r)
          "" vars
        ^ res
      else "" ^ res)
    (if List.length e > 0 then "true\n" else "false\n")
    e

let eval_dec (dec, db) =
  match dec with
  | Clause (head, body) -> add_db (dec, db)
  | Query body ->
      let result = eval_query (body, db, []) in
      let vars = uniq (find_vars body) in
      let vars_num = List.length vars in
      print_string (string_of_result result vars vars_num);
      db

let eval = eval_dec
