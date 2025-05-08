open Base

type status = Sat | Unsat | Unit of Lit.t | Unresolved

let status (nu : Eval.t) (c : Clause.t) : status =
  match
    List.partition3_map (Set.to_list c) ~f:(fun l ->
        match Eval.lit nu l with
        | Some false -> (* false *) `Fst l
        | None -> (* unassigned *) `Snd l
        | Some true -> (* true *) `Trd l)
  with
  | _, _, _ :: _ -> Sat
  | _, [], [] -> Unsat
  | _, [x], [] -> Unit x
  | _, _, [] -> Unresolved

type result = Unsat of Clause.t | Unknown | Sat

let rec run (level : int) (a : Assign.t) (cs : Clause.t list) : result * Assign.t =
  match Utils.partition4_map cs ~f:(fun c ->
      match status a.nu c with
      | Sat -> `P1 c
      | Unsat -> `P2 c
      | Unit l -> `P3 (c, l)
      | Unresolved -> `P4 c)
  with
  | _, unsat::_, _, _ -> (Unsat unsat, a)
  | _, [], [], [] -> (Sat, a)
  | _, [], (c, l)::units, rest ->
      let a' = Assign.assign_implied a level c l in
      run level a' (List.append (List.map units ~f:fst) rest)
  | _, [], [], _::_ -> (Unknown, a)
