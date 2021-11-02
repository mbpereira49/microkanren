open Types

let mzero: 'a stream = Empty;;
let unit (st: 'a state) = Mature (st, mzero);;

let var_eq (v1: var) (v2: var): bool = v1 = v2;;
  
let rec walk (u: 'a term) (s: 'a substitution): 'a term = 
  match u with
  | Empty -> Empty
  | Val _ -> u
  | Var v ->
      let present = List.find_opt (fun (v1, v2) -> v = v1) s in
      (match present with
      | None -> u
      | Some (v1, v2) -> walk v2 s)
  | List l -> match l with
    | [] -> Empty
    | _ -> u;;

let ext_s (x: var) (v: 'a term) (s: 'a substitution): 'a substitution = (x, v) :: s;;

let rec unify (u: 'a term) (v: 'a term) (s: 'a substitution option): 'a substitution option = 
  match s with
  | None -> None
  | Some s -> 
      let u = walk u s in 
      let v = walk v s in 
      match u, v with
      | Empty, _ | _, Empty -> if u = v then Some s else None
      | List (hdu::tlu), List (hdv::tlv) ->
          let s = unify hdu hdv (Some s) in
          unify (List tlu) (List tlv) s
      | Var u, Var v -> Some (if var_eq u v then s else ext_s u (Var v) s)
      | Var u, _ -> Some (ext_s u v s)
      | _, Var v -> Some (ext_s v u s)
      | _ -> if u = v then Some s else None;;

let eq (u: 'a term) (v: 'a term): 'a state -> 'a stream = 
  fun (s, c) ->
    let s = unify u v (Some s) in 
    match s with
    | None -> Empty
    | Some s -> unit (s, c)

let call_fresh (f: 'a term -> 'a goal): 'a goal = 
  fun (s, c) ->
    f (Var c) (s, c + 1);;
  
let rec mplus (s1: 'a stream) (s2: 'a stream): 'a stream = 
  match s1 with
  | Empty -> s2
  | Immature f -> Immature (fun () -> mplus s2 (f ()))
  | Mature (st, str) -> Mature (st, mplus str s2);;

let rec bind (s: 'a stream) (g: 'a goal): 'a stream =
  match s with
  | Empty -> Empty
  | Immature f -> Immature (fun () -> bind (f ()) g)
  | Mature (st, str) -> mplus (g st) (bind str g);;

let disj (g1: 'a goal) (g2: 'a goal): 'a goal =
  fun st -> mplus (g1 st) (g2 st);;
let conj (g1: 'a goal) (g2: 'a goal): 'a goal = 
  fun st -> bind (g1 st) g2;;