open Types

let mzero: stream = [];;
let unit (st: state) = st::mzero;;

let var_eq (v1: var) (v2: var): bool = v1 = v2;;
  
let rec walk (u: term) (s: substitution): term = 
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

let ext_s (x: var) (v: term) (s: substitution): substitution = (x, v) :: s;;

let rec unify (u: term) (v: term) (s: substitution option): substitution option = 
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

let eqeq (u: term) (v: term): state -> stream = 
  fun (s, c) ->
    let s = unify u v (Some s) in 
    match s with
    | None -> mzero
    | Some s -> unit (s, c)

let call_fresh (f: term -> goal): goal = 
  (* Consider cleaning up this notation? *)
  fun (s, c) ->
    f (Var c) (s, c + 1);;
  
let rec mplus (s1: stream) (s2: stream): stream = 
  match s1 with
  | [] -> s2
  | hd::tl -> hd :: (mplus tl s2)
and bind (s: stream) (g: goal): stream =
  match s with
  | [] -> mzero
  | hd::tl -> mplus (g hd) (bind tl g);;

let disj (g1: goal) (g2: goal): goal =
  fun st -> mplus (g1 st) (g2 st);;
let conj (g1: goal) (g2: goal): goal = 
  fun st -> bind (g1 st) g2;;