open Types
open Print

let mzero: stream = [];;
let unit (st: state) = st::mzero;;

let var_eq (v1: var) (v2: var): bool = v1 = v2;;

let rec walk_single (u: term_single) (s: substitution): term_any = 
  match u with
  | Val _ -> Single u
  | Var v ->
      let present = List.find_opt (fun (v1, v2) -> v = v1) s in
      match present with
      | None -> Single u
      | Some (v1, v2) -> walk v2 s
and walk (u: term_any) (s: substitution): term_any = 
  match u with
  | Empty -> Empty
  | Single t -> walk_single t s
  | List l -> match l with
      | [] -> Empty
      | _ -> u;;

let ext_s (x: var) (v: term_any) (s: substitution): substitution = (x, v) :: s;;

let rec unify (u: term_any) (v: term_any) (s: substitution option): substitution option = 
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
      | Single (Var u), Single (Var v) -> Some (if var_eq u v then s else ext_s u (Single (Var v)) s)
      | Single (Var u), _ -> Some (ext_s u v s)
      | _, Single (Var v) -> Some (ext_s v u s)
      | _ -> if u = v then Some s else None;;

let eqeq (u: term_any) (v: term_any): state -> stream = 
  fun (s, c) ->
    let s = unify u v (Some s) in 
    match s with
    | None -> mzero
    | Some s -> unit (s, c)

let call_fresh (f: term_any -> goal): goal = 
  (* Consider cleaning up this notation? *)
  fun (s, c) ->
    f (Single (Var c)) (s, c + 1);;
  
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

let empty_state = ([], 0);;
let res =
  let goal = call_fresh
    (fun p ->
      call_fresh (fun q -> 
        let l1 = List [p; Single (Val 7)] in
        let l2 = List [Single (Val 9); Single (Val 7)] in
        (conj
          (eqeq q (Single (Val 5)))
          (eqeq l1 l2)
        )
        ))
  in
  goal empty_state;;
  

print_stream res;;