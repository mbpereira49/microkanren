open Microkanren
open Types
open Print

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