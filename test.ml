open Microkanren
open Types
open Print

let empty_state = ([], 0);;
let goal = call_fresh
  (fun p ->
    call_fresh 
      (fun q -> 
        let l1 = List [p; Val 7] in
        let l2 = List [(Val 9); (Val 7)] in
        conj
          (eqeq q (Val 5))
          (eqeq l1 l2)
      )
    )
in
let res = goal empty_state in
print_string (format_stream res);;