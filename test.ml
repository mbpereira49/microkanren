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
print_string (format_stream res 3);;

let rec fives x =
  disj
    (eqeq x (Val 5))
    (fun (s, c) ->
      Immature (fun () ->
        (fives x) (s, c)))
in
let rec sixes x =
  disj
    (eqeq x (Val 6))
    (fun (s, c) ->
      Immature (fun () ->
        (sixes x) (s, c)))
in
let fives_and_sixes =
  call_fresh (
    fun x ->
      disj
        (fives x)
        (sixes x)
  )
in
let res = fives_and_sixes empty_state in
print_string(format_stream res 5);;
