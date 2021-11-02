open Microkanren
open Sugar
open Types

let empty_state = ([], 0);;

let a_and_b =
  conj
    (call_fresh (
      fun a -> eqeq a (Val 7)
    ))
    (call_fresh
      (fun b ->
        disj
         (eqeq b (Val 5)) 
         (eqeq b (Val 6))
      )
    );;

let rec fives x =
  disj
    (eqeq x (Val 5))
    (fun (s, c) ->
      Immature (fun () ->
        (fives x) (s, c)));;
let rec sixes x =
  disj
    (eqeq x (Val 6))
    (delay (lazy (sixes x)));;
let fives_and_sixes =
  call_fresh (
    fun x ->
      disj
        (fives x)
        (sixes x)
  );;

let lists = call_fresh
  (fun p ->
    call_fresh 
      (fun q -> 
        let l1 = List [p; Val 7] in
        let l2 = List [(Val 9); (Val 7)] in
        conj
          (eqeq q (Val 5))
          (eqeq l1 l2)
      )
    );;
