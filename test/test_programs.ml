open Microkanren
open Sugar
open Types

let empty_state = ([], 0);;

let a_and_b =
  conj
    (call_fresh (
      fun a -> eq a (int_t 7)
    ))
    (call_fresh
      (fun b ->
        disj
         (eq b (int_t 5)) 
         (eq b (int_t 6))
      )
    );;

let rec fives x =
  disj
    (eq x (int_t 5))
    (fun (s, c) ->
      Immature (fun () ->
        (fives x) (s, c)));;
let rec sixes x =
  disj
    (eq x (int_t 6))
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
        let l1 = List [p; int_t 7] in
        let l2 = List [(int_t 9); (int_t 7)] in
        conj
          (eq q (int_t 5))
          (eq l1 l2)
      )
    );;

let rec appendo =
  fun l s out ->
    disj
      (conj (eq l (Val (List []))) (eq s out))
      (call_fresh
        (fun a ->
          call_fresh (
            fun d ->
              conj
                (eq (List [a; d]) l)
                (call_fresh
                  (fun res ->
                    conj
                      (eq (List [a; res]) out)
                      (fun (st, c) ->
                        Immature (fun () -> (appendo d s res) (st, c))
                      )
                  )
                )
          )
        )
      );;
  
let call_appendo = 
  call_fresh (
    fun q ->
      call_fresh (
        fun l ->
          call_fresh (
            fun s -> 
              call_fresh (
                fun out ->
                  (appendo l s out)
              )
          )
      )
  )

(* let ground_appendo = appendo (list_t [int_t 1]) (list_t [int_t 2]) (list_t [int_t 1; int_t 2]);; *)