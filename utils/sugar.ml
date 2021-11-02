open Microkanren
open Types
open Lazy


let delay (g: goal Lazy.t): goal = 
  fun st ->
    Immature (fun () -> (Lazy.force g) st)

(* converts an int into a term *)
let int_t (z : int): term =
  Val (Int z);;

(* converts a term list into a term *)
let list_t (l: term list): term = 
  Val (List l);;