open Microkanren
open Types
open Lazy


let delay (g: 'a goal Lazy.t): 'a goal = 
  fun st ->
    Immature (fun () -> (Lazy.force g) st)

(* converts an int into a term *)
let int_t (z : int): 'a term =
  Val (Int z);;

(* converts a term list into a term *)
let list_t (l: 'a term list): 'a term = 
  Val (List l);;