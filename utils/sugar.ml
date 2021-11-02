open Microkanren
open Types
open Lazy


let delay (g: 'a goal Lazy.t): 'a goal = 
  fun st ->
    Immature (fun () -> (Lazy.force g) st)