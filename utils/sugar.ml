open Microkanren
open Types
open Lazy

let delay (g: goal Lazy.t): goal = 
  fun st ->
    Immature (fun () -> (Lazy.force g) st)