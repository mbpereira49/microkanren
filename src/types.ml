type counter = int;;
type var = int;;
type term =
  | Empty
  | Var of var
  | Val of int
  | List of term list;;
type substitution = (var * term) list;;

type state = substitution * counter;;

type stream = 
  | Empty
  | Immature of (unit -> stream)
  | Mature of state * stream;;

type goal = state -> stream;;