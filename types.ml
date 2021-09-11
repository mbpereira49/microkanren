type counter = int;;
type var = int;;
type term =
  | Empty
  | Var of var
  | Val of int
  | List of term list;;
type substitution = (var * term) list;;

(*Consider making state a record*)
type state = substitution * counter;;
type stream = state list;;

type goal = state -> stream;;