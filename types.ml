type counter = int;;
type var = int;;
type term_single = 
  | Var of var
  | Val of int;;
type term_any =
  | Empty
  | Single of term_single
  | List of term_any list;;
type substitution = (var * term_any) list;;

(*Consider making state a record*)
type state = substitution * counter;;
type stream = state list;;

type goal = state -> stream;;