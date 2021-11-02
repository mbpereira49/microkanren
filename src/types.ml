type counter = int;;
type var = int;;
type value = 
  | Int of int
  | List of term list
and
term =
  | Empty
  | Var of var
  | Val of value
  | List of term list;;
type substitution = (var * term) list;;

type state = substitution * counter;;

type stream = 
  | Empty
  | Immature of (unit -> stream)
  | Mature of state * stream;;

type goal = state -> stream;;