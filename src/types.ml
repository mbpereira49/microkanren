type counter = int;;
type var = int;;
type 'a value = 
  | Int of int
  | List of 'a term list
and
'a term =
  | Empty
  | Var of var
  | Val of 'a value
  | List of 'a term list;;
type 'a substitution = (var * 'a term) list;;

type 'a state = 'a substitution * counter;;

type 'a stream = 
  | Empty
  | Immature of (unit -> 'a stream)
  | Mature of 'a state * 'a stream;;

type 'a goal = 'a state -> 'a stream;;