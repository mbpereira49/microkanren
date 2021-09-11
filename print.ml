open Types
open Printf

let print_var (v: var): string = 
  sprintf "#(%d)" v;;

let print_list (l: 'a list) (f: ('a -> string)) (sep: string): string =
  let string_l = List.map f l in
  String.concat sep string_l

let rec print_term (t: term): string = 
  match t with
  | Empty -> ""
  | Var v -> print_var v
  | Val v -> sprintf "%d" v
  | List t -> sprintf "(%s)" (print_list t print_term " ");; 

let print_substitution (s: substitution) = 
  let format_mapping (v, t) = sprintf "%s -> %s" (print_var v) (print_term t) in 
  Printf.sprintf "[%s]" (print_list s format_mapping ", ");;

let print_state ((s, c): state): string =
  Printf.sprintf "(%s, %d)" (print_substitution s) c;;

let print_stream (str: stream) =
  let repr = Printf.sprintf "[%s]\n" (print_list str print_state ", ") in
  print_string repr;;