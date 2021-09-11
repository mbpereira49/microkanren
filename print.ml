open Types
open Printf

let format_var (v: var): string = 
  sprintf "#(%d)" v;;

let format_list (l: 'a list) (f: ('a -> string)) (sep: string): string =
  let string_l = List.map f l in
  String.concat sep string_l

let rec format_term (t: term): string = 
  match t with
  | Empty -> ""
  | Var v -> format_var v
  | Val v -> sprintf "%d" v
  | List t -> sprintf "(%s)" (format_list t format_term " ");; 

let format_substitution (s: substitution) = 
  let format_mapping (v, t) = sprintf "%s -> %s" (format_var v) (format_term t) in 
  sprintf "[%s]" (format_list s format_mapping ", ");;

let format_state ((s, c): state): string =
  sprintf "(%s, %d)" (format_substitution s) c;;

let format_stream (str: stream): string =
  sprintf "[%s]\n" (format_list str format_state ", ");;