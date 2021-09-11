open Types
open Printf

let print_var (v: var): string = 
  Printf.sprintf "#(%d)" v;;

let rec print_term_list (t: term list): string =
  match t with
  | [] -> ""
  | hd::tl -> 
      let hd_string = print_term hd in
      let tl_string = print_term_list tl in
      Printf.sprintf "%s %s" hd_string tl_string
and print_term (t: term): string =
  match t with
  | Empty -> ""
  | Var v -> print_var v
  | Val v -> Printf.sprintf "%d" v
  | List t -> Printf.sprintf "(%s)" (print_term_list t);;

let rec print_substitution_helper (s: substitution) = 
  match s with
  | [] -> ""
  | (v, t)::tl -> Printf.sprintf "%s -> %s, %s" (print_var v) (print_term t) (print_substitution_helper tl);;

let print_substitution (s: substitution) = 
  Printf.sprintf "[%s]" (print_substitution_helper s);;

let print_state ((s, c): state): string =
  Printf.sprintf "(%s, %d)" (print_substitution s) c;;

let rec print_stream_helper (str: stream): string =
  match str with
  | [] -> ""
  | st::tl -> Printf.sprintf "%s, %s" (print_state st) (print_stream_helper tl);;

let print_stream (str: stream) =
  let repr = Printf.sprintf "[%s]\n" (print_stream_helper str) in
  print_string repr;;