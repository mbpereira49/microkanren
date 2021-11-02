open Test_programs
open Print;;

let computations = 
[
    format_stream (a_and_b empty_state) 2;
    format_stream (fives_and_sixes empty_state) 5;
    format_stream (lists empty_state) 3;
    format_stream (call_appendo empty_state) 5;
] in
print_string (format_list computations (fun x -> x) "\n");;