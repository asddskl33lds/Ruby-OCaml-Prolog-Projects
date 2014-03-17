(* test parsing string to RE *)
#use "nfa.ml"

let test_parser str =
    try 
        let r = (Nfa.string_to_regexp str) in
        print_endline (Nfa.regexp_to_string r)
    with Nfa.IllegalExpression s -> 
        print_endline ("IllegalExpression " ^ str);;

test_parser "ab" ;;
test_parser "c|d" ;;
test_parser "e*" ;;
test_parser "f|" ;;
test_parser "E*" ;;

print_string "\nAlex Examples below\n";;

test_parser "a|E" ;;
test_parser "(a|b)" ;;
test_parser "(a)" ;;
test_parser "((a))" ;;
test_parser "(a|(bc))" ;;

test_parser "E|E" ;;
test_parser "E*" ;;
test_parser "a*" ;;
test_parser "(a*)|(b*)" ;;
test_parser "EE" ;;
test_parser "E" ;;

print_string "\n\nMore Examples below\n";;
test_parser "EEE" ;;
test_parser "(E" ;;
test_parser "E|" ;;
test_parser "" ;;

test_parser "(EEE)|EEEE*E*E" ;;
test_parser "(a*)*" ;;
test_parser "E|" ;;
test_parser "" ;;




