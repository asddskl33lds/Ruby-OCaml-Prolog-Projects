(* Test Puzzle Solver 1  *)

#use "puzzle.ml";;

let a = [1;0;2;3];;
let b = [1;3;2;0];;
let c = [1;0;2;3;4;5;6;7;8];;
let d = [1;2;0;3;4;5;6;7;8];;
let e = [3;1;2;6;4;5;7;8;0];;

(* Test solve_board (b,n) *)

(* print_endline ("solve_board (a,3)") ;; *)
List.map prt_int_list_list (solve_board (a,3)) ;;

(* print_endline ("solve_board (b,3)") ;; *)
List.map prt_int_list_list (solve_board (b,3)) ;;

(* print_endline ("solve_board (d,4)") ;; *)
List.map prt_int_list_list (solve_board (d,4)) ;;

(* print_endline ("solve_board (e,4)") ;; *)
List.map prt_int_list_list (solve_board (e,4)) ;;

(* print_endline ("solve_board (c,2)") ;; *)
List.map prt_int_list_list (solve_board (c,2)) ;;

(* print_endline ("solve_board (c,12)") ;; *)
List.map prt_int_list_list (List.sort compare (solve_board (c,12))) ;;

(* List.map print_boards (solve_board (c,12)) ;; *)
