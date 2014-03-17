(* Test Puzzle Functions 2  *)

#use "puzzle.ml";;

let a = [0;1;2;3];;
let b = [1;2;0;3];;
let c = [0;1;2;3;4;5;6;7;8];;
let d = [1;2;3;4;0;5;6;7;8];;
let e = [1;2;3;4;5;0;6;7;8];;

(* Test move_pos b *)
prt_int_list (move_pos a) ;;
prt_int_list (move_pos b) ;;
prt_int_list (move_pos c) ;;
prt_int_list (move_pos d) ;;
prt_int_list (move_pos e) ;;

(* Test make_move (b,x) *)
prt_int_list (make_move (a,1)) ;;
prt_int_list (make_move (a,2)) ;;
prt_int_list (make_move (b,0)) ;;
prt_int_list (make_move (b,3)) ;;
prt_int_list (make_move (c,1)) ;;
prt_int_list (make_move (c,3)) ;;
prt_int_list (make_move (d,1)) ;;
prt_int_list (make_move (d,3)) ;;
prt_int_list (make_move (d,5)) ;;
prt_int_list (make_move (d,7)) ;;
prt_int_list (make_move (e,5)) ;;
prt_int_list (make_move (e,8)) ;;

(* Test make_moves b *)
prt_int_list_list (make_moves a) ;;
prt_int_list_list (make_moves b) ;;
prt_int_list_list (make_moves c) ;;
prt_int_list_list (make_moves d) ;;
prt_int_list_list (make_moves e) ;;

