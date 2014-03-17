(* Test Puzzle Functions 1  *)

#use "puzzle.ml";;

let a = [0;1;2;3] ;;
let b = [1;2;0;3] ;;
let c = [0;1;2;3;4;5;6;7;8] ;;
let d = [1;2;3;4;0;5;6;7;8] ;;
let e = [1;2;3;4;5;0;6;7;8] ;;

(* Test find_board_size b *)
prt_int (find_board_size a) ;;
prt_int (find_board_size b) ;;
prt_int (find_board_size c) ;;
prt_int (find_board_size d) ;;
prt_int (find_board_size e) ;;

(* Test pos_of_xy (x, y, s) *)
prt_int (pos_of_xy (0,0,2)) ;;
prt_int (pos_of_xy (0,1,2)) ;;
prt_int (pos_of_xy (1,0,2)) ;;
prt_int (pos_of_xy (1,1,2)) ;;
prt_int (pos_of_xy (0,0,3)) ;;
prt_int (pos_of_xy (0,1,3)) ;;
prt_int (pos_of_xy (0,2,3)) ;;
prt_int (pos_of_xy (1,0,3)) ;;
prt_int (pos_of_xy (2,0,3)) ;;
prt_int (pos_of_xy (1,2,10)) ;;
prt_int (pos_of_xy (8,9,10)) ;;

(* Test xy_of_pos (p, s) *)
let (x,y) = xy_of_pos (0, 2) in prt_int_list [x;y] ;;
let (x,y) = xy_of_pos (1, 2) in prt_int_list [x;y] ;;
let (x,y) = xy_of_pos (2, 2) in prt_int_list [x;y] ;;
let (x,y) = xy_of_pos (3, 2) in prt_int_list [x;y] ;;
let (x,y) = xy_of_pos (0, 3) in prt_int_list [x;y] ;;
let (x,y) = xy_of_pos (1, 3) in prt_int_list [x;y] ;;
let (x,y) = xy_of_pos (2, 3) in prt_int_list [x;y] ;;
let (x,y) = xy_of_pos (3, 3) in prt_int_list [x;y] ;;
let (x,y) = xy_of_pos (4, 3) in prt_int_list [x;y] ;;
let (x,y) = xy_of_pos (5, 3) in prt_int_list [x;y] ;;
let (x,y) = xy_of_pos (48, 10) in prt_int_list [x;y] ;;
let (x,y) = xy_of_pos (49, 10) in prt_int_list [x;y] ;;
let (x,y) = xy_of_pos (50, 10) in prt_int_list [x;y] ;;
