let rec len x = match x with
	[] -> 0
	| h::t -> 1 + len(t);;

let rec prt_int_list x = match x with
					[] -> print_string "]\n"
					| h::t -> print_int h ; print_string ";" ; prt_int_list t;;

let prt_int x = print_int x;;

let find_board_size b = int_of_float(sqrt(float(len b)));;

let a = [0;1;2;3] ;;
let b = [1;2;0;3] ;;
let c = [0;1;2;3;4;5;6;7;8] ;;
let d = [1;2;3;4;0;5;6;7;8] ;;
let e = [1;2;3;4;5;0;6;7;8] ;;

(* Test find_board_size b *)
print_int (find_board_size a) ;;
print_int (find_board_size b) ;;
print_int (find_board_size c) ;;
print_int (find_board_size d) ;;
print_int (find_board_size e) ;;

print_string "\n POS_OF_XY\n";;

let pos_of_xy (x,y,s) = if (x<0 || y<0 || x > s-1 || y > s-1 ) then -1 
						else x*s+y ;;
						
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
prt_int (pos_of_xy (-1,9,10)) ;;
prt_int (pos_of_xy (8,-1,10)) ;;
prt_int (pos_of_xy (10,2,10)) ;;
prt_int (pos_of_xy (9,9,10)) ;;
print_string "\nXY OF POS\n";;


let xy_of_pos (p, s) = ( int_of_float (floor(float(p)/.float(s))) , int_of_float((mod_float (float_of_int(p)) (float_of_int(s)))) );;
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

