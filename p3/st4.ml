#use "testUtils.ml";;
					
let rec len x = match x with
	[] -> 0
	| h::t -> 1 + len(t);;
					
let prt_int x = print_int x;;

let rec find (x,v) = match x with 
		[] ->  false
		| h::t -> if h = v then true
				else find(t,v);;
				
				
let rec get_val (x, n) 	= match x with
[] -> -1
| h::t -> if n = 0 then h
			else get_val (t,n-1);;
			

let rec in_bound (x,y) = if (x=[] && not(y=[])) then false
						else match y with
						[] -> true
						| h::t -> if  (get_val(x,h) = -1) then false
									else in_bound (x,t);;

let rec get_vals (x,y) = 
			if (not(in_bound (x,y))) then []
			else match y with
				[] -> []
				| h::t -> get_val(x,h)::(get_vals(x,t));;

let rec index (x,v) = 
	if (find(x,v) = false) then -1
	else match x with
		[] -> -1
	| h::t -> if h = v then 0
				else 1+index(t,v);;
				
let find_board_size b = int_of_float(sqrt(float(len b)));;

let pos_of_xy (x,y,s) = if (x<0 || y<0 || x > s-1 || y > s-1 ) then -1
						else (x*s)+y;;

let xy_of_pos (p, s) = ( int_of_float (floor(float(p)/.float(s))) , int_of_float((mod_float (float_of_int(p)) (float_of_int(s)))) );;


let xy_zero b = xy_of_pos(index (b,0) ,find_board_size (b));;


let adjacents (c,s) = match c with
	(x,y) -> pos_of_xy(x-1,y,s)::(pos_of_xy(x,y-1,s)::(pos_of_xy(x,y+1,s)::(pos_of_xy(x+1,y,s))::[]));;

let rec filter_negative x = match x with
		[] -> []
		| (h::t) -> if h = -1 then filter_negative(t)
				else h::(filter_negative(t));;

let move_pos b = filter_negative(adjacents(xy_zero(b),find_board_size(b)));;

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

let rec list_swap_val (b, u, v) = match b with
				[] -> []
				| h::t -> if h = u then v::list_swap_val(t, u, v)
						else if h = v then u::list_swap_val(t, u, v)
						else h::list_swap_val(t, u, v);;

let make_move (b,x) = list_swap_val (b, 0, get_val(b,x));;

let rec mov (l,b) = match l with
	[] -> [];
	| (h::t) -> make_move (b, h) :: mov (t,b);;
	
let make_moves b = mov(move_pos(b),b);;


print_string "\n2ND MOVE \n";;
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

print_string "\nLAST\n";;

prt_int_list_list (make_moves a) ;;
prt_int_list_list (make_moves b) ;;
prt_int_list_list (make_moves c) ;;
prt_int_list_list (make_moves d) ;;
prt_int_list_list (make_moves e) ;;