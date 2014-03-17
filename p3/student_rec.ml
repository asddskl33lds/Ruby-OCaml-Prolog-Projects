let rec print_list x = match x with
					[] -> print_string "]\n"
					| h::t -> print_int h ; print_string ";" ; print_list t;;
					
let rec fold (f, a, l) = match l with
    [] -> a
  | (h::t) -> fold (f, f (a, h), t)
;;

let rec find (x,v) = match x with 
		[] ->  false
		| h::t -> if h = v then true
				else find(t,v);;
				
let rec get_val (x, n) 	= match x with
[] -> -1
| h::t -> if n = 0 then h
			else get_val (t,n-1);;
			

let length x = fold ((fun (a,y) -> a+1), 0, x)
;;

print_string "\nGET VAL\n6 -> ";;
print_int (get_val([5;6;7;3],1));;
print_string "\n-1 -> ";;
print_int (get_val([5;6;7;3],9));;
print_string "\n5 -> ";;
print_int (get_val([5],0));;
print_string "\n-1 -> ";;
print_int (get_val([],0));;


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
	

print_string "\n\nGET_VALS\n[7;5] -> [";;

if (not(in_bound([5;6;7;3],[2;0]))) then
	print_string "\nALERT : IN_BOUND IS WRONG\n";;
	
print_list (get_vals ([5;6;7;3],[2;0]));;	

print_string "[3] -> [";;
print_list (get_vals ([4;2;5;3],[3]));;	

print_string "[2;5] -> [";;
print_list (get_vals ([4;2;5;3],[1;2]));;	

print_string "[] -> [";;
print_list (get_vals ([],[2;0]));;	

print_string "[] -> [";;
print_list (get_vals ([4;2;5;3],[]));;	

if (in_bound([4],[0;6])) then
	print_string "\nALERT : IN_BOUND IS WRONG\n";;
	
print_string "[] -> [";;
print_list (get_vals ([4],[0;6]));;	

let rec set_n (x, n, v) = 
		if ( n <((length x)-1) ) then x 
		else match x with
			[] -> []
			| (h::t) -> if n = 0 then v::t
			else h::set_n (t,n-1,v);;
	


print_string "\nSET_n\n[5;9;7;3] -> [";;
print_list (set_n ([5;6;7;3],1,9));;

print_string "[5;6;7;3] -> [";;
print_list (set_n ([5;6;7;3],8,9));;

print_string "[9] -> [";;
print_list (set_n ([5],0,9));;

print_string "[] -> [";;
print_list (set_n ([],1,9));;

print_string "[1;2;3] -> [";;
print_list (set_n ([1;2;3],3,9));;


let rec list_swap_val (b, u, v) = match b with
				[] -> []
				| h::t -> if h = u then v::list_swap_val(t, u, v)
						else if h = v then u::list_swap_val(t, u, v)
						else h::list_swap_val(t, u, v);;
				
print_string "\n\nSWAP_VAL\n[7;6;5;3] -> [";;
print_list (list_swap_val ([5;6;7;3],7,5));;

print_string "[7;6;9;3] -> [";;
print_list (list_swap_val ([5;6;9;3],7,5));;

print_string "[2;6;5;3] -> [";;
print_list (list_swap_val ([2;6;7;3],7,5));;

print_string "[1;2;3;4] -> [";;
print_list (list_swap_val ([1;2;3;4],7,5));;

print_string "[1] -> [";;
print_list (list_swap_val ([8],1,8));;

print_string "[] -> [";;
print_list (list_swap_val ([],7,5));;



let rec index (x,v) = 
	if (find(x,v) = false) then -1
	else match x with
		[] -> -1
	| h::t -> if h = v then 0
				else 1+index(t,v);;
				
				
print_string "\n\nINDEX\n2 -> ";;
print_int (index ([5;6;7;3],7));;
print_string "\n-1 -> ";;
print_int (index([6;3;1;3;2],4));;
print_string "\n2 -> ";;
print_int (index([5;5;6;6;7],6));;
print_string "\n0 -> ";;
print_int (index([5],5));;
					

let rec uniq x = match x with
	[] -> []
	| h::t -> if find(t,h) then uniq t
				else h::uniq(t);;
				
(*
					
let rec uniq_aux (x,v) = match x with
							| [] -> []
							| h :: t -> if v = h then uniq_aux(t,v)
										else h::(uniq_aux (t,v));;
let rec uniq x = match x with
						 [] -> []
						| h::t -> h::(uniq_aux ((uniq t),h));;
 *)
print_string "\n\nUNIQUE\n[6;5;3] -> [";;
print_list (uniq[5;6;5;3]);;
print_string "[6] -> [";;
print_list (uniq[6;6;6;6]);;

print_string "[1;2;3;4] -> [";;
print_list (uniq[1;2;3;4]);;
print_string "[5;3;4;6] -> [";;
print_list (uniq[4;5;3;4;6]);;



  
  
let rec find_new (x,y) = match x with 
		[] -> []
		| h::t -> if find(y,h) then find_new(t,y)
				else
					h::find_new(t,y);;
					
					
print_string "\n\nFIND_NEW\n[4;7] -> [";;
print_list (find_new ([4;3;7],[5;6;5;3]));;

print_string "[] -> [";;
print_list (find_new ([4;3;7],[5;4;3;7]));;

print_string "[4;3;7] -> [";;
print_list (find_new ([4;3;7],[5;6;5;9]));;

print_string "[] -> [";;
print_list (find_new ([4],[5;4;3;7]));;


print_string "[4;3;7] -> [";;
print_list (find_new ([4;3;7],[]));;

print_string "[] -> [";;
print_list (find_new ([],[5;6;5;3]));;




let rec is_sorted x = match x with
					[] -> true
					|[x] -> true
					| (h::(t::z)) -> if h > t then false
									else is_sorted (t::z);;

print_string "\nSorted\n[5;5;7;9] Yes -> ";;
if (is_sorted ([5;5;7;9])) then print_string "Yes\n"
else print_string "\nNo\n";;


print_string "[5;6;7] Yes -> ";;
if (is_sorted ([5;5;5])) then print_string "Yes\n"
else print_string "\nNo\n";;

print_string "[7;8;7] No -> ";;
if (is_sorted ([7;8;1])) then print_string "Yes\n"
else print_string "No\n";;

print_string "[7] Yes -> ";;
if (is_sorted ([7])) then print_string "Yes\n"
else print_string "No\n";;

print_string "[] Yes -> ";;
if (is_sorted ([])) then print_string "Yes\n"
else print_string "No\n";;

print_string "\nTHE END"
	



