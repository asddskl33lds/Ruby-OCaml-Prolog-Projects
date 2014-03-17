(* Test recursion  1 *)

#use "puzzle.ml";;

let x = [5;6;7;3] ;;

(* Test get_val (x, n)	*)
prt_int (get_val(x,0));;
prt_int (get_val(x,1));;
prt_int (get_val(x,2));;
prt_int (get_val(x,3));;

(* Test get_vals (x, y)	*)
prt_int_list (get_vals(x,[0]));;
prt_int_list (get_vals(x,[1]));;
prt_int_list (get_vals(x,[3]));;
prt_int_list (get_vals(x,[0;2]));;
prt_int_list (get_vals(x,[0;2;1]));;
prt_int_list (get_vals(x,[0;2;1;3]));;

(* Test set_n (x, n, v) *)
prt_int_list (set_n (x,0,9));;
prt_int_list (set_n (x,1,9));;
prt_int_list (set_n (x,2,9));;
prt_int_list (set_n (x,3,9));;

(* Test list_swap_val (b, u, v)	*)
prt_int_list (list_swap_val (x,7,5));;
prt_int_list (list_swap_val (x,6,5));;
prt_int_list (list_swap_val (x,3,5));;
prt_int_list (list_swap_val (x,7,3));;

