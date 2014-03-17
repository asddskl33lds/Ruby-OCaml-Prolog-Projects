(* Test Recursion 2  *)

#use "puzzle.ml";;

let x = [5;6;7;3] ;;
let y = [5;6;7;5] ;;
let z = [7;5;6;5] ;;
let a = [3;5;8;9] ;;

(* Test index (x, v)	*)
prt_int (index (x, 5)) ;;
prt_int (index (x, 6)) ;;
prt_int (index (x, 7)) ;;
prt_int (index (x, 3)) ;;
prt_int (index (z, 5)) ;;

(* Test uniq x	*)
prt_int_list (List.sort compare (uniq x)) ;; 
prt_int_list (List.sort compare (uniq y)) ;; 
prt_int_list (List.sort compare (uniq z)) ;; 
prt_int_list_list (List.sort compare (uniq [x;y;z])) ;; 
prt_int_list_list (List.sort compare (uniq [x;y;y])) ;; 
prt_int_list_list (List.sort compare (uniq [x;y;x])) ;; 

(* Test find_new (x, y) *)
prt_int_list (find_new (x,[3]));;
prt_int_list (find_new (x,[3;5]));;
prt_int_list (find_new (x,[3;6]));;
prt_int_list_list (find_new ([x;y;z],[y]));;
prt_int_list_list (find_new ([x;y;z],[y;x]));;
prt_int_list_list (find_new ([x;y;z],[x]));;

(* Test is_sorted x *)
prt_bool (is_sorted x) ;;
prt_bool (is_sorted y) ;;
prt_bool (is_sorted z) ;;
prt_bool (is_sorted a) ;;

