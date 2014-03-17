(* Test Higher Order Functions 1  *)

#use "puzzle.ml";;

let x = [5;6;7;3] ;;
let y = [5;6;7;5] ;;
let z = [7;5;6;5] ;;
let a = [3;5;8;9] ;;

(* Test grow_lists (x, y)	*)
prt_int_list_list (grow_lists ([1],[3])) ;; 
prt_int_list_list (grow_lists ([1],x)) ;; 
prt_int_list_list (grow_lists ([1;2],x)) ;; 
prt_int_list_list (grow_lists (x,y)) ;; 
prt_int_list_list (grow_lists (x,z)) ;; 
prt_int_list_list (grow_lists (a,z)) ;; 
List.map prt_int_list_list (grow_lists ([a],[z])) ;; 
List.map prt_int_list_list (grow_lists ([a],[z;x])) ;; 
List.map prt_int_list_list (grow_lists ([a;y],[z])) ;; 
List.map prt_int_list_list (grow_lists ([a;y],[z;x;a])) ;; 

(* Test concat_lists *)
prt_int_list (concat_lists [a;z]) ;; 
prt_int_list (concat_lists [x;a;z]) ;; 
prt_int_list (concat_lists [y;a;a;z]) ;; 
prt_int_list_list (concat_lists [[a];[z]]) ;; 
prt_int_list_list (concat_lists [[a;y];[z;x]]) ;; 
prt_int_list_list (concat_lists [[a;y];[a];[z;x]]) ;; 
