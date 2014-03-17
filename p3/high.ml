#use "testUtils.ml";;

					
let rec fold (f, a, l) = match l with
    [] -> a
  | (h::t) -> fold (f, f (a, h), t)
;;

let rec map (f, l) = match l with
    [] -> []
  | (h::t) -> (f h)::(map (f, t))
;;
					
					
let rec append (lis, e) = match lis with
				[] -> e
				| (h::t) -> h::(append (t, e));;


let concat_lists x = fold (append, [], x) ;;

let grow_lists (x, y) =  let pre h  = h :: y in 
							map(pre,x);;

let x = [5;6;7;3] ;;
let y = [5;6;7;5] ;;
let z = [7;5;6;5] ;;
let a = [3;5;8;9] ;;

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

prt_int_list (concat_lists [a;z]) ;; 
prt_int_list (concat_lists [x;a;z]) ;; 
prt_int_list (concat_lists [y;a;a;z]) ;; 
prt_int_list_list (concat_lists [[a];[z]]) ;; 
prt_int_list_list (concat_lists [[a;y];[z;x]]) ;; 
prt_int_list_list (concat_lists [[a;y];[a];[z;x]]) ;; 