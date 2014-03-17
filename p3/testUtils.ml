
let prt_int x = print_endline (string_of_int x)
;;

let string_of_int_list l =
    let rec string_of_int_elements l = match l with
          [] -> ""
        | (h::[]) -> string_of_int h
        | (h::t) -> string_of_int h ^ ";" ^ string_of_int_elements t
    in "[" ^ string_of_int_elements l ^ "]"
;;

let prt_int_list l = print_endline (string_of_int_list l)
;;

let rec string_of_int_list_list l = match l with
      [] -> ""
    | (h::t) -> (string_of_int_list h) ^ (string_of_int_list_list t)
;;

let prt_int_list_list l = print_endline (string_of_int_list_list l)
;;

let prt_bool x = print_endline (string_of_bool x)
;;

let string_of_bool_list l =
    let rec string_of_bool_elements l = match l with
          [] -> ""
        | (h::[]) -> string_of_bool h
        | (h::t) -> string_of_bool h ^ ";" ^ string_of_bool_elements t
    in "[" ^ string_of_bool_elements l ^ "]"
;;

let prt_bool_list l = print_endline (string_of_bool_list l)
;;

