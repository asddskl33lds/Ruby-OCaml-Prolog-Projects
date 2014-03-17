(* CMSC 330 / Fall 2013 / Project 4 *)
(* Name: Sarthi Andley*)

#load "str.cma"

(* ------------------------------------------------- *)
(* MODULE SIGNATURE *)
(* ------------------------------------------------- *)

module type NFA =
  sig
    (* You may NOT change this signature *)

    (* ------------------------------------------------- *)
    (* PART 1: NFA IMPLEMENTATION *)
    (* ------------------------------------------------- *)

    (* ------------------------------------------------- *)
    (* Abstract type for NFAs *)
    type nfa

    (* Type of an NFA transition.

       (s0, Some c, s1) represents a transition from state s0 to state s1
       on character c

       (s0, None, s1) represents an epsilon transition from s0 to s1
     *)
    type transition = int * char option * int 

    (* ------------------------------------------------- *)
    (* Returns a new NFA.  make_nfa s fs ts returns an NFA with start
       state s, final states fs, and transitions ts.
     *)
    val make_nfa : int -> int list -> transition list -> nfa

    (* ------------------------------------------------- *)
    (*  Calculates epsilon closure in an NFA.  

	e_closure m ss returns a list of states that m could 
	be in, starting from any state in ss and making 0 or 
	more epsilon transitions.

       There should be no duplicates in the output list of states.
     *)

    val e_closure : nfa -> int list -> int list

    (* ------------------------------------------------- *)
    (*  Calculates move in an NFA.  

	move m ss c returns a list of states that m could 
	be in, starting from any state in ss and making 1
	transition on c.

       There should be no duplicates in the output list of states.
     *)

    val move : nfa -> int list -> char -> int list

    (* ------------------------------------------------- *)
    (* Returns true if the NFA accepts the string, and false otherwise *)
    val accept : nfa -> string -> bool

    (* ------------------------------------------------- *)
    (* PART 2: REGULAR EXPRESSION IMPLEMENTATION *)
    (* ------------------------------------------------- *)

    (* ------------------------------------------------- *)
    type regexp =
	Empty_String
      | Char of char
      | Union of regexp * regexp
      | Concat of regexp * regexp
      | Star of regexp

    (* ------------------------------------------------- *)
    (* Given a regular expression, print it as a regular expression in 
       postfix notation (as in project 2).  Always print the first regexp 
       operand first, so output string will always be same for each regexp.
     *)
    val regexp_to_string : regexp -> string 

    (* ------------------------------------------------- *)
    (* Given a regular expression, return an nfa that accepts the same
       language as the regexp
     *)
    val regexp_to_nfa : regexp -> nfa

    (* ------------------------------------------------- *)
    (* PART 3: REGULAR EXPRESSION PARSER *)
    (* ------------------------------------------------- *)

    (* ------------------------------------------------- *)
    (* Given a regular expression as string, parses it and returns the
       equivalent regular expression represented as the type regexp.    
     *)
    val string_to_regexp : string -> regexp

    (* ------------------------------------------------- *)
    (* Given a regular expression as string, parses it and returns 
       the equivalent nfa 
     *)
    val string_to_nfa: string -> nfa

    (* ------------------------------------------------- *)
    (* Throw IllegalExpression expression when regular
       expression syntax is illegal
     *)
    exception IllegalExpression of string

end

(* ------------------------------------------------- *)
(* MODULE IMPLEMENTATION *)
(* ------------------------------------------------- *)

    (* Make all your code changes past this point *)
    (* You may add/delete/reorder code as you wish *)

module NfaImpl =

struct 

let rec map (f, l) = match l with
    [] -> []
  | (h::t) -> (f h)::(map (f, t));;

let rec fold (f, a, l) = match l with
    [] -> a
  | (h::t) -> fold (f, f (a, h), t);;

let rec checkList (l,e) = match l with
		[] -> false
		| (h::t) -> if (h=e) then true
					else checkList (t,e);;

let rec filter_negative x = match x with
		[] -> []
		| (h::t) -> if h = -16 then filter_negative(t)
				else h::(filter_negative(t));;
				
let next = let count = ref 0 in
			function () -> let temp = !count in
								count := (!count) + 1;
								temp;;


let rec find (x,v) = match x with 
		[] ->  false
		| h::t -> if h = v then true
				else find(t,v);;
				
let rec uniq x = match x with
	[] -> []
	| h::t -> if find(t,h) then uniq t
				else h::uniq(t);;
				



type transition = int * char option * int

type nfa = int * int list * transition list
(* starting state, list of final states and transitions*)

let get_transList m = match m with
			(s,f, t) -> t;;
		
let get_start m = match m with
			(s,f,t) -> s;;
			
let get_finalList m = match m with
			(s,f,t) -> f;;

let make_nfa (ss :int) (fs :int list) (ts :transition list) = (ss, fs, ts);;

let moving (m :nfa) ss c = List.flatten ( map( (function (s1,letter,s2) -> if (c = letter && checkList(ss,s1))then [s2] else []), get_transList m));;

let rec e_closure (m:nfa) ss = let ecStart = uniq(ss @ (moving m ss None)) in 
								if ss = ecStart then ss 
								else (e_closure m ecStart);;

let move (m:nfa) ss c = uniq (moving m ss (Some c));; 

let rec isFinalHelp fList st = match st with
			[] -> false
			| (h::t) -> if checkList(fList, h) then true
						else isFinalHelp fList t;;
						
let containsFinal (m:nfa) st = isFinalHelp (get_finalList m) st;;

let rec helper (m:nfa) s count startList = let state = e_closure m startList in 
												if count = (String.length(s)) then containsFinal m state
												else helper m s (count+1) (moving m state (Some (String.get s count)));;
												
let accept (m: nfa) s = helper m s 0 [get_start m];;

type regexp =
	  Empty_String
	| Char of char
	| Union of regexp * regexp
	| Concat of regexp * regexp
	| Star of regexp
	
let rec regexp_to_string r = match r with
	  Empty_String -> "E"
	| Char c -> String.make 1 c
	| Union (a,b) -> (regexp_to_string a) ^ " " ^ (regexp_to_string b) ^ " |" 
	| Concat (a,b) -> (regexp_to_string a) ^ " " ^(regexp_to_string b) ^ " ."
	| Star a -> (regexp_to_string a) ^ " *"

	;;


let make_union ((a : nfa), (b : nfa)) = let a_start = get_start a in let a_final = get_finalList a in 

						let b_start = get_start b in let b_final = get_finalList b in
						
						let a_trans = get_transList a in let b_trans = get_transList b in
						
						let newStart = next() in let newFinal = next() in
						
						let s_a = (newStart, None, a_start) in 
						let s_b = (newStart, None, b_start) in
						
						let a_f = ( List.hd(a_final), None, newFinal) in
						let b_f = ( List.hd(b_final), None, newFinal) in
						
						let transList = a_trans @ b_trans @ [s_a] @ [s_b] @ [a_f] @ [b_f] in
						
						make_nfa newStart ([newFinal]) transList
						;;


let make_concat ((a :nfa), (b :nfa)) = let a_start = get_start a in let a_final = get_finalList a in 

						let b_start = get_start b in let b_final = get_finalList b in
						
						let a_trans = get_transList a in let b_trans = get_transList b in
						
						let trans_concat = [(List.hd(a_final), None, b_start)] in
						
						let transList =  (a_trans @ b_trans @  trans_concat) in
						
						make_nfa a_start (b_final) transList;;

let make_star (a :nfa) = let a_start = get_start a in 
						 let a_final = get_finalList a in 

							let a_trans = get_transList a in 
							
							let newStart = next() in let newFinal = next() in
							
							let s_a = (newStart, None, a_start) in 
							
							let a_f = ( List.hd(a_final), None, newFinal) in
							
							let s_f = (newStart, None, newFinal) in
							
							let f_s = (newFinal, None, newStart) in
							
							let transList = a_trans @ [s_a] @ [a_f] @ [s_f] @ [f_s] in
								
							make_nfa newStart ([newFinal]) transList;;
						
let rec regexp_to_nfa r = match r with
				
				Empty_String -> let i = next() in let j = next() in make_nfa i [j] [(i, None, j)]
				
				| Char c -> let i = next() in let j = next() in make_nfa i [j] [(i, Some c, j)]
				
				| Union (a,b) ->  let (a_nfa : nfa) = regexp_to_nfa (a :regexp) in let b_nfa = regexp_to_nfa(b :regexp) in make_union (a_nfa, b_nfa)
				
				| Concat (a,b) -> let (a_nfa :nfa) = regexp_to_nfa (a :regexp) in let b_nfa = regexp_to_nfa(b :regexp) in make_concat (a_nfa, b_nfa)
				
				| Star a -> make_star (regexp_to_nfa a :nfa)
;;
				
	
exception IllegalExpression of string

(************************************************************************)

(* Scanner code provided to turn string into a list of tokens *)

type token =
   Tok_Char of char
 | Tok_Epsilon
 | Tok_Union
 | Tok_Star
 | Tok_LParen
 | Tok_RParen
 | Tok_END

let re_var = Str.regexp "[a-z]"
let re_epsilon = Str.regexp "E"
let re_union = Str.regexp "|"
let re_star = Str.regexp "*"
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"

let tokenize str =
 let rec tok pos s =
   if pos >= String.length s then
     [Tok_END]
   else begin
     if (Str.string_match re_var s pos) then
       let token = Str.matched_string s in
       (Tok_Char token.[0])::(tok (pos+1) s)
	 else if (Str.string_match re_epsilon s pos) then
       Tok_Epsilon::(tok (pos+1) s)
	 else if (Str.string_match re_union s pos) then
       Tok_Union::(tok (pos+1) s)
	 else if (Str.string_match re_star s pos) then
       Tok_Star::(tok (pos+1) s)
     else if (Str.string_match re_lparen s pos) then
       Tok_LParen::(tok (pos+1) s)
     else if (Str.string_match re_rparen s pos) then
       Tok_RParen::(tok (pos+1) s)
     else
       raise (IllegalExpression "tokenize")
   end
 in
 tok 0 str

 let lookahead tok_list = match tok_list with
        [] -> raise (IllegalExpression "lookahead")
        | (h::t) -> (h,t);;
(************************************************************************)

(*
  [Basic grammar]
  E -> A Tok_Sum E | A     D->  C
  A -> B Tok_Mult A | B
  B -> Tok_Num | Tok_LParen E Tok_RParen 
*)
let rec parse_D l =       (*UNION*)
        let (a1,l1) = parse_C l in
        let (t,n) = lookahead l1 in
        match t with 
                Tok_Union -> ( 		(* D -> C Tok_Union D *)
								let (a2,l2) = (parse_D n) in
								(Union (a1,a2) , l2)
							)
                | _ -> (a1,l1) 		(* D -> C *)
 
and parse_C l =         (*CONCAT*)
        let (a1,l1) = parse_B l in
				let (t,n) = lookahead l1 in
					match t with
						Tok_Char c -> 
							let (a2,l2) = (parse_C l1) in 
								(Concat (a1, a2), l2)
								
						| Tok_LParen -> 
							let (a2,l2) = (parse_C l1) in 
								(Concat (a1, a2), l2)
								
						| Tok_Epsilon -> 
								let (a2,l2) = (parse_C l1) in 
								(Concat (a1, a2), l2)
						
						| _ -> (a1,l1)		(* C->B *)
				
and parse_B l =       (*STAR*)
        let (a1,l1) = parse_A l in
        let (t,n) = lookahead l1 in
        match t with
	        Tok_Star ->  		(* B -> A Tok_Star *)
                (Star a1,n)
            | _ -> (a1,l1)		(* B->A *)

and parse_A l =        (*BASE*)
        let (t,n) = lookahead l in
						match t with
										(* A -> Tok_Char *)
							Tok_Char c -> (Char c, n) 

									  (* A -> Tok_LParen D Tok_RParen *)
							| Tok_LParen -> (
									let (a2,l2) = (parse_D n) in 
									let (t2,n2) = lookahead l2 in 
									if (t2 = Tok_RParen) then
										(a2,n2)
									else
											raise (IllegalExpression "parse_A")
									)

							| Tok_Epsilon -> (Empty_String, n)
							
							| _ -> raise (IllegalExpression "parse_A")
;;

let string_to_regexp s = let tok_list = tokenize s in

							let (a,t) = (parse_D tok_list) in 

								if t <> [Tok_END] then raise (IllegalExpression "parse_394") 
								else a;;

let string_to_nfa s = regexp_to_nfa (string_to_regexp s);;

end

module Nfa : NFA = NfaImpl;;
