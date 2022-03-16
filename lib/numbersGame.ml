
type expr =
	| Num of int
    | Plus of expr * expr        (* a + b *)
    | Minus of expr * expr       (* a - b *)
    | Times of expr * expr       (* a * b *)
    | Divide of expr * expr      (* a / b *)
;;

let input = [6; 5; 4; 2; 75; 50];;
let goal = 537;;


let insert_all_possible_indexes n ls =
	let rec insert_into list index = (match (list, index) with
		| (x :: xs, 0) -> n :: x :: xs
		| (x :: xs, i) -> x :: insert_into xs (i-1)
		| ([], _) -> [n]
	) in
	List.init ((List.length ls) + 1) (insert_into ls)
in
let rec all_shuffles_from ls = 
	match ls with
	| [] -> [[]]
	| x :: xs -> 
		let all_sl = all_shuffles_from xs in
		let all_sls_with_x = List.map (insert_all_possible_indexes x) all_sl in
		let liststs = List.flatten all_sls_with_x in
		liststs 
in

let op_to_target (list: int list) (target: int) : expr option = 
	let rec calculate runningVal runningExpr ls: expr option =
		match ls, runningVal with
		| _, i when i = target -> Some runningExpr
		| [], _ -> None
		| x :: xs, _ -> 
			let add_branch = calculate (runningVal + x) (Plus(runningExpr, Num x)) xs in
			if Option.is_some add_branch then
				add_branch
			else 
			let sub_branch = calculate (runningVal - x) (Minus(runningExpr, Num x)) xs in
			if Option.is_some sub_branch then
				sub_branch
			else
			let mul_branch = calculate (runningVal * x) (Times(runningExpr, Num x)) xs
			in
			if Option.is_some mul_branch then
				mul_branch
			else
			if runningVal mod x = 0 then
				calculate (runningVal / x) (Divide(runningExpr, Num x)) xs
			else
			None
	in
	calculate (List.hd list) (Num (List.hd list)) (List.tl list)
in
let rec find_first shuffles target : expr option =
	match shuffles with
	| [] -> None
	| shuffle :: remaining_shuffles -> 
		let attempt = op_to_target shuffle target in
		if Option.is_some attempt then
			Some (Option.get attempt)
		else
			find_first remaining_shuffles target
in

let solve input target: expr option = 
	let number_shuffles = all_shuffles_from input in
	find_first number_shuffles target
in

let solution_opt = solve input goal in

let rec to_string e =
    match e with
	| Num i -> string_of_int i
    | Plus (left, right) ->
       "(" ^ to_string left ^ " + " ^ to_string right ^ ")"
    | Minus (left, right) ->
       "(" ^ to_string left ^ " - " ^ to_string right ^ ")"
    | Times (left, right) ->
       "(" ^ to_string left ^ " * " ^ to_string right ^ ")"
    | Divide (left, right) ->
       "(" ^ to_string left ^ " / " ^ to_string right ^ ")"
in

if Option.is_none solution_opt then
	print_endline "No solution exists"
else
	(print_endline "Solution found";
	print_endline (to_string (Option.get solution_opt)))
