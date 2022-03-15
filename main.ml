
let input = [5; 2; 3; 1; 50; 75] in
let goal = 365 in

let print_list_of_sublists l_sl = 
	let print_list a = List.iter (Printf.printf "%d ") a in
	List.iter (fun l -> print_string "[ "; print_list l; print_string "]\n") l_sl
in

let rec insert_all_possible_indexes n ls =
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

let op_to_target (list: int list) (target: int) : (char list) option = 
	let rec calculate runningVal runningOps ls: (char list) option =
		match ls, runningVal with
		| _, i when i = target -> Some runningOps
		| [], _ -> None
		| x :: xs, i -> 
			let add_branch = calculate (runningVal + x) ('+' :: runningOps) xs in
			if Option.is_some add_branch then
				add_branch
			else 
			let sub_branch = calculate (runningVal - x) ('-' :: runningOps) xs in
			if Option.is_some sub_branch then
				sub_branch
			else
			let mul_branch = calculate (runningVal * x) ('*' :: runningOps) xs
			in
			if Option.is_some mul_branch then
				mul_branch
			else
			if runningVal mod x = 0 then
				calculate (runningVal / x) ('/' :: runningOps) xs
			else
			None
	in
	calculate (List.hd list) ([]) (List.tl list)
in
let number_shuffles = all_shuffles_from input in
let rec find_first shuffles : (int list * char list) option =
	match shuffles with
	| [] -> None
	| shuffle :: remaining_shuffles -> 
		let attempt = op_to_target shuffle goal in
		if Option.is_some attempt then
			Some (shuffle, Option.get attempt)
		else
			find_first remaining_shuffles
in
let solution_opt = find_first number_shuffles in
let rec get_first_k k list = 
	match list, k with
	| _, 0 -> []
	| x :: xs, _ -> x :: (get_first_k (k-1) xs)
	| [], _ -> []
in

let rec zip l1 l2 = 
	match l1, l2 with
	| [], [] -> []
	| _, [] -> []
	| [], x :: xs -> [x]
	| x :: xs, y :: ys -> x :: y :: (zip xs ys)
in
let reverse_polish numbers operators : string list =
	let numbers_str = List.map (fun i -> string_of_int i) numbers in
	let operators_str = List.map (fun o -> Char.escaped o) operators in
	(List.hd numbers_str) :: (zip (List.tl numbers_str) operators_str)
in

if Option.is_some solution_opt then
	let (nums, ops) = Option.get solution_opt in
	let nums_used = get_first_k (List.length ops + 1) nums in
	let solution = String.concat " " (reverse_polish nums_used ops) in
	print_string "Solution (in reverse polish notation):\n";
	print_string solution;
	print_newline();
else 
	print_string "No solution exists.\n"