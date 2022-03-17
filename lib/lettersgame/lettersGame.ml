
let words_arr = Arg.read_arg "lib/lettersgame/word_list.txt"
let all_words = Array.to_list words_arr
module FreqMap = Map.Make(Char)

let update_freq_map (map : int FreqMap.t) (l : char) =
	let increment_opt i_opt =
		if Option.is_some i_opt then
			Some ((Option.get i_opt) + 1)
		else Some (1)
	in
	FreqMap.update l (increment_opt) map

let is_sub_freq f1 f2 = 
	let f1_not_sub k i =
		let f1_k_opt = FreqMap.find_opt k f1 in
		if Option.is_some f1_k_opt then
			i <= (Option.get f1_k_opt)
		else
			false
	in
	FreqMap.exists f1_not_sub f2

let resolve (input : char list) (amount : int) : string list = 
	let letters = List.map (Char.uppercase_ascii) input in

	let contains_non_letter (word : string) : bool =
		String.exists (fun c -> not (List.mem c letters)) word
	in
	let candidates1 = List.filter contains_non_letter all_words in

	let letters_freq = List.fold_left update_freq_map FreqMap.empty letters in
	let string_freq str = String.fold_left update_freq_map FreqMap.empty str in
	
	let _ = is_sub_freq (string_freq "bruh") letters_freq in

	let candidates2 = List.filter
		(fun s -> is_sub_freq (string_freq s) letters_freq)
		candidates1
	in

	(* todo: sort candidates2 by string length. return the best 'amount' *)
	let _ = candidates2 in
	let _ = amount in
	[]
