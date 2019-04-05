let counter = ref 0

let get_fresh =
	let var_string = "t" ^ string_of_int !counter in
	counter := !counter + 1;
	var_string