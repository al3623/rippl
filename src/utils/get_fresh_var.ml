let counter = ref 0

let get_fresh a =
	let var_string = a ^ string_of_int !counter in
	counter := !counter + 1;
	var_string
