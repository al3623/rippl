let check_scope maplist var
match maplist with
	|hd :: tl -> if (StringMap.mem var hd) then "IN SCOPE" else check_scope tl var
	|[] -> "VARIABLE NOT IN SCOPE""



