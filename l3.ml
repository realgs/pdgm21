(*1*)
let splitBySign list =
	let rec splitBySignHelper list (l1, l2) =
		match list with
			[] -> (List.rev l1, List.rev l2)
			| (h::t) when h < 0 -> splitBySignHelper t (h::l1, l2)
			| (h::t) when h > 0 && h mod 2 <> 0 -> splitBySignHelper t (l1, h::l2)
			| (h::t) -> splitBySignHelper t (l1, l2)
	in splitBySignHelper list ([], [])
;;

splitBySign [-3; -2; -1; 1; 3] = ([-3; -2; -1], [1; 3]);;
splitBySign [-5; 7; 2; 3; -9; 4; 1] = ([-5; -9], [7; 3; 1]);;
splitBySign [] = ([], []);;

(*2*)
let rec lengthOfList list =
	if list=[] then 0
	else 1 + lengthOfList(List.tl list)
;;

lengthOfList [1; 2; 3] = 3;;
lengthOfList [1] = 1;;
lengthOfList [] = 0;;

(*3*)
let rec joinLists l1 l2 =
	match (l1, l2) with
		([], []) -> []
		| (h::t, []) -> h::joinLists t l2
		| ([], h::t) -> h::joinLists l1 t
		| (h1::t1, h2::t2) -> h1::h2::joinLists t1 t2
;;

joinLists [1; 6; 3; 7; 4; 9] [4; 5; 6; 2; 7; 9] = [1; 4; 6; 5; 3; 6; 7; 2; 4; 7; 9; 9];;
joinLists [3; 6; 1; 3; 4] [2; 3] = [3; 2; 6; 3; 1; 3; 4];;
joinLists [2; 3] [3; 6; 1; 3; 4] = [2; 3; 3; 6; 1; 3; 4];;
joinLists [1; 2; 3] [] = [1; 2; 3];;
joinLists [] [] = [];;