(*Stanis³aw Izdebski*)

(*Zad. 1*)
let splitBySign xs =
	let rec splitBySignHelper (xs, accB0, accOdd) = 
		match xs with
		| [] -> (accB0, accOdd)
		| xh::xt -> 
			if xh<0 then splitBySignHelper(xt, accB0@[xh], accOdd)
			else if xh mod 2 <> 0 then splitBySignHelper(xt, accB0, accOdd@[xh])
			else splitBySignHelper(xt, accB0, accOdd)
	in
	splitBySignHelper(xs,[],[]);;

splitBySign([-1; 3; -5; 7; -8; -6; -7]);;
splitBySign([]);;
splitBySign([-3; 5; 2; 6; -5; 7]);;

(*Zad. 2*)
let lengthOfList xs =
	let rec lengthOfListHelper (xs, acc) =
		match xs with
		|[] -> acc
		| xh::xt -> lengthOfListHelper(xt, acc+1)
	in		
	lengthOfListHelper(xs, 0);;


lengthOfList([3; 6; 2; 8; 3; 6; 1]);;
lengthOfList([]);;
lengthOfList(["b"; "a"]);;


(*Zad. 3*)
let rec joinLists xs ys =
	match xs,ys with
	| xh::xt,  yh::yt -> xh::yh::joinLists xt yt
	| xh::xt,  []     -> xs
	| [],  yh::yt     -> ys
	| [], []          -> [];;

joinLists [1;3;5;7] [2;4;6];;
joinLists ["a";"c"] ["b"];;
joinLists [] [];;

