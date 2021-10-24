(* zadanie 1*)
(* zlozonosc czasowa: n *)
let splitBySign myList = 
	let rec splitBySignHelper (myList, negative, positiveAndOdd) = 
		if myList=[] then (List.rev negative, List.rev positiveAndOdd) 
		else if List.hd myList < 0 then 
			splitBySignHelper(List.tl myList, List.hd myList :: negative, positiveAndOdd)
		else if List.hd myList > 0 && List.hd myList mod 2 = 1 then
			splitBySignHelper(List.tl myList, negative, List.hd myList :: positiveAndOdd)
		else splitBySignHelper(List.tl myList, negative, positiveAndOdd)
	 in splitBySignHelper(myList, [], [])
;;

splitBySign [-3;-6;7;-9;13] = ([-3;-6;-9],[7;13]);;
splitBySign [] = ([],[]);;
splitBySign [1] = ([],[1]);;
splitBySign [0;6] = ([],[]);;
splitBySign [-1] = ([-1],[]);;

(* zadanie 2 *)
(* zlozonosc czasowa: n *)
let lengthOfList myList = 
	let rec lengthOfListHelper (myList, length) = 
		match myList with 
			[] -> length
			| _ -> lengthOfListHelper(List.tl myList, length+1)
	in lengthOfListHelper(myList, 0)
;;

lengthOfList[5;4;3;2] = 4;;
lengthOfList["ala"] = 1;;
lengthOfList[] = 0;;

(* zadanie 3*)
let rec joinLists myList1 myList2 = 
	match (myList1, myList2) with
		([], _) -> myList2
		| (_, []) -> myList1
		| (_,_) -> List.hd myList1 :: joinLists myList2 (List.tl myList1)
;;

joinLists[5;4;3;2] [1;2;3;4;5;6] = [5;1;4;2;3;3;2;4;5;6];;
joinLists [] [] = [];;
joinLists ['a'] [] = ['a'];;
joinLists [] ['a'] = ['a'];;
joinLists [1;3] [2] = [1;2;3];;
