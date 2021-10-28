(*Dawid Krutul*)
(*Zadanie 1*)

(* zlozonosc obliczeniowa - liniowa od dlugosci listy
   zlozonosc pamieciowa - liniowa od dlugosci listy  *)

let rec splitBySign (numbers,list1,list2) =
   if numbers != [] then match(List.hd numbers < 0, List.hd numbers mod 2 != 0) with
    |(true,_) -> splitBySign(List.tl numbers, list1 @ [List.hd numbers], list2)
    |(false,true) -> splitBySign(List.tl numbers,list1,list2 @ [List.hd numbers])
    |(_,_) -> splitBySign(List.tl numbers,list1,list2)
   else
      (list1,list2);;

splitBySign([-3;-6;7;-9;13],[],[]) = ([-3;-6;-9],[7;13]);;
splitBySign([-2;-1;0;1;2;3;4],[],[]) = ([-2;-1],[1;3]);;
splitBySign([],[],[]) = ([],[]);;

(*Zadanie 2*)

let rec lengthOfList list =
    if list = [] then 0
    else 1 + lengthOfList(List.tl list);;


lengthOfList([1;2;3;4]) = 4;;
lengthOfList([]) = 0;;
lengthOfList(["jest";"fajnie"]) = 2;;

(* zlozonosc obliczeniowa - liniowa od dlugosci listy
   zlozonosc pamieciowa = liniowa od dlugosci listy  *)

(*Zadanie 3*)

let rec joinLists (list1,list2) =
    match(list1,list2) with
        |([],[]) -> []
        |([],_) -> List.hd list1 :: joinLists(list1,List.tl list2)
        |(_,[]) -> List.hd list1 :: joinLists(List.tl list1,list2)
        |(_,_) -> List.hd list1 :: List.hd list2 :: joinLists(List.tl list1,List.tl list2);;

joinLists([1;3;5;7],[2;4;6;8]) = [1; 2; 3; 4; 5; 6; 7; 8];;
joinLists([1;2;3;4],[1;2;3;4]) = [1; 1; 2; 2; 3; 3; 4; 4];;
joinLists([],[]) = [];;

(* zlozonosc obliczeniowa - kwadratowa od dlugosci listy
   zlozonosc pamieciowa - kwadratowa od dlugosci listy  *)
