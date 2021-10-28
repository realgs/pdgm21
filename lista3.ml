(*So³tysiak Magdalena*)

(*zadanie 1*)
let rec splitBySign list =
  match list with
  |[] -> ([],[])
  |h::t ->
    let (negative, posOdd) = splitBySign t in
     if (h < 0) then (h::negative, posOdd)
     else if (h>0 && h mod 2!=0) then (negative, h::posOdd)
     else (negative, posOdd);;

splitBySign[(-3);(-6);7;(-9);13];;
splitBySign[-2;-9;4;7;15];;
splitBySign[];;


(*zadanie 2*)

let rec lengthOfList list =
  if list = [] then 0
  else 1 + lengthOfList(List.tl list);;

lengthOfList[5;4;3;2];;
lengthOfList[1;2;3;4;7;8;9];;
lengthOfList[];;
lengthOfList['A'];;

(*zadanie 3*)

let rec joinLists (list1,list2) =
  match (list1, list2) with
    ([],[]) -> []
   |(_,[]) -> list1
   |([],_) -> list2
   |(h1::t1, h2::t2) -> h1::h2::joinLists(t1, t2);;

joinLists([],[1;2;3]);;
joinLists([4;5;6],[]);;
joinLists([5;4;3;2],[1;2;3;4;5;6]);;
joinLists([],[]);;



(*druga wersja*)

let rec joinLists1 (list1,list2) =
  match list1 with
    [] -> list2
   |h::list1 -> h:: joinLists(list2, list1);;

joinLists1([],[1;2;3]);;
joinLists1([4;5;6],[]);;
joinLists1([5;4;3;2],[1;2;3;4;5;6]);;
joinLists1([],[]);;
