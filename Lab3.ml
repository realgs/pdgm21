
(*Zadanie 1*)

let reverseList list = 
  let rec reverseHelper list result = 
    match list with
    | [] -> result
    | h::t -> reverseHelper t (h::result)
  in reverseHelper list [];;

let splitBySign list = 
  let rec splitHelper (list, negative, positiveOdd)=
    if list != [] then match (List.hd list < 0, List.hd list mod 2 != 0) with
      | (false, true) -> splitHelper(List.tl list, negative, List.hd list :: positiveOdd)
      | (true, true) -> splitHelper(List.tl list, List.hd list :: negative, positiveOdd)
      | (_, false) -> splitHelper(List.tl list, negative, positiveOdd)
    else (reverseList(negative), reverseList(positiveOdd))
  in splitHelper(list, [], []);;

splitBySign [1;2;0;-5;3;17;-13] = ([-5; -13], [1; 3; 17]);;
splitBySign([1; 2; -4; -5; 5; 0; 2; 10; 15; 22; -123; 17; -69; 69; 13]) = ([-5; -123; -69], [1; 5; 15; 17; 69; 13]);;
splitBySign([-3;-6;7;-9;13]) = ([-3; -9], [7; 13]);;
splitBySign([]) = ([], []);;
splitBySign([-12; -13; -15; 0; 0; 0; 1; 2; 3; 4; 68; 77; 159]) = ([-13; -15], [1; 3; 77; 159]);;


(* Zadanie 2 *)

let lengthOfList list=
  let rec lengthHelper(list, listLength) = 
    if list = [] then listLength
    else lengthHelper(List.tl list, (listLength+1))
  in lengthHelper(list, 0);;

lengthOfList [1; 2; 3; 4; 5] = 5;;
lengthOfList [] = 0;;
lengthOfList ["test"; "dzialania"; "string"] = 3;;

(* Zadanie 3 *)
 
let rec joinLists(list1, list2)=
  if (list1 != [] && list2!= []) then (List.hd list1):: (List.hd list2)::joinLists((List.tl list1),  (List.tl list2))
  else if list1!=[] then list1
  else list2;;

joinLists([1;2;3;4;5;6], [-1;-2;-3;-4;-5;-6]) = [1; -1; 2; -2; 3; -3; 4; -4; 5; -5; 6; -6];;
joinLists([], []) = [];;
joinLists([], [1;2;3]) = [1; 2; 3] ;;
joinLists([1;2;3], []) = [1; 2; 3];;


