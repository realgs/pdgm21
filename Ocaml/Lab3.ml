(*Zadanie 1*)
(*Zlozonosc obliczeniowa - O(n^2)*)
(*Zlozonosc pamieciowa - O(n)*)
let splitBySign list =
  let rec checkIfListCorrect list =
    if list = [] then true
    else if List.hd list >= 0 && List.hd list mod 2 = 0 then false
    else checkIfListCorrect (List.tl list)
  and splitIntoLists (list, negList, posList) =
    if list = [] then (negList, posList)
    else if List.hd list < 0 then splitIntoLists (List.tl list, negList @ [List.hd list], posList)
    else splitIntoLists (List.tl list, negList, posList @ [List.hd list])
  in
  if checkIfListCorrect list <> true then raise(Failure "List cannot be separated")
  else splitIntoLists (list, [], [])
;;

splitBySign [-3; -6; 7; -9; 13];;
splitBySign [];;
splitBySign [3; 5; 7];;
(*splitBySign [0];;*)

(*Zadanie 2*)
(*Zlozonosc obliczeniowa - O(n)*)
(*Zlozonosc pamieciowa - O(1)*)
let lengthOfList list =
  let rec calculateLength list i =
    if list = [] then i
    else calculateLength (List.tl list) (i+1)
  in
  calculateLength list 0
;;

lengthOfList [5; 4; 3; 2];;
lengthOfList [];;
lengthOfList [["Ala"; "Bartek"]; ["Celina"]];;

(*Zadanie 3*)
(*Zlozonosc obliczeniowa - O(n^2)*)
(*Zlozonosc pamieciowa - O(n)*)
let joinLists listA listB =
  let rec concatenateA (listA, listB, finalList) =
    if listA = [] then finalList @ listB
    else concatenateB (List.tl listA, listB, finalList @ [List.hd listA])
  and concatenateB (listA, listB, finalList) =
    if listB = [] then finalList @ listA
    else concatenateA (listA, List.tl listB, finalList @ [List.hd listB])
  in
  concatenateA (listA, listB, [])
;;

joinLists [5; 4; 3; 2] [1; 2; 3; 4; 5; 6];;
joinLists [] [];;
joinLists [5; 3; 4; 5; 6] [];;
