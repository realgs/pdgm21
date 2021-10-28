(*Pomocnicze funkcje*)

let rec addToList (xs, x) =
  if xs <> []
    then List.hd xs :: addToList (List.tl xs, x)
  else x::[];; 

let rec addToList2 (xs, x, y) =
  if xs <> []
    then List.hd xs :: addToList2 (List.tl xs, x, y)
  else x::y::[];; 



(*Zadanie 1*)

let splitBySign xs = 
  let rec splitter (xs, xsNeg, xsPosOdd) =
    if xs = []
      then (xsNeg, xsPosOdd)
    else if List.hd xs < 0
      then splitter ((List.tl xs), (addToList (xsNeg, (List.hd xs))), xsPosOdd)
    else if ((List.hd xs) > 0 && (List.hd xs) mod 2 <> 0)
      then splitter ((List.tl xs), xsNeg ,(addToList (xsPosOdd, (List.hd xs))))
    else splitter ((List.tl xs), xsNeg, xsPosOdd)
  in splitter (xs, [], []);;

  splitBySign [-3; -6; 7; -9; 13];;
  splitBySign [];;
  splitBySign [-2; 4; 5; 0; 5; 13];;



(*zadanie 2*)

let rec listLength xs =
  if xs <> [] 
    then 1 + listLength (List.tl xs)
  else 0;;

listLength [];;
listLength [1; 2; 3; 4; 5; 6];;
listLength ["k"; "o"; "t"];;


(*Zadanie 3*)

let joinLists xs1 xs2 = 
  let rec joinHelper (xs1, xs2, xsResult) =
    match (xs1, xs2) with
    [], [] -> xsResult
    | [], _ -> joinHelper(xs1, List.tl xs2, addToList(xsResult, List.hd xs2))
    | _, [] -> joinHelper(List.tl xs1, xs2, addToList(xsResult, List.hd xs1))
    | _, _ -> joinHelper(List.tl xs1, List.tl xs2, addToList2(xsResult, List.hd xs1, List.hd xs2))
  in joinHelper(xs1, xs2, [])  ;;


joinLists [5; 4; 3; 2] [1; 2; 3; 4; 5; 6];;
joinLists [1; 1; 1; 1; 1; 1; 1; 1] [2; 2; 2; 2];;

