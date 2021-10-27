(*Zadanie 1*)
let splitBySign xs =
  let rec splitBySignIter xs minus plus =
    if xs = [] then (minus, plus)
    else if List.hd xs < 0 then splitBySignIter (List.tl xs) (minus @ [List.hd xs]) plus
    else if List.hd xs mod 2 = 0 then splitBySignIter (List.tl xs) minus plus
    else splitBySignIter (List.tl xs) minus (plus @ [List.hd xs]) in
  splitBySignIter xs [] [];;

splitBySign [-3; -6; 7; -9; 13] = ([-3; -6; -9], [7; 13]);;
splitBySign [0; -7; 11; -15; 4; -5; -12; 3; 2; 4] = ([-7; -15; -5; -12], [11; 3]);;
splitBySign [2; 4; 6; 8; 10; 11; -11] = ([-11], [11]);;
splitBySign [] = ([], []);;

(*Zadanie 2*)
let lengthOfList xs =
  let rec lengthOfListIter xs a =
    if xs = [] then a
    else lengthOfListIter (List.tl xs) (a+1) in
  lengthOfListIter xs 0;;

lengthOfList [0; 1; 2; -5; 4; 61; 3] = 7;;
lengthOfList ['A'; 'B'; 'C'; 'D'] = 4;;
lengthOfList [] = 0;;

(*Zadanie 3*)
let rec joinLists xs ys =
  match (xs, ys) with
    (hx::tx, hy::ty) -> hx::hy::joinLists (List.tl xs) (List.tl ys) |
    ([], _) -> ys |
    (_, []) -> xs;;

joinLists [5; 4; 3; 2] [1; 2; 3; 4; 5; 6] = [5; 1; 4; 2; 3; 3; 2; 4; 5; 6];;
joinLists ["x"; "y"; "z"] ["a"; "b"; "c"; "d"] = ["x"; "a"; "y"; "b"; "z"; "c"; "d"];;
joinLists [] [-2.0; 3.0] = [-2.0; 3.0];;
joinLists [] [] = [];;

