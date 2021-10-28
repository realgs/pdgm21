(*Zadanie 1*)

let rec splitBySign xs =
    match xs with
    | [] -> ([], [])
    | h :: t -> if h < 0 then let (p1, p2) = splitBySign t in (h :: p1, p2)
            else if h mod 2 = 0 then splitBySign t
            else let (p1, p2) = splitBySign t in (p1, h :: p2);;

splitBySign([-3; -6; 7; -9; 13]) = ([-3; -6; -9], [7; 13]);;
splitBySign([]) = ([], []);;
splitBySign([-1; -2; -3]) = ([-1; -2; -3], []);;
splitBySign([0; 2; 4]) = ([], []);;
splitBySign([-1; 4; 3; -6; 2; 7]) = ([-1; -6], [3; 7]);;
splitBySign([1; 3; 5]) = ([], [1; 3; 5]);;


(*Zadanie 2*)

let rec lengthOfList xs =
   match xs with
   | [] -> 0
   | _ -> 1 + lengthOfList(List.tl xs);;

lengthOfList [5; 4; 3; 2] = 4;;
lengthOfList [] = 0;;
lengthOfList [1] = 1;;

(*Zadanie 3*)

let rec joinLists(xs, ys) =
    match (xs, ys) with
    | ([], []) -> []
    | (_, []) -> xs
    | ([], _) -> ys
    | (_, _) -> List.hd xs :: List.hd ys :: joinLists(List.tl xs, List.tl ys);;

joinLists([5;4;3;2], [1;2;3;4;5;6]) = [5; 1; 4; 2; 3; 3; 2; 4; 5; 6];;
joinLists([], [2; 5; 3]) = [2; 5; 3];;
joinLists([9; 8; 7], []) = [9; 8; 7];;
joinLists([], []) = [];;
