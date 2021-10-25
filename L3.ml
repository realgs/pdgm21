
let rec splitBySign xs =
    match xs with
    | [] -> ([], [])
    | head::body when head < 0 -> let (a, b) = splitBySign body in (a, head::b)
    | head::body when head mod 2 = 1 -> let (a, b) = splitBySign body in (head::a, b)
    | head::body -> splitBySign body
;;

splitBySign([1;2;-3;4;-5;-6;7;8]) = ([1; 7], [-3; -5; -6]);;
splitBySign([]) = ([], []);;
splitBySign([-1;-2;4]) = ([], [-1;-2]);;
splitBySign([1;3;5]) = ([1;3;5], []);;


let lengthOfList xs =
    let rec accLengthOfList xs acc =
        match xs with
        | head::body -> accLengthOfList body (acc+1)
        | [] -> acc
    in accLengthOfList xs 0
;;

lengthOfList [] = 0;;
lengthOfList [1;2;3] = 3;;
lengthOfList [3.0] = 1;;


let rec joinLists l1 l2 =
    match (l1, l2) with
    | (h1 :: b1, h2 :: b2) -> h1 :: h2 :: joinLists b1 b2
    | (h1 :: b1, []) -> l1
    | ([], h2 :: b2) -> l2
    | _ -> []
;;

joinLists [1;3;5;7;9] [2;4;6;8] = [1; 2; 3; 4; 5; 6; 7; 8; 9] ;;
joinLists [] [2;4;6;8;10] = [2; 4; 6; 8; 10];;
joinLists [2;4;6;8;10] [] = [2; 4; 6; 8; 10];;
joinLists [] [] = [];;
