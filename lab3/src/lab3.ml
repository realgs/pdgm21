(*zadanie1*)

let rec splitBySign xs =
    match xs with
        (h :: t) ->
            if h < 0 then let (small, large) = splitBySign(t) in (h :: small, large)
            else if  h mod 2 != 0 then let (small, large) = splitBySign(t) in (small, h :: large)
            else  splitBySign(t)

        | _ -> ([], []);;

splitBySign([-3;-6;7;-9;13]);;
splitBySign([]);;
splitBySign([0;0;0;1;-3;2]);;

(*zadanie2*)

let rec lengthOfList xs =
    match xs with
        (h :: t) -> 1 + lengthOfList(t)
        | _ -> 0;;

lengthOfList([1;2;3;4]);;
lengthOfList([]);;
lengthOfList(["ala"; "ma"; "kota"]);;


(*zadanie3*)

let rec joinLists (xs, ys) =
    match (xs, ys) with
        ((h1 :: t1), (h2 :: t2)) -> [h1; h2] @ joinLists(t1, t2)
        | ([], (h2::t2)) -> [h2] @ joinLists([], t2)
        | ((h1::t1), []) -> [h1] @ joinLists(t1, [])
        | _ -> [];;

joinLists([5;4;3;2], [1;2;3;4;5;6]);;
joinLists([], []);;
joinLists([1;3;2], [5]);;