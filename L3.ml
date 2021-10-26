let splitBySign list =
    let rec split list negative positive =
        if list = [] then [negative; positive]
        else if List.hd list < 0 then split(List.tl list)(negative @ [List.hd list])(positive)
        else if List.hd list > 0 && List.hd list mod 2 <> 0 then split(List.tl list)(negative)(positive @ [List.hd list]) 
        else split(List.tl list)(negative)(positive) in
    split list [] [];;

splitBySign([-3;-6;7;-9;13]) = [[-3; -6; -9];[7; 13]];;
splitBySign([-4;15;0;12;-12;13]) = [[-4;-12];[15;13]];;
splitBySign([]) = [[];[]];;

let rec lengthOfList list = 
    if list<> [] then 1 + lengthOfList(List.tl list) else 0;;

lengthOfList([-4;15;0;12;-12;13]) = 6;;
lengthOfList([]) = 0;;
lengthOfList(["a";"b";"c"]) = 3;;

let rec joinLists (xs, ys) =
    match xs, ys with
    | ([],_) -> ys
    | (_,[]) -> xs
    | (_,_) -> List.hd xs :: List.hd ys :: joinLists(List.tl xs, List.tl ys)
    ;;

joinLists([5;4;3;2], [1;2;3;4;5;6]) = [5;1;4;2;3;3;2;4;5;6];;
joinLists([5;4;3;2], []) = [5;4;3;2];;
joinLists([],[]) = [];;