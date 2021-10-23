let splitBySign list =
    let rec splitBySignIn (number,list1, list2 ) =
        if number != [] then match (List.hd number < 0, List.hd number mod 2 != 0) with
            (true, _) -> splitBySignIn(List.tl number,list1 @ [List.hd number], list2)
           |(false, true) -> splitBySignIn(List.tl number, list1, list2 @ [List.hd number])
           |(_, _) -> splitBySignIn(List.tl number, list1, list2)
        else (list1, list2)
    in splitBySignIn(list, [], []);;

splitBySign([-3; -6; 7; -9; 13]) = ([-3; -6; -9], [7; 13]);;
splitBySign([-5; -10; -20; 2; 4; 8; 11]) = ([-5; -10; -20], [11]);;
splitBySign([]) = ([], []);;

let rec listLength xs =
    if  xs = [] then 0
    else 1 + listLength(List.tl xs);;

listLength ["maslo"; "kawa"; "mleko"] = 3;;
listLength [3; 2; 4; 5] = 4;;
listLength [] = 0;;


let rec joinLists (list1, list2) =
  match (list1, list2) with
    ([],[]) -> []
   |([],head2 :: tail2) -> head2::joinLists (list1, tail2)
   |(head1 :: tail1,[]) -> head1::joinLists (tail1, list2)
   | (head1 :: tail1, head2 :: tail2) -> head1::head2::joinLists (tail1, tail2);;


joinLists([1; 2; 3; 4],[1; 2; 3; 4]) = [1; 1; 2; 2; 3; 3; 4; 4];;
joinLists([],[]) = [];;
joinLists([1; 2],[4; 5 ; 6]) = [1; 4; 2; 5; 6]

