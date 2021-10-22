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
   | (head1 :: tail1, head2 :: tail2) -> head1::head2::joinLists (tail1,tail2);;