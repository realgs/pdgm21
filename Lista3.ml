(*Zad 1*)

(*metoda pomocnicza reverse*)
let reverse(list)=
  let rec reverseIn(list, reversed)=
    match list with
      [] -> reversed
      |head :: tail -> reverseIn(List.tl list, head :: reversed)
  in reverseIn(list, [])
;;

reverse([3;2;1])=[1;2;3];;

(*metoda splitBySign*)
let splitBySign(list) =
  let rec splitBySign(list, lower, higher) =
    match list with
      [] -> (reverse(lower), reverse(higher))
      |head :: tail ->
        if (head > 0 && head mod 2 != 0) then splitBySign(tail, lower, head :: higher)
        else if head < 0 then splitBySign(tail, head :: lower, higher)
        else splitBySign(tail, lower, higher)

  in splitBySign(list, [], [])
;;

splitBySign([-3; -6; 7; -9; 13]) = ([-3; -6; -9], [7; 13]);;
splitBySign([2; -6; 4; 1; 0]) = ([-6], [1]);;
splitBySign([])=([],[]);;

(*Zad2*)

let rec lengthOfList(list) =
  if list = [] then 0
  else 1 + lengthOfList(List.tl list)
;;
lengthOfList([5; 4; 3; 2]) = 4;;
lengthOfList([]) = 0;;
lengthOfList([5; 4; 3; 2; 3; 4; 5]) = 7;;

(*Zad3*)

let rec joinLists(list1, list2)=
  match (list1, list2) with
    ([], []) -> []
    |([], head :: tail) -> head :: joinLists([], tail)
    |(head :: tail, []) -> head :: joinLists(tail, [])
    |(head1 :: tail1, head2 :: tail2) -> head1 :: head2 :: joinLists(tail1, tail2)
;;
joinLists([5; 4; 3; 2], [1; 2; 3; 4; 5; 6]) = [5; 1; 4; 2; 3; 3; 2; 4; 5; 6];;
joinLists(["Ola";"do"],["poszla";"szkoly"]) = ["Ola"; "poszla"; "do"; "szkoly"];;
joinLists([5; 4; 3; 2],[]) = [5; 4; 3; 2];;

