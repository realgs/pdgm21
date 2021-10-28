(*zadanie 1*)
let rec splitBySign list =
  match list with
    [] -> ([],[])
  | head:: tail -> let (negative, positiveOdd) = splitBySign tail in
                   if head < 0 then (head::negative, positiveOdd)
                   else if head mod 2 = 1 then (negative, head::positiveOdd)
                   else (negative, positiveOdd);;

splitBySign [-3;-6;7;-9;13] = ([-3;-6;-9],[7;13]);;
splitBySign [] = ([],[]);;
splitBySign [1;2;-3] = ([-3],[1]);;
splitBySign [1;3] = ([],[1;3]);;

(*zadanie 2*)
let rec lengthOfList list =
  if list = [] then 0
  else 1 + lengthOfList(List.tl list);;

lengthOfList[] = 0;;
lengthOfList[1;2] = 2;;
lengthOfList["a";"b";"c"] = 3;;
lengthOfList[5;4;3;2] = 4;;

(*zadanie 3*)
let rec joinList list1 list2 =
  match(list1,list2) with
    ([],[]) -> []
  | ([],_) -> list2
  | (_,[]) -> list1
  | (head1::tail1, head2::tail2) -> head1::head2::joinList(tail1 tail2);;

joinList [5;4;3;2] [1;2;3;4;5;6] = [5; 1; 4; 2; 3; 3; 2; 4; 5; 6];;
joinList [] ["a";"b"] = ["a";"b"];;
joinList [1;2;3] [] = [1;2;3];;
joinList [1;3;5;7] [2;4] = [1; 2; 3; 4; 5; 7];;

