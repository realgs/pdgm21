(* Kacper Wójcicki *)

(* 1 *)
let splitBySign nums = 
  let rec splitBySignIn nums negativeNums positiveOddNums =
    if nums = [] then [negativeNums; positiveOddNums]
    else if List.hd nums < 0 then splitBySignIn(List.tl nums)(negativeNums @ [List.hd nums])(positiveOddNums)
    else if List.hd nums > 0 &&  (List.hd nums) mod 2 <> 0 then splitBySignIn(List.tl nums)(negativeNums)(positiveOddNums @ [List.hd nums])
    else splitBySignIn(List.tl nums)(negativeNums)(positiveOddNums) in
  splitBySignIn nums [] [];; 

splitBySign [-3;-6;7;-9;13] = [[-3;-6;-9];[7;13]];;
splitBySign [-3;-6;0;8;7;-9;13] = [[-3;-6;-9];[7;13]];;
splitBySign [] = [[];[]];;

(* 2 *)
let lengthOfList list =
  let rec lengthOfListTailRec list i = 
    if list = [] then i
    else lengthOfListTailRec (List.tl list) i+1 in
  lengthOfListTailRec list 0;;

lengthOfList [5;4;3;2] = 4;;
lengthOfList ["Lech"; "Mistrz"; "Polski"] = 3;;
lengthOfList [] = 0;;

(* 3 *)
let joinLists list1 list2 = 
  let rec joinListsRec result list1 list2 =
    if list1 = [] then result @ list2
    else if list2 = [] then result @ list1
    else joinListsRec (result @ ((List.hd list1) :: [List.hd list2])) (List.tl list1) (List.tl list2) in
  joinListsRec [] list1 list2;;

joinLists [5;4;3;2] [1;2;3;4;5;6] = [5;1;4;2;3;3;2;4;5;6];;
