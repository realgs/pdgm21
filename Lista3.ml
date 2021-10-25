(*Sebastian Bednarski*)
(*task 1*)
let splitBySign list =
  let rec split (list, negative, positiveOdd) =
    if list = [] then [negative, positiveOdd]
    else if List.hd list mod 2 <> 0 && List.hd list > 0 then split(List.tl list, negative, positiveOdd @ [List.hd list])
    else if List.hd list < 0 then split(List.tl list, negative @ [List.hd list], positiveOdd)
    else split(List.tl list, negative, positiveOdd) in
    split(list, [], []);;

splitBySign([-3; -6; 7; -9; 13]);;
splitBySign([-1; 8; -7; 21; -13]);;
splitBySign([]);;

(*task 2*)
let listLength list = 
  let rec listLengthIter(list, length) =
    if list = [] then length
    else listLengthIter(List.tl list, length + 1) in
    listLengthIter(list, 0);;


listLength([5;4;3;2]);;
listLength([-1;2;3;4;5]);;
listLength(["a";"l";"a"]);;
listLength([]);;

(*task 3*)
let listMerge (list1,list2) =
  let rec listMergeIter(accum,list1,list2) =
    if list1 = [] then accum @ list2
    else if list2 = [] then accum @ list1
    else listMergeIter(accum @ List.hd list1 :: [List.hd list2], List.tl list1, List.tl list2) in
    listMergeIter([],list1,list2);;

listMerge([5;4;3;2],[1;2;3;4;5;6]);;
listMerge([],[1;2;3;4;5;6]);;
listMerge([5;4;3;2],[]);;
listMerge(["A";"l";"a"],["k";"o";"t";"a"]);;

