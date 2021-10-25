let rec append (list, elem) = 
  if list = [] then [elem]
  else List.hd list :: append (List.tl list, elem)
;;  

let rec marge (list1, list2) =
  if list1 = [] then list2
  else List.hd list1 :: marge(List.tl list1, list2)
;;  

let splitBySign list =
  let rec splitBySignHlp (list, neg, pos) = 
    if list = [] then (neg, pos)
    else if List.hd list < 0 then splitBySignHlp(List.tl list, append(neg, List.hd list), pos)
    else if List.hd list mod 2 <> 0 then splitBySignHlp(List.tl list, neg , append(pos ,List.hd list))
    else splitBySignHlp(List.tl list, neg, pos)
  in splitBySignHlp(list, [], [])
;;
splitBySign [-3;-6;7;-9;13];;
splitBySign [-2;-6;-4];;
splitBySign [1;3;9];;
splitBySign [];;

let listLength list =
  let rec listLengthIter(list, length) =
    if list = [] then length
    else listLengthIter(List.tl list, length+1)
  in listLengthIter(list,0)
;;

listLength [];;
listLength [1;1;1;1;1];;
listLength ['a';'a'];;

let conectLists(list1, list2) =
  let rec conectListsHlp (list1, list2, res) =
    if list1 = [] && list2 = [] then res
    else if list1 = [] then marge(res, list2)
    else if list2 = [] then marge(res, list1)
    else conectListsHlp(List.tl list1,List.tl list2,append(append(res,List.hd list1),List.hd list2))
  in conectListsHlp(list1, list2, [])
;;  

conectLists ([1;1;1;1],[2;2;2;2;2]);;
conectLists ([],[]);;
conectLists ([],[1;2;3]);;
conectLists ([9;8;7],[]);;
