(*zadanie 1*)
let splitBySign xs =
  let rec splitBySignIter ys l1 l2 =
    if ys = [] then (l1, l2) else
      if List.hd ys < 0 then splitBySignIter (List.tl ys)(List.hd ys :: l1) l2 else
        if List.hd ys mod 2 = 1 then splitBySignIter(List.tl ys) l1 (List.hd ys :: l2) else
          splitBySignIter (List.tl ys) l1 l2
  in
  splitBySignIter xs [] [];;

splitBySign [1;2;3;-1;-2;-3;4;5;6;0;-2];;


(*zadanie 2*)
let rec lengthOfList xs =
  if xs = [] then 0 else
    1 + lengthOfList (List.tl xs);;

lengthOfList [];;
lengthOfList [1;2;3];;


(*zadanie 3*)
let rec joinLists xs ys =
  match (xs, ys) with
    ([],[]) -> []
   |([],_) -> (List.hd ys)::(joinLists xs (List.tl ys))
   |(_,[]) -> (List.hd xs)::(joinLists (List.tl xs) ys)
   | _ -> (List.hd xs)::(List.hd ys)::(joinLists (List.tl xs)(List.tl ys));;

joinLists [1;2;3][0;0;0;0];;
joinLists [1;2;3][];;
joinLists [][1;2;3];;
