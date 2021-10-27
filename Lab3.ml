
(*zadanie1*)
let splitBySign xs =
let rec splitBySignIter(xs,negative, positive) =
  match xs with
    [] -> (negative, positive)
  |h :: t -> if h  <  0 then splitBySignIter(t,negative @ [h], positive)
        else if(h > 0 &&  h mod 2 != 0) then splitBySignIter(t, negative, positive @ [h]) 
        else splitBySignIter(t,negative, positive) in
    splitBySignIter(xs,[],[]);;
                       

splitBySign[-3;-1;2;0;3;5];;
splitBySign[1;2;3;4;5];;
splitBySign[-5;-4;-4;2];;
splitBySign[];;

(*zadanie2*)
let rec lengthOfList xs=
  if xs = [] then 0 else
    1+lengthOfList(List.tl xs);;

lengthOfList["a";"l";"a"];;
lengthOfList[2;3;2;12;9;2];;
lengthOfList[];;

(*zadanie3*)
let rec joinLists (xs1, xs2) =
  match (xs1, xs2) with
    ([], []) -> []
   |(h1 :: t1, h2 :: t2) -> h1 :: h2 :: joinLists(t1, t2)
   |(_,[]) -> xs1
   |([],_) -> xs2;;
  

joinLists([1;2;3;4], [5;6;7;8]);;
joinLists([1], [5;6;7;8;9]);;
joinLists([1;2;3;4;5], [6]);;
joinLists([], [5;6;7]);;
joinLists([1;2;3;4], []);;
joinLists([], []);;
