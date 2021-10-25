let reverse xs =
  let rec reverseTail (xs, result) =
    if xs = [] then result
    else reverseTail(List.tl xs, List.hd xs::result)
  in reverseTail(xs,[]);;


(*task 1*)

let splitBySign xs =
  let rec splitBySignTail (xs, ys, zs) =
    if xs = [] then (reverse(ys), reverse(zs))
    else if List.hd xs < 0 then splitBySignTail(List.tl xs, List.hd xs::ys, zs)
    else if List.hd xs mod 2 = 1 then splitBySignTail(List.tl xs, ys, List.hd xs::zs)
    else splitBySignTail(List.tl xs, ys, zs)
  in splitBySignTail(xs,[],[]);;


splitBySign([-3;-6;7;-9;13]) = ([-3;-6;-9],[7;13]);;
splitBySign([]) = ([],[]);;
splitBySign([-3;-6;-7;-9;-13]) = ([-3;-6;-7;-9;-13],[]);;
splitBySign([0;6;7;9;13]) = ([],[7;9;13]);;



(*task 2*)

let lengthOfList xs =
  let rec lengthOfListTail (xs, result) =
    if xs = [] then result
    else lengthOfListTail(List.tl xs, 1 + result)
  in lengthOfListTail(xs,0);;


lengthOfList([5;4;3;2]) = 4;;
lengthOfList([]) = 0;;
lengthOfList(["aa";"a";"aaaaa"]) = 3;;



(*task 3*)

let joinLists (xs, ys) =
  let rec joinListsTail (xs, ys, result) =
    match (xs, ys) with
      ([],[]) -> reverse(result)
     |([], h2::t2) -> joinListsTail(xs, t2, h2::result)
     |(h1::t1, []) -> joinListsTail(t1, ys, h1::result)
     |(h1::t1, h2::t2) -> joinListsTail(t1, t2, h2::h1::result)
  in joinListsTail(xs,ys,[]);;


joinLists([5;4;3;2],[1;2;3;4;5;6]) = [5;1;4;2;3;3;2;4;5;6];;
joinLists([],[]) = [];;
joinLists([],["a";"aa"]) = ["a";"aa"];;
joinLists([5;4;3;2],[]) = [5;4;3;2];;
    
                             
      
  
