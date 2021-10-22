(*1*)
let rec splitBySign(xs)=
          let rec split(xs, negativeList, positiveList)=
                    if xs=[] then negativeList, positiveList
                    else if List.hd xs <0 then split(List.tl xs, negativeList@[List.hd xs], positiveList)
                    else if List.hd xs >0 && List.hd xs mod(2)=1 then split(List.tl xs, negativeList, positiveList@[List.hd xs])
                    else split(List.tl xs, negativeList, positiveList)
in split(xs, [], []);;

splitBySign([ 1; 2; -3; 10; -7; 9; 10; -11; 13]);;
splitBySign([]);;
splitBySign([1;1;-1;-1;1;10;-11;11;12;14]);;

(*2*)
let rec lengthOfList(xs)=(
    if xs<>[] then 1+ lengthOfList(List.tl xs) else 0);;
lengthOfList([5;4;3;2]);;
lengthOfList([]);;
lengthOfList([1;2;3;4;5;6;7;8;9]);;

(*3*)
let rec joinList(xs, ys)=(
    match(xs, ys) with
      ([], []) -> []
     |([],_) -> ys
     |(_, []) -> xs
     |(_,_) -> List.hd xs :: List.hd ys :: joinList(List.tl xs,List.tl ys));;
joinList([5; 4; 3; 2], [1; 2; 3; 4; 5; 6]);;
joinList([1;2;3;4;5;6;7], [0;11]);;
joinList([], []);;
                                                                        
