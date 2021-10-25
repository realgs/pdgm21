(*zadanie1*)
let splitBySign(xs) = 
    let rec splitBySignIter(xs,plus,minus) =
        if xs = [] then (plus,minus)
        else if List.hd xs < 0 then splitBySignIter(List.tl xs,plus,minus@[List.hd xs])
        else if List.hd xs mod 2 = 0 then splitBySignIter(List.tl xs,plus,minus) 
        else splitBySignIter(List.tl xs,plus@[List.hd xs],minus) in
    splitBySignIter(xs,[],[])
;;

splitBySign([1;-2;3;-4;-5;6]);;
splitBySign([1;2;3;4;5]);;
splitBySign([-1;-2;-3;-4;-5]);;
splitBySign([]);;

(*zadanie2*)
let rec lengthOfList(xs) = 
    if xs=[] then 0
    else 1+lengthOfList(List.tl xs)
;;

lengthOfList([1;2;3]);;
lengthOfList(['a';'b';'c';'d']);;
lengthOfList([]);;

let lengthOfListR(xs) = 
    let rec lengthOfListIter(xs,x)=
        if xs=[] then x
        else lengthOfListIter(List.tl xs,x+1) in
    lengthOfListIter(xs,0)
;;

lengthOfListR([1;2;3]);;
lengthOfListR(['a';'b';'c';'d']);;
lengthOfListR([]);;


(*zadanie3*)
let rec joinLists(xs,ys) =
    match (xs,ys) with
        (hx::tx,hy::ty) -> hx::hy::joinLists(tx,ty)
        |([],_) -> ys
        |(_,[]) -> xs
;;

joinLists([5;6],[1;2;3]);;
joinLists([1;2;3],[5;6]);;
joinLists(['a';'b';'c'],['d';'e']);;

