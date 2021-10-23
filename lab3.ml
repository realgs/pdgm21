(*task 1*)

let splitBySign xs = 
    let rec splitBySignIn (xs , less, more) = 
        if xs = [] then (less,more)
        else if List.hd xs < 0 then splitBySignIn(List.tl xs , List.hd xs ::less , more) 
        else if List.hd xs > 0 && List.hd xs mod 2 <> 0 then splitBySignIn(List.tl xs ,less , List.hd xs :: more)
        else splitBySignIn(List.tl xs,less,more)
    in splitBySignIn (xs, [], [])

;;
splitBySign[1;2;-3;-5;-6;-7];;

(*task 2*)

let rec listLength xs = 
    if xs <> [] then 1+listLength(List.tl xs)
    else 0
;;

(*task 3*)
let rec concatLists (xs, ys) =
    match xs,ys with 
    | ([],_) -> ys
    | (_,[]) -> xs
    | (_,_) -> List.hd xs :: List.hd ys :: concatLists(List.tl xs,List.tl ys)
;;
concatLists([6;7;8;9],[1;2;3;4]);;
concatLists([],[]);;
concatLists([],[5;6;2;1]);;