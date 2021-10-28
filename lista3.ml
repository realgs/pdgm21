let reverse xs =
    let rec rev xs ys =
        if xs = [] then ys else rev (List.tl xs) (List.hd xs :: ys)
    in rev xs [];;

(* zad 1 *)
let splitBySign xs = 
    let rec splitIter xs neg pos =
        match xs with
            [] -> (neg, pos)
            | h::t -> if h < 0 then splitIter t (h::neg) pos
                else if h > 0 && h mod 2 = 1 then splitIter t neg (h::pos)
                else splitIter t neg pos
    in splitIter (reverse xs) [] [];;

(* testy do 1 *)
splitBySign [-3;-6;7;-9;13] = ([-3; -6; -9], [7; 13]);;
splitBySign [] = ([], []);;
splitBySign [0; -1; 1; 1; -1; 0; 2; -2; -2; 2; 0] = ([-1; -1; -2; -2], [1; 1]);;

(* zad 2 *)
let lengthOfList xs =
    let rec len xs n =
        if xs = [] then n else len (List.tl xs) (n+1)
    in len xs 0;;

(* testy do 2 *)
lengthOfList [5; 4; 3; 2] = 4;;
lengthOfList [] = 0;;
lengthOfList ["Koperek"] = 1;;

(* zad 3 *)
let joinLists xs ys =
    let rec join xs ys zs np =
        match (xs, ys, np) with
            ([], [], _) -> reverse zs
            | (h1::t1, [], _) -> join t1 ys (h1::zs) false
            | (h1::t1, h2::t2, true) -> join t1 ys (h1::zs) false
            | ([], h2::t2, _) -> join xs t2 (h2::zs) true
            | (h1::t1, h2::t2, false) -> join xs t2 (h2::zs) true
    in join xs ys [] true;;
            
(* testy do 3 *)
joinLists [5;4;3;2] [1;2;3;4;5;6] = [5;1;4;2;3;3;2;4;5;6];;
joinLists [3.2; 5.6; 1.1] [] = [3.2; 5.6; 1.1];;
joinLists [] ["Pomysłowe słowo 1"; "Pomysłowe słowo 2"; "Pomysłowe słowo 3"] = ["Pomysłowe słowo 1"; "Pomysłowe słowo 2"; "Pomysłowe słowo 3"];;
joinLists [] [] = [];;
