let reverse lista =
    let rec rev lista wynik =
        if lista = [] then wynik else rev (List.tl lista) (List.hd lista :: wynik)
    in rev lista [];;

(* zad 1 *)
let splitBySign lista = 
    let rec splitIter lista neg pos =
        match lista with
            [] -> (neg, pos)
            | h::t -> if h < 0 then splitIter t (h::neg) pos
                else if h > 0 && h mod 2 = 1 then splitIter t neg (h::pos)
                else splitIter t neg pos
    in splitIter (reverse lista) [] [];;

(* testy do 1 *)
splitBySign [-3;-6;7;-9;13] = ([-3; -6; -9], [7; 13]);;
splitBySign [] = ([], []);;
splitBySign [0; -1; 1; 1; -1; 0; 2; -2; -2; 2; 0] = ([-1; -1; -2; -2], [1; 1]);;

(* zad 2 *)
let lengthOfList lista =
    let rec len lista n =
        if lista = [] then n else len (List.tl lista) (n+1)
    in len lista 0;;

(* testy do 2 *)
lengthOfList [5; 4; 3; 2] = 4;;
lengthOfList [] = 0;;
lengthOfList ["Koperek"] = 1;;

(* zad 3 *)
let joinLists listaL listaP =
    let rec join listaL listaP wynik np =
        match (listaL, listaP, np) with
            ([], [], _) -> reverse wynik
            | (h1::t1, [], _) -> join t1 listaP (h1::wynik) false
            | (h1::t1, h2::t2, true) -> join t1 listaP (h1::wynik) false
            | ([], h2::t2, _) -> join listaL t2 (h2::wynik) true
            | (h1::t1, h2::t2, false) -> join listaL t2 (h2::wynik) true
    in join listaL listaP [] true;;
            
(* testy do 3 *)
joinLists [5;4;3;2] [1;2;3;4;5;6] = [5;1;4;2;3;3;2;4;5;6];;
joinLists [3.2; 5.6; 1.1] [] = [3.2; 5.6; 1.1];;
joinLists [] ["Pomysłowe słowo 1"; "Pomysłowe słowo 2"; "Pomysłowe słowo 3"] = ["Pomysłowe słowo 1"; "Pomysłowe słowo 2"; "Pomysłowe słowo 3"];;
joinLists [] [] = [];;
