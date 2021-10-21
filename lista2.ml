(* Szymon Anikiej *)


(* zadanie 1 *)
let suma (p1, p2) = p1+p2;;

(* testy do 1 *)
suma(1, 5);;
suma(0, 0);;
suma(-13, 8);;
suma(-9, -9);;


(* zadanie 2 *)
let sp = " ";;
let dot = ".";;
let zdanie (s1, s2) = s1 ^ sp ^ s2 ^ dot;;


(* testy do 2 *)
zdanie("Ala ma", "kota");;
zdanie("", "");;
zdanie("K$1wsa798%as", "");;


(* zadanie 3 *)
let rec dodatnie li =
    if li = [] then true
    else if List.hd li <= 0 then false
    else dodatnie (List.tl li);;

(* testy do 3 *)
dodatnie [1; 3; 2];;
dodatnie [0; 4];;
dodatnie [16; -9];;
dodatnie [];;


(* zadanie 4 *)
let rec silnia n =
    if n <= 1 then 1 (* dla nieprawidłowych argumentów ujemnych silni funkcja zwróci 1 *)
    else n * silnia (n-1);;

(* testy do 4 *)
silnia 5;;
silnia 1;;
silnia 0;;
silnia (-4);;
