let rec zad1 list =
    if list != [] then  List.hd list + zad1(List.tl list)
    else 0;;

zad1([]) = 0;;
zad1([1; 2 ;0 ;3 ;4]) = 10;;
zad1([1; -1; 2; -2]) = 0;;

let rec zad2 (list,c) =
    if  list = [] then c
    else if List.tl list = [] then List.hd list ^ c
    else List.hd list ^ "_" ^ zad2(List.tl list,c);;

zad2([],":") = ":";;
zad2(["Pora";"na";"przygode"],"!") = "Pora_na_przygode!";;
zad2(["Garnek"],"!") = "Garnek!"

let rec zad3 l =
    if l = [] then true
    else if List.hd l > 0 then zad3(List.tl l)
    else false;;

zad3([3; 4; 5; 6]) = true;;
zad3([0; 21; 332]) = false;;
zad3([-1; 4 ;2137]) = false;;
zad3([]) = true;;

let rec zad4 n =
    if n<0 then failwith "Bad number"
    else if n = 0 then 1
    else if n = 1 then 1
    else n * zad4(n-1);;

zad4(5) = 120;;
zad4(0) = 1;;
zad4(1) = 1;;
zad4(-1);;

