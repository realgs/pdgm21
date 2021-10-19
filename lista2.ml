(*Justyna Stankiewicz*)

(*Zadanie 1*)
let rec sum(list)=
    if list=[] then 0
    else List.hd list+sum(List.tl list)
;;
sum([1;2;3;-4])=2;;
sum([1;2;3])=6;;
sum([])=0;;

(*Zadanie 2*)
let rec sentence(list)=
    if list=[] then ""
    else if List.hd list="?" then "."
    else " "^List.hd list^sentence(List.tl list)
;;
sentence(["Ala";"ma";"kota";"?"])=" Ala ma kota.";;
sentence([])="";;
sentence(["Ala?";"ma";"kota";"?"])=" Ala? ma kota.";;

(*Zadanie 3*)
let rec isPositive(list)=
    if list=[] then false
    else if List.hd list<=0 then false
    else
        if List.tl list=[] then true
        else isPositive(List.tl list)
;;
isPositive([2;4;3])=true;;
isPositive([])=false;;
isPositive([-1;0;2])=false;;

(*Zadanie 4*)
let rec factorial(n)=
    if n<0 then failwith("blad")
    else if n==0 then 1
    else n*factorial(n-1)
;;
factorial(4)=24;;
factorial(0)=1;;
(*factorial(-1);; zostanie wyrzucony blad*)

