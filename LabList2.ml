(*Mateusz Pietrych*)

(*Zad_1*)


let sum n =
    let rec sumRec (n,accum) =
       if n=[] then accum else sumRec(List.tl n,accum+List.hd n)
in sumRec(n,0);;

sum(2::5::7::5::[]);;
sum(-10::5::-3::5::[]);;
sum([]);;


(*Zad_2*)

let listToString (n, a) =
    let rec listToStringRec (n,a,accum) =
       if n=[] then accum ^ a  else listToStringRec(List.tl n,a,accum^" "^List.hd n )
in if n <> [] then listToStringRec(List.tl n,a,List.hd n) else "";;

listToString("Ala"::"ma"::"kota"::[],"?");;
listToString([],"?");;


(*let listToString (n, a) =*)
(*   *)
(*in listToStringRec(n,a,"");;*)

(*Zad_3*)

 let rec listGreterThan0 n =
       if n=[] then true
       else  if List.hd n >0 then listGreterThan0(List.tl n)
       else false;;

 listGreterThan0(3::5::5::90::[]);;
 listGreterThan0(-1::5::5::90::[]);;
 listGreterThan0([]);;

(*Zad_4*)

 let factorial n =
     let rec factorialRec (n,accum) =
        if n=0 then accum else factorialRec(n-1,accum*n)
 in factorialRec(n,1);;

factorial(5);;
factorial(1);;
factorial(0);;

(*#use ”LabList2.ml”;;*)
