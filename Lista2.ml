(*Zadanie 1*)

let rec sumList xs=
  if xs = [] then failwith "Given list is empty!"
  else if List.tl xs == [] then List.hd xs
  else List.hd xs + sumList(List.tl xs);;


sumList([2;5;6]) = 13;;
sumList([0;-5;6;-8]) = -7;;
sumList([2]) = 2;;
(*sumList([]);;*)


(*Zadanie 2*)

let stringSeparator = " ";;

let rec appendListOfStrings (xs, x)=
  if x = "" then failwith "Missing sentence ending symbol!"
  else if xs = [] then x
  else if List.tl xs = [] then List.hd xs ^ x
  else List.hd xs ^ stringSeparator ^ appendListOfStrings(List.tl xs, x);;


appendListOfStrings(["Ala";"ma";"kota"], ".") = "Ala ma kota.";;
appendListOfStrings(["Ala"], "!") = "Ala!";;
appendListOfStrings([], ".") = ".";;
(*appendListOfStrings(["Ala";"ma";"kota"], "");;*)


(*Zadanie 3*)

let rec positivityOfList xs=
  if xs = [] then failwith "Given list is empty!"
  else if List.tl xs = [] then List.hd xs > 0.0
  else List.hd xs > 0.0 && positivityOfList(List.tl xs);;


positivityOfList([2.;5.;6.]) = true;;
positivityOfList([2.;-5.;6.]) = false;;
positivityOfList([2.3;5.6;6.9]) = true;;
positivityOfList([-2.7;-5.7;6.1]) = false;;
positivityOfList([-2.]) = false;;
positivityOfList([2.]) = true;;
(*positivityOfList([]);;*)


(*Zadanie 4*)

(*let rec numFactorial x=
  if x<0 then failwith "Podano ujemna liczbe!"
  else if x == 0 then 1
  else x * numFactorial(x-1);;*)


(*Rekursja ogonowa*)

let numFactorial x=
  if x < 0 then failwith "Podano ujemna liczbe!"
  else
    let rec numFactorialIter (x, ac)=
      if x = 0 then ac 
      else numFactorialIter(x - 1, x * ac)
    in numFactorialIter(x, 1);;


numFactorial(4) = 24;;
numFactorial(1) = 1;;
numFactorial(0) = 1;;
(*numFactorial(-6);;*)


