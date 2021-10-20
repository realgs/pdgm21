(*zad1*)
let rec sumOfElements list  =
  if list == [] then 0
  else List.hd list + sumOfElements (List.tl list);;

sumOfElements [1;2;3;4] = 10;;
sumOfElements [] = 0;;
sumOfElements [1;2;-3;4] = 4;;

(*zad2*)
let separator = " ";;

let rec makeSentence (list, char) =
  if list = [] then char
  else if List.tl list = [] then List.hd list ^ makeSentence(List.tl list, char)
  else  List.hd list ^ separator ^ makeSentence(List.tl list, char);;
           
makeSentence (["Ala";"ma";"kota"],".") = "Ala ma kota.";;
makeSentence(["Ola"; "ma"; "kota"], "?") = "Ola ma kota?";;
makeSentence(["Kot"],"!") = "Kot!";;
makeSentence([], " ") = " ";;

(*zad3*)
let rec isPositive list =
  if list = [] then false
  else if List.tl list = [] then List.hd list > 0
  else if List.hd list > 0 then isPositive(List.tl list)
  else false;;

isPositive [1;2;3;4] = true;;
isPositive [1;-3] = false;;
isPositive [] = false;;
 
(*zad4*)
let rec factorial n =
  if n < 0 then failwith "invalid argument"
  else if n = 0 || n = 1  then 1
  else n * factorial(n-1);;
                    
factorial 3 = 6;;
factorial 10 = 3628800;;
factorial (-2);;



