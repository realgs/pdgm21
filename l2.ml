(*zad 1*)
let rec sum list =
  if list = [] then 0 else List.hd list + sum(List.tl list);;

sum([1; 2; 3; 4; 5]);;
sum([-2; -1; 0; 5; -6; 3]);;
sum([]);;

(*zad 2*)
let endOfSentence s =
  if s = "." || s = "?" || s = "!" then true else false;;

let rec sentence list =
  if list = [] then failwith "Empty list!"
  else if endOfSentence(List.hd list) then List.hd list
  else List.hd list ^
         (if List.tl list = [] then failwith "The list has no end!"
          else if endOfSentence(List.hd (List.tl list)) = false  then " " else "")
         ^ sentence(List.tl list);;

sentence(["Ala"; "ma"; "kota"; "."]);;
sentence(["1"; "2"; "3"; "!"]);;
sentence(["Ala"; "ma"; "kota"]);;
sentence([]);;

(*zad 3*)
let rec ifGreaterThen0 list =
  if list = [] then true
  else if List.hd list > 0 && ifGreaterThen0(List.tl list) then true else false;;

ifGreaterThen0([1; 2; 3; 4]);;
ifGreaterThen0([-1; 2; 3; 4]);;
ifGreaterThen0([]);;

(*zad 4*)
let rec factorial n =
  if n < 0 then failwith "Negative number!"
  else if n = 0 then 1 else n * factorial(n - 1);;

factorial(5);;
factorial(0);;
factorial(-2);;
