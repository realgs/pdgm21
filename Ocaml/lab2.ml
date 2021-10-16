(*Task 1*)
let rec sum list =
  if list = [] then 0
  else List.hd list + sum (List.tl list)
;;

(*Helper for task 2*)
let rec isSequenceCorrect list =
  if list = [] then false
  else if List.tl list = ["."] || List.tl list = ["?"] || List.tl list = ["!"] then true
  else isSequenceCorrect (List.tl list)
;;

(*Task 2*)
let rec concatenateStrings list =
  let rec appendWords list =
    if List.hd list = "." || List.hd list = "?" || List.hd list = "!" then List.hd list
    else " " ^ List.hd list ^ appendWords(List.tl list)
  in

  if isSequenceCorrect list <> true then raise(Failure "Incorrect sequence")
  else List.hd list ^ appendWords(List.tl list)
;;

(*Task 3*)
let rec isPositive list =
  if list = [] then true
  else if List.hd list <= 0 then false
  else isPositive(List.tl list)
;;

(*Task 4*)
let rec calculateFactorial n =
  if n < 0 then raise(Failure "Cannot calculate negative factorial")
  else if n = 0 then 1
  else n * calculateFactorial(n-1)
;;

print_int (sum [1; 3; 6]);;
print_int (sum [-4; 89; 1; -56]);;
print_int (sum []);;

print_string (concatenateStrings ["Hello"; "World"; "!"]);;
print_string (concatenateStrings ["Did"; "I"; "code"; "this"; "task"; "properly"; "?"]);;
print_string (concatenateStrings ["You"; "are"; "wonderful"; "."]);;
(*Will print error*)
(*concatenateStrings [];;*)
(*concatenateStrings ["."; "Hello"];;*)

print_string (string_of_bool (isPositive [1; 5; 7]));;
print_string (string_of_bool (isPositive [5; -2; 3]));;
print_string (string_of_bool (isPositive []));;

print_int (calculateFactorial(0));;
print_int (calculateFactorial(1));;
print_int (calculateFactorial(5));;
(*Will throw an exception*)
(*print_int (calculateFactorial(-1));;*)
