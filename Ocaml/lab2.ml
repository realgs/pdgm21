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

print_int (sum [1; 3; 6]);;
print_int (sum [-4; 89; 1; -56]);;
print_int (sum []);;

print_string (concatenateStrings ["Hello"; "World"; "!"]);;
print_string (concatenateStrings ["Did"; "I"; "code"; "this"; "task"; "properly"; "?"]);;
print_string (concatenateStrings ["You"; "are"; "wonderful"; "."]);;
(*Will print error*)
(*concatenateStrings [];;*)
(*concatenateStrings ["."; "Hello"];;*)
