(*exercise 1*)
let rec sum nrList = 

  if nrList = [] then 0
  else List.hd nrList + sum (List.tl nrList)
;;

print_int (sum [2; 5; 3]);;       (*= 10*)  print_string "\n";;
print_int (sum []);;              (*= 0*)   print_string "\n";;
print_int (sum [-2; 2; -5; 5]);;  (*= 0*)   print_string "\n";;

(*exercise 2*)
let wordsSeparator = " ";;
let rec makeSentence (strList, sign) = 

  if strList = [] then ("" ^ sign)
  else if makeSentence(List.tl strList, sign) = ("" ^ sign) then (List.hd strList) ^ makeSentence(List.tl strList, sign)
  else (List.hd strList) ^ wordsSeparator ^ makeSentence(List.tl strList, sign)
;;

print_string (makeSentence(["Let's"; "play"; "a game"], "."));; print_string "\n";;
print_string (makeSentence(["egzample"], "."));;                print_string "\n";;
print_string (makeSentence([], "?"));;                          print_string "\n";;

(*exercise 3*)
let rec checkPositive nrList =

  if nrList = [] then true
  else if (List.hd nrList) <= 0.0 then false
  else checkPositive (List.tl nrList)
;;

checkPositive [1.; 2.; 2.; 1.; 35254.];;
checkPositive [];;
checkPositive [0.];;
checkPositive [2.5; -2.; 0.66];;

(*exercise 4*)
let rec factorial n = 

    if n < 0 then failwith "Factorial from negative number undefined"
    else if n = 0 then 1
    else n * factorial (n - 1)
;;

print_int (factorial 5);; print_string "\n";;
print_int (factorial 0);; print_string "\n";;
print_int (factorial 1);; print_string "\n";;
(*factorial -2;;*) (*throws exception*)