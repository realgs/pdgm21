(* Kacper Wójcicki *)

(* 1 *)
let rec sumOfListNums xs = 
  if xs = [] then 0
  else List.hd xs + sumOfListNums(List.tl xs);;

sumOfListNums([1;2;3]) = 6;;
sumOfListNums([]) = 0;;
sumOfListNums([-5;-3;-10]) = -18;;

(* 2 *)
let makeSentence words endChar =
  let rec makeSentenceIn words  endChar result = 
    if words = [] then raise(Failure "Sentence end char not found in words list!")
    else if List.hd words = endChar then result ^ endChar
    else if compare result "" = 0 then makeSentenceIn (List.tl words)(endChar)(List.hd words)
    else makeSentenceIn (List.tl words)(endChar)( (result ^ " ") ^ (List.hd words) ) in
  if words = [] then ""
  else makeSentenceIn words endChar "";;

makeSentence(["Hello"; "World"; "!"])("!") = "Hello World!";;
makeSentence([])("!") = "";;
makeSentence(["!"])("!") = "!";;

(* 3 *)
let rec isBiggerThanZero xs = 
  if xs = [] then true
  else if List.hd xs <= 0 then false
  else isBiggerThanZero(List.tl xs);;

isBiggerThanZero([1;12341423;2134321]) = true;;
isBiggerThanZero([]) = true;;
isBiggerThanZero([0]) = false;;

(* 4 *)
let rec myFactorial number = 
  if number < 0 then raise(Failure "Invalid argument!")
  else if number = 1 || number = 0 then 1
  else number * myFactorial(number - 1);;

myFactorial(6) = 720;;
myFactorial(1) = 1;;
myFactorial(3) = 6;;
myFactorial(0) = 1;;
