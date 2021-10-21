let rec sum list =
    if list = [] then 0
    else List.hd list + sum(List.tl list);;

sum([1; 2; 3; 4]) = 10;;
sum([-15;4;3;8]) = 0;;

let space = " ";;

let rec makeSentence( list, endOfSentence) = 
     if list = [] then ""
      else 
        if List.tl list = [] then List.hd list ^ endOfSentence 
        else List.hd list ^ space ^ makeSentence(List.tl list, endOfSentence);;

makeSentence(["My"; "name"; "is"; "Yuliia"], ".");;

let rec isPositive list =
    if list == [] then true
    else if List.hd list > 0 then isPositive(List.tl list) else false ;;

isPositive([1; 2; 3; 4]) = true;;
isPositive([-15;4;3;8]) = false;;

let rec factorial n = 
    if n<= 0 then 0 
    else if n==1 then 1
    else n*factorial(n-1);;

factorial(5)=120;;
factorial(1)=1;;
factorial(0)=0;;