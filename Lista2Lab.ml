let rec sumOfList(list) = (
    if list = [] then 0
    else List.hd list + sumOfList(List.tl list)
  );;

(sumOfList([1;2;3;4])=10);;
  (sumOfList([1;-2;-3;4])=0);;
  (sumOfList([])=0);;
(sumOfList([5;5;5;5;-5])=15);;

let rec length(list) = (
    if list = [] then 0 else 1 + length(List.tl list)
  );;

(length([])=0);;
  (length([1;2;3;4;5])=5);;
  (length([1])=1);;
  (length([-1;-2;-3;-4])=4);;

  let rec createSentence(list, character) = (
      match list with
      | [] -> failwith "Pusta lista, proszę podać słowa z których ułożę zdanie"
      | h::t -> if t=[] then  h^character else h ^ " " ^ createSentence(t, character)    
  );;

createSentence(["Marian";"ma";"dywan"], ".");;
createSentence(["Marian";"ma";"miotłe";],".");;
createSentence(["Ala"], ".");;
createSentence([],".");;

let rec areElementsPositive(list) = (
    if list = [] then true
    else if List.hd list > 0 then areElementsPositive(List.tl list)
         else false
  );;

(areElementsPositive([])=true);;
  (areElementsPositive([1;2;3;4;5])=true);;
  (areElementsPositive([1;2;3;-4;5])=false);;
  (areElementsPositive([-1;-2;-3;-4;-5])=false);;

  
let rec factorial(x) = (
  if x>1 then x*factorial(x-1)
  else 1
);;

(factorial(1)=1);;
  (factorial(0)=1);;
  (factorial(2)=2);;
  (factorial(5)=120);;


