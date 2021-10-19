let rec sum(xs)=(
  if xs=[] then 0
  else (List.hd xs)+sum(List.tl xs)
);;

sum([1;2;3]);;
sum([]);;
sum([-1;2;3]);;


let space  =" ";;

let rec sentence(xs,s) = (
  if xs= [] then s
  else space^ List.hd xs^ sentence(List.tl xs,s)
);;

sentence(["Nazywam";"sie";"Kuba"],"!");;
sentence(["Ala";"ma";"kota"],".");;
sentence([],"?");;

let rec isPositive(xs)= (
  if xs=[] then true
  else if List.hd xs <0 then false
  else isPositive(List.tl xs)
);;

isPositive([1;2;-3]);;
isPositive([1;2;3]);;
isPositive([-1;2;-3]);;

let rec factorial(n) = (
  if n<0 then failwith "nie istnieje silnia z liczby ujemnej"
  else if n=0 then 1
  else n*factorial(n-1)
);;

factorial(5);;
factorial(17);;
factorial(-1);;
