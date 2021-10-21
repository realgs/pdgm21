(*zadanie 1*)
let rec add xs =
  if xs = [] then 0
  else List.hd xs + add(List.tl xs);;

add([1;2;3])=6;;
add([])=0;;
add([-10;2])= -8;;

(*zadanie 2*)
let rec sentence (xs,x) =
  if xs <> [] then List.hd xs ^ " " ^ sentence(List.tl xs,x)
  else x;;
 

sentence(["Hello";"world"],"!");;
sentence(["a";"b";"c"],".");;
sentecnce([],"?");;

(*zadanie 3*)
let rec  moreThanZero xs =
  if xs = [] then failwith "The list is empty"
  else if List.tl xs = [] then List.hd xs > 0
  else List.hd xs > 0 &&  moreThanZero(List.tl xs)
  ;;

moreThanZero([-1;2]);;
moreThanZero([1;2;3;0]);;
moreThanZero([]);;
moreThanZero([1;2;3]);;

(*zadanie 4*)
let rec factorial x =
  if x = 0 then 1
  else if x > 0 then x*factorial(x-1)
  else failwith "Negative argument"
;;

factorial(0);;
factorial(-2);;
factorial(5);;
