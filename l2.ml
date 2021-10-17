(*Paulina Drzazga, 260370*)

(*Task 1*)

let rec sum x = 
  if x=[] then 0
  else (List.hd x)+sum(List.tl x);;

sum [10; 15] = 25;;
sum [-100; 1000;345;46;23;43;-34] = 1323;;
sum []= 0;;

(*Task 2*)

let sep=" "

let rec sentence ls char =
  if ls=[] then char
  else (List.hd ls)^sep^sentence (List.tl ls) char;;


  sentence ["My"; "name"; "is"; "Paulina"] "." ="My name is Paulina .";;
  sentence [] "!" ="!";;
  sentence ["I"; "love"; "programming"] "" ="I love programming ";;

(*Task 3*)

let rec positiveArguments ls=
  if ls = [] then true
  else 
    if List.hd ls > 0 then positiveArguments (List.tl ls)
    else false;;
  

positiveArguments [1;2;3;4;5;6;7;8;9;10] =true;;
positiveArguments [1;2;3;4;5;6;7;8;9;-10] =false;;
positiveArguments []=true;;
positiveArguments [-1;-2;-3;4;5;6;7;8;9;10] =false;;


(*Task 4*)

let rec factorial n=
  if n<0 then raise(Failure "Negative argument")
  else if n=0 then 1
  else factorial(n-1)*n;;


factorial 5 = 120;;
factorial 6 = 720;;
factorial 0 = 1;;
(*factorial (-10);;    FAILURE*)
