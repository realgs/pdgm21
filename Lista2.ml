(*Stanis³aw Izdebski*)

(*Zad. 1*)

let rec add xs= 
	
	match xs with
	|   []  -> 0
	|   h::t-> h+add(t)
;;

add([1;-2;-3;4;5]);;
add([5]);;
add([]);;


(*Zad. 2*)

let rec stringUniter(xs,s)=
	
	if s<>"." && s<>"!" && s<>"?" then raise(Failure"Niepoprawny znak konczacy zdanie")
	else	
  	match (xs,s) with
  	| ([],_) -> ""
  	| (h::[],_)-> h
  	| (h::t,_) -> h^" "^stringUniter(t,s)
;;

stringUniter(["One";"Ring";"to";"rule"; "them"; "all,"; "One" ;"Ring" ;"to" ;"find" ;"them,";"One" ;"Ring"; "to"; "bring" ;"them" ;"all"; "and" ;"in"; "the"; "darkness"; "bind" ;"them"],".");;
stringUniter([],".");;
(*stringUniter(["Something"],"g");;*)


(*Zad. 3*)
let rec aboveZeroChecker xs=
	
	match xs with
	| [] -> raise(Failure"Lista jest pusta")
	| h::t -> if h<=0 then false
	  else if t=[] then true 
		else aboveZeroChecker(t)
;;

aboveZeroChecker([1;2;3;4;5;6]);;
aboveZeroChecker([1;2;3;4;0;6]);;
aboveZeroChecker([4;7;1;3;-5]);;
(*aboveZeroChecker([]);;*)


(*Zad. 4*)

let rec factorial n=
	
	let rec factorialCounter(n,acc)=
		if n=0 then acc
		else factorialCounter(n-1,acc*n) 
	in
	if n<0 then raise(Failure"Ujemny argument")
	else factorialCounter(n,1) 
;;

factorial(0);;
factorial(20);;
(*factorial(-5);;*)
	
	