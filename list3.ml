let reverse xs = 
  let rec reverseRec (xs, x) =
    if xs = [] then x 
    else reverseRec(List.tl xs, List.hd xs::x)
  in reverseRec(xs,[]);;


(*Zadanie 1*)
let splitNumbers xs =
  let rec splitNumbersRec (xs, x, y)=
    if xs = [] then (x, y)
    else if List.hd xs<0 then splitNumbersRec(List.tl xs, List.hd xs::x, y)
    else if List.hd xs>0 && List.hd xs mod 2 = 1 then splitNumbersRec(List.tl xs, x, List.hd xs::y)
    else splitNumbersRec(List.tl xs, x, y)
  in splitNumbersRec(reverse(xs), [], []);;

splitNumbers([])=([],[]);;
splitNumbers([0;2;4;6;8;10])=([],[]);;
splitNumbers([0;1;2;3;4;5;6;7;8;9;-1;-2;-3;-4;-5])= ([-1; -2; -3; -4; -5], [1; 3; 5; 7; 9]);;

(*Zadanie 2*)
let lenght xs=
  let rec lenghtRec (xs, x)=
    if(xs = []) then x
    else lenghtRec(List.tl xs, x+1)
  in lenghtRec(xs, 0);;

lenght([])=0;;
lenght([1;2;3;4;5])=5;;
lenght(["a";"b";"c";"d"])=4;;


(*Zadanie 3*)

let rec joinLists (xs1, xs2) =
      match (xs1, xs2) with
      | (h1 :: t1, h2 :: t2) -> h1 :: h2 :: joinLists(t1, t2)
      | (_, []) -> xs1
      | ([], _) -> xs2
      | ([], [])-> []
;;

joinLists([],[]) = [] ;;
joinLists([],["1"]) = ["1"] ;;
joinLists(["1"],[]) = ["1"] ;;
joinLists([1;2;3],[4;5;6]) = [1;4;2;5;3;6] ;;

