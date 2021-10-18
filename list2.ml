(*task 1*)

let addElements xs =
  if xs = [] then raise (Failure "empty list")
  else
    let rec addElementsRec(xs, result) =
      if xs = [] then result
      else addElementsRec(List.tl xs, result + List.hd xs)
    in addElementsRec(xs, 0);;


addElements([1;1;1])=3;;
addElements([1;-2;-1;-5])=(-7);;
addElements([1;-1;-1;1])=0;;
(*addElements([]);;*)


(*task 2*)

let makeSentence (xs, x) =
  if xs = [] then raise (Failure "empty list")
  else if ((x <> "!")&&(x <> "?")&&(x <> ".")) then raise (Failure "bad ending sign")
  else
    let rec makeSentenceRec (xs, x) =
      if List.tl xs  = [] then List.hd xs ^ x
      else List.hd xs ^ " " ^ makeSentenceRec(List.tl xs, x)
    in makeSentenceRec(xs,x);;


makeSentence(["Ala";"ma";"kota"],".") = "Ala ma kota.";;
makeSentence(["Ala";"ma";"kota"],".") = "Ala ma kota.";;
makeSentence(["Ala";"ma";"kota"],"!") = "Ala ma kota!";;
makeSentence(["Jaka";"jest";"pogoda";"we";"Wroclawiu"],"?") = "Jaka jest pogoda we Wroclawiu?";;
(*makeSentence([],".");;
makeSentence(["Ala";"ma";"kota"]," ");;
makeSentence(["Ala";"ma";"kota"],"..");;*)


(*task 3*)

let greaterThanZero xs =
  if xs = [] then raise (Failure "empty list")
  else
    let rec greaterThanZeroRec xs =
      if List.hd xs <= 0. then false
      else if List.tl xs = [] then true
      else greaterThanZeroRec(List.tl xs)
    in greaterThanZeroRec(xs);;         

greaterThanZero([4.;3.;2.0]) = true;;
greaterThanZero([-4.;3.;2.0]) = false;;
greaterThanZero([3.;3.;-6.]) = false;;
(*greaterThanZero([]);;*)


(*task 4*)

let factorial n =
  if n<0 then raise (Failure "wrong input: negative number")
  else if n>12 then raise (Failure "wrong input: number > 12")
  else
    let rec factorialRec (n,result) =
      if n = 0 then result
      else factorialRec(n-1, n*result)
    in factorialRec(n,1);;         


factorial(0) = 1;;
factorial(1) = 1;;
factorial(10) = 3628800;;
factorial(12) == 479001600;;
(*factorial(-5);;
factorial(13);;*)
