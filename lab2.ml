let rec sum list=
    if list = [] then 0.0
    else List.hd list +. sum (List.tl list);;

sum([1.;2.;3.;4.]) = 10.;;
sum([]) = 0.;;
sum([-1.;1.]) = 0.;;

let rec makeSentence (list,endPhrase)  =
    if list = [] then failwith("List can't be empty empty")
    else if (List.tl list) = [] then List.hd list ^ endPhrase
    else (List.hd list)^ " " ^ makeSentence(List.tl list, endPhrase);;

makeSentence(["."],".") = ".";;
makeSentence(["Ala";"ma";"kota"],"!") = "Ala ma kota!";;
makeSentence(["Ala";"ma";".";"kota"],".") = "Ala ma . kota.";;

let rec checkIfPositive list =
    if list = [] then true
    else if List.hd list > 0. then checkIfPositive(List.tl list)
    else false;;

checkIfPositive[5.;3.;-1.] = false;;
checkIfPositive[1.;2.;3.] = true;;
checkIfPositive[0.] = false;;

let factorial number =
    let rec factorialInternal (value,sum) =
        if value = 0 then sum
        else factorialInternal(value - 1, value * sum)
    in factorialInternal(number,1);;

factorial 0 = 1;;
factorial 1 = 1;;
factorial 5 = 120;;