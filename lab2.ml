let rec sumListIteration = fun (intList, accumulator) ->
        if intList = [] then accumulator
        else sumListIteration (List.tl intList, accumulator + List.hd intList);;

let sumList = fun intList ->
    sumListIteration (intList, 0);;

sumList [1; 3; 5; 7; 9] = 25;;
sumList [] = 0;;
sumList [-2; -4; -6; -8; -10] = -30;;

let rec toSentenceIteration = fun (stringList, stop, result) ->
    if stringList = [] then result ^ stop
    else toSentenceIteration(List.tl stringList, stop, result ^ (if result = "" then "" else " ") ^ List.hd stringList);;

let toSentence = fun (stringList, stop) ->
    toSentenceIteration (stringList, stop, "");;

toSentence (["Ala"; "ma"; "kota"], ".") = "Ala ma kota.";;
toSentence (["Czy"; "Ala"; "ma"; "kota"], "?") = "Czy Ala ma kota?";;
toSentence ([], "") = "";;
toSentence (["Zdanie"; "bez"; "kropki"], "") = "Zdanie bez kropki";;
toSentence ([], "!") = "!";;

let rec areAllPositive = fun intList ->
    if intList = [] then true
    else if List.hd intList <= 0 then false
    else areAllPositive (List.tl intList);;

areAllPositive [1; 4; 10; 15] = true;;
areAllPositive [5; 0; 22] = false;;
areAllPositive [] = true;;

let rec factorialIteration = fun (number, accumulator) ->
    if number < 0 then raise(Invalid_argument "number cannot be negative")
    else if number = 0 || number = 1 then accumulator
    else factorialIteration (number-1, accumulator*number);;

let factorial = fun number ->
    factorialIteration (number, 1);;

factorial 0 = 1;;
factorial 1 = 1;;
factorial 5 = 120;;
