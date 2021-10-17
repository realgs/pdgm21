(*task 1*)
    let rec addListElements xs =
      if xs = [] then 0
      else addListElements(List.tl xs) + List.hd xs;;

    addListElements[] = 0;;
    addListElements[0] = 0;;
    addListElements[1; -2; -10; 5] = -6;;
    addListElements[10; 2; 5; 3] = 20;;
    
(*task 1 tail recursion*)
    let addListElementsTail xs =
      let rec addListElementsIter (xs, accum) =
        if xs = [] then accum
        else addListElementsIter(List.tl xs, accum + (List.hd xs))
    in addListElementsIter(xs, 0);;
         
    addListElementsTail[] = 0;;
    addListElementsTail[0] = 0;;
    addListElementsTail[1; -2; -10; 5] = -6;;
    addListElementsTail[10; 2; 5; 3] = 20;;

(*task 2*)
    let charInSentence = " ";;
    
    let rec createSentence (xs, x) =
      if xs = [] then x
      else if createSentence(List.tl xs, x) <> x then List.hd xs ^ charInSentence ^ createSentence(List.tl xs, x)
      else List.hd xs ^ createSentence(List.tl xs, x);;

    createSentence([], ".") = ".";;
    createSentence(["Aliens"], "!") = "Aliens!";;
    createSentence(["Does"; "free"; "will"; "exist"], "?") = "Does free will exist?";;
    createSentence(["Vanitas"; "vanitatum"; "et"; "omnia"; "vanitas"], ".") = "Vanitas vanitatum et omnia vanitas.";;

(*task 2 tail recursion*)
    let createSentenceTail (xs, x) =
      let rec createSentenceIter (xs, x, accum) =
        if xs = [] then accum ^ x
        else if List.tl xs = [] then createSentenceIter(List.tl xs, x, accum ^ List.hd xs)
        else createSentenceIter(List.tl xs, x, accum ^ List.hd xs ^ charInSentence)
    in createSentenceIter(xs, x, "");;         

    createSentenceTail([], ".") = ".";;
    createSentenceTail(["Aliens"], "!") = "Aliens!";;
    createSentenceTail(["Does"; "free"; "will"; "exist"], "?") = "Does free will exist?";;
    createSentenceTail(["Vanitas"; "vanitatum"; "et"; "omnia"; "vanitas"], ".") = "Vanitas vanitatum et omnia vanitas.";;

(*task 3*)
    let rec isListPositive xs =
      if xs = [] then true
      else if List.hd xs <= 0.0 then false
      else isListPositive(List.tl xs);;

    isListPositive[] = true;;
    isListPositive[0.] = false;;
    isListPositive[10.; 2.5; -1.2] = false;;
    isListPositive[110.; 22.1; 11.2] = true;;
    
(*task 4*)
    let rec factorial n =
      if n = 0 then 1 else n * factorial(n - 1);;

    factorial 0 = 1;;
    factorial 1 = 1;;
    factorial 5 = 120;;
    factorial 12 = 479001600;;

(*task 4 tail recursion*)
    let factorialTail n =
      let rec factorialIter (n, accum) =
        if n = 0 then accum else factorialIter(n - 1, n * accum)
    in factorialIter(n, 1);;
    
    factorial 0 = 1;;
    factorial 1 = 1;;
    factorial 5 = 120;;
    factorial 12 = 479001600;;
