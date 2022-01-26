let splitBySign inputList =
    let rec splitBySignTailrec (inputList, result) =
        let myReverse inputList =
            let rec myReverseTailrec (inputList, accumulator) =
                if inputList = [] then accumulator
                else myReverseTailrec (List.tl inputList, (List.hd inputList) :: accumulator)
            in myReverseTailrec (inputList, [])
        in
        if inputList = [] then (myReverse (fst result), myReverse (snd result))
        else if List.hd inputList < 0 then splitBySignTailrec (List.tl inputList, (List.hd inputList :: fst result, snd result))
        else if List.hd inputList mod 2 = 1 then splitBySignTailrec (List.tl inputList, (fst result, List.hd inputList :: snd result))
        else splitBySignTailrec (List.tl inputList, (fst result, snd result))
    in splitBySignTailrec (inputList, ([], []));;

splitBySign [-3; -6; 7; -9; -13] = ([-3; -6; -9], [7; 13]);;
splitBySign [1; -1; 2] = ([-1], [1]);;
splitBySign [] = ([], []);;
splitBySign [0; 1; 2; 3; 4; 5] = ([], [1; 3; 5]);;

let lengthOfList inputList =
    let rec lengthOfListTailrec (inputList, accumulator) =
        if inputList = [] then accumulator
        else lengthOfListTailrec (List.tl inputList, accumulator + 1)
    in lengthOfListTailrec (inputList, 0);;

lengthOfList [5; 4; 3; 2] = 4;;
lengthOfList [] = 0;;
lengthOfList [['a'; 's'; 'd'; 'f']] = 1;;

let joinLists firstInputList secondInputList =
    let rec joinListsTailrec (firstInputList, secondInputList, accumulator, fromFirstList) =
        let myReverse inputList =
            let rec myReverseTailrec (inputList, accumulator) =
                if inputList = [] then accumulator
                else myReverseTailrec (List.tl inputList, (List.hd inputList) :: accumulator)
            in myReverseTailrec (inputList, [])
        in
        match (firstInputList, secondInputList) with
            (_, []) -> (myReverse accumulator) @ firstInputList
          | ([], _) -> (myReverse accumulator) @ secondInputList
          | _ -> if fromFirstList then joinListsTailrec (List.tl firstInputList, secondInputList, (List.hd firstInputList) :: accumulator, not fromFirstList)
                 else joinListsTailrec (firstInputList, List.tl secondInputList, (List.hd secondInputList) :: accumulator, not fromFirstList)
    in
    joinListsTailrec (firstInputList, secondInputList, [], true);;

joinLists [5; 4; 3; 2] [1; 2; 3; 4; 5; 6] = [5; 1; 4; 2; 3; 3; 2; 4; 5; 6];;
joinLists [] [] = [];;
joinLists [] [1; 2; 8; 9] = [1; 2; 8; 9];;
joinLists [3; 4; 6; 7] [] = [3; 4; 6; 7];;
