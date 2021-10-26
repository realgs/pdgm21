let reverseList list =
    let rec reverseListHelper (srcList, destList) =
        if srcList = [] then destList
        else reverseListHelper((List.tl srcList), ((List.hd srcList)::destList))
    in reverseListHelper (list,[]);;


let splitBySign list =
    let rec splitHelper (srcList, negative, oddPositive) =
        if srcList = [] then [reverseList negative, reverseList oddPositive]
        else if (List.hd srcList) < 0 then splitHelper(List.tl srcList, (List.hd srcList) :: negative, oddPositive)
        else if((List.hd srcList) > 0 && (List.hd srcList) mod 2 == 1) then splitHelper(List.tl srcList, negative, (List.hd srcList)::oddPositive)
        else splitHelper(List.tl srcList, negative, oddPositive)
    in splitHelper(list,[],[]);;

splitBySign [-3;-6;7;-9;13];;
splitBySign [];;
splitBySign [1;2;3;4;5;6];;


let listLength list =
    let rec listLengthHelper(list, sum) =
        if list == [] then sum
        else  listLengthHelper(List.tl list, sum + 1)
    in listLengthHelper(list,0);;

listLength([0;1;2;3;4]);;
listLength([]);;
listLength(["A";"B";"C"]);;


let rec joinLists(first, second) =
    match (first,second) with
        |([],_) -> second
        |(_,[]) -> first
        |(hFirst::tFirst,hSecond::tSecond) -> hFirst::hSecond::joinLists(List.tl first, List.tl second);;

joinLists([5;4;3;2],[1;2;3;4;5;6]);;
joinLists([],[1;2;3;4]);;
joinLists(["A";"B";"C"],[]);;
