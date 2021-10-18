(*Monika Jung*)

let list1 = 1::(-5)::8::4::(-3)::2::1::0::[];;
let list2 = 7::(-7)::9::(-8)::3::[];;
let list3 = 3::(-2)::(-1)::2::[];;
let wList1 = "a"::"b"::"c"::[];;
let wList2 = "s"::"t"::"u"::"w"::"x"::"y"::[];;

(*task 1 - assuming zero as positive*)
let createTwoListsF list =
    let rec recCreateTwoListsF (list, (listA, listB)) =
        if list = [] then (listA, listB)
        else if List.hd list < 0 then recCreateTwoListsF (List.tl list, (listA @ [List.hd list], listB))
        else recCreateTwoListsF (List.tl list, (listA, listB @ [List.hd list]))
    in recCreateTwoListsF (list, ([], []));;

createTwoListsF list1;;
createTwoListsF list2;;
createTwoListsF list3;;

(*task 2*)
let findListLengthF list =
    let rec recFindListLengthF (list, resultInt) =
        if list = [] then resultInt
        else recFindListLengthF (List.tl list, resultInt+1)
    in recFindListLengthF (list, 0);;

findListLengthF list1;;
findListLengthF list2;;
findListLengthF list3;;
findListLengthF wList1;;
findListLengthF wList2;;

(*task 3*)
let mergeTwoListsF (listA, listB) =
    let rec recMergeTwoListsF (listA, listB, resultList) =
        if listB = [] then resultList @ listA
        else if listA = [] then resultList @ listB
        else recMergeTwoListsF (List.tl listB, listA, resultList @ [List.hd listB])
    in recMergeTwoListsF (listA, listB, []);;

mergeTwoListsF (list1, list2);;
mergeTwoListsF (list2, list1);;
mergeTwoListsF (list2, list3);;
mergeTwoListsF (wList1, wList2);;