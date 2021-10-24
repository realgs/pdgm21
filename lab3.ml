(*Jakub Koalczyk*)

let reverse(list) = 
  let rec go (list, output) = 
    if list == [] then output
    else go(List.tl list, (List.hd list)::output) in
  go(list, []);;

(*Zadanie 1*)

let splitBySign(list) =
  let rec go1(list, negativeNumbers, positiveOddNumbers) =
    if list == [] then (reverse(negativeNumbers), reverse(positiveOddNumbers))
    else if List.hd list < 0 then go1(List.tl list, (List.hd list)::negativeNumbers, positiveOddNumbers)
    else if (List.hd list > 0 && List.hd list mod 2 == 1) then go1(List.tl list, negativeNumbers, (List.hd list)::positiveOddNumbers)
    else go1(List.tl list, negativeNumbers, positiveOddNumbers) in
  go1(list, [], []);;

splitBySign([-3;-6;7;-9;13]);;

(*Zadanie 2*)

let lengthOfList(list) =
  let rec go2(list, a) =
    if list == [] then a
    else go2(List.tl list, a+1) in
  go2(list, 0);;

lengthOfList([5;4;3;2]);;

(*Zadanie 3*)

let joinLists(listA, listB) =
  let rec go3(listA, listB, output, bool) =
    if(listA == [] && listB == []) then reverse(output)
    else if listA == [] then go3(listA, List.tl listB, (List.hd listB)::output, true)
    else if listB == [] then go3(List.tl listA, listB, (List.hd listA)::output, true)
    else if bool == true then go3(List.tl listA, listB, (List.hd listA)::output, false)
    else go3(listA, List.tl listB, (List.hd listB)::output, true) in
  go3(listA, listB, [], true);;

joinLists([5;4;3;2], [1;2;3;4;5;6]);;
