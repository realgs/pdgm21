let splitBySign(nums) = (
    let rec splitBySignIt(numbers,positiveOdd,negative)= (
      if numbers = [] then (positiveOdd,negative)
      else if List.hd numbers <0 then splitBySignIt(List.tl numbers,positiveOdd,negative@[List.hd numbers])
      else if List.hd numbers mod 2==1 then splitBySignIt(List.tl numbers,positiveOdd@[List.hd numbers],negative)
      else splitBySignIt(List.tl numbers,positiveOdd,negative)
    )
    in splitBySignIt(nums,[],[])
);;
splitBySign([1;-22;3;-41;-5;61]);;
splitBySign([12;23;3;4;51]);;
splitBySign([-13;-2;-3;-4;-51]);;
splitBySign([]);;



let lengthOfList(list) = (
  let rec lengthOfListIt(list,iter) = (
    if list=[] then iter
    else lengthOfListIt(List.tl list,iter+1)
  )
  in lengthOfListIt(list,0)
);;

lengthOfList([1;2;3]);;
lengthOfList(['s';'t';'a';'l';' ';'G';'o';'r';'z';'o';'w']);;
lengthOfList([]);;

let rec joinList(l1,l2) = (
  match (l1,l2) with
    ([],_)->l2
    | (_,[])->l1
    | (hl1::tl1,hl2::tl2)->hl1::(hl2::(joinList(tl1,tl2)))
);;
joinList([5;4;3;2],[1;2;3;4;5;6]);;
joinList([5;6],[1;2;3]);;
joinList([1;2;3],[5;6]);;
