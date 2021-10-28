(*Zadanie 1*)
(*własne reverse*)
let reverse(list)=
    let rec reverseIn(list,revList)=
        match list with
            []->revList
            |head::tail->reverseIn(List.tl list,head::revList)
    in reverseIn(list,[])
;;
reverse([13;-9;7;-6;-3])=[-3;-6;7;-9;13];;
reverse([])=[];;
reverse(["kota";"ma";"Ala"])=["Ala";"ma";"kota"];;
reverse([1])=[1];;

let splitBySign(list)=
    let rec splitBySignIn(list, negativeNum, positiveNum)=
        match list with
            []->(negativeNum,positiveNum)
            |(head::tail)->
                            if head<0 then splitBySignIn(tail,head::negativeNum,positiveNum)
                            else if (head>0 && head mod 2 ==1) then splitBySignIn(tail,negativeNum,head::positiveNum)
                            else splitBySignIn(tail,negativeNum,positiveNum)
    in splitBySignIn(reverse(list),[],[]) 
;;
splitBySign([-3;-6;7;-9;13])=([-3;-6;-9],[7;13]);;
splitBySign([0;2;1;-4;-1])=([-4;-1],[1]);;
splitBySign([])=([],[]);;
splitBySign([-1;-2;-3;-1])=([-1;-2;-3;-1],[]);;
splitBySign([1;3;5])=([],[1;3;5]);;                           

(*Zadanie 2*)
let rec lengthOfList(list)=
    if list==[] then 0
    else 1+lengthOfList(List.tl list)
;;
lengthOfList[5;4;3;2]=4;;
lengthOfList[]=0;;
lengthOfList["Ala";"ma";"kota"]=3;;

(*Zadanie 3*)
let rec joinLists(list1,list2)=
    match(list1,list2) with
        ([],[])->[]
        |(head::tail,[])->head::joinLists(tail,[])
        |([],head::tail)->head::joinLists([],tail)
        |(head1::tail1,head2::tail2)->head1::head2::joinLists(tail1,tail2)
;;
joinLists([5;4;3;2],[1;2;3;4;5;6])=[5;1;4;2;3;3;2;4;5;6];;
joinLists([],[])=[];;
joinLists([1;2;3],[])=[1;2;3];;
joinLists([],[1;2;3])=[1;2;3];;
joinLists(["Ala";"kota"],["ma"])=["Ala";"ma";"kota"];;


