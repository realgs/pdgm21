let splitBySign entryList=
  let rec splitBySignIter (entryList, ac)=
    if entryList = [] then ac
    else if List.hd entryList<0 then splitBySignIter(List.tl entryList, (fst ac @ [List.hd entryList], snd ac))
    else if List.hd entryList>0 && List.hd entryList mod 2 != 0 then splitBySignIter(List.tl entryList, (fst ac, snd ac @ [List.hd entryList]))
    else splitBySignIter(List.tl entryList, ac)
  in splitBySignIter(entryList, ([],[]));;

splitBySign([-3;-6;7;-9;13]);;
splitBySign([]);;
splitBySign([-3;-9;-18;0;2;4;6;8;3;-5;5;7;9;11]);;

let lengthOfList entryList=
  let rec lengthOfListIter (entryList, ac)=
    if(entryList = []) then ac
    else lengthOfListIter(List.tl entryList, ac+1)
  in lengthOfListIter(entryList, 0);;

lengthOfList([-3;-6;7;-9;13]);;
lengthOfList([]);;
lengthOfList(["Ala";"ma";"kota"]);;

let joinLists(firstList, secondList)=
  let rec joinListsIter firstList secondList ac=
    if(firstList = [] && secondList = []) then ac
    else if (firstList = []) then ac @ secondList
    else if (secondList = []) then ac @ firstList
    else joinListsIter (List.tl firstList) (List.tl secondList) (ac @ ((List.hd firstList) :: [List.hd secondList]))
  in joinListsIter firstList secondList [];;

joinLists([-3;-6;7;-9;13], [55;80;15;20;30;15;25;45]);;
joinLists([], ["Ala";"ma";"kota"]);;
joinLists(["Ala";"ma";"kota"], []);;
joinLists([], []);;

