(*zad1*)

let reverseList list =
  let rec reverseIter(listToReverse, reversed) =
    match listToReverse with
      [] -> reversed
    | hd :: tl -> reverseIter(tl, hd :: reversed)
  in reverseIter(list, []);;


let splitBySign list =
  let rec split(listToSplit, splitNeg, splitPosOdd) =
    match listToSplit with
      [] -> (splitNeg,splitPosOdd)
    | hd:: tl ->
       if hd < 0 then split(tl, hd :: splitNeg, splitPosOdd)
       else if  hd > 0 && hd mod 2 = 1 then split(tl, splitNeg, hd :: splitPosOdd)
       else split(tl, splitNeg, splitPosOdd)
  in split(reverseList list, [],[]);; 

splitBySign [-3;-6;7;-9;13] = ([-3;-6;-9],[7;13]);;
splitBySign [-3;-6;0;-2;20;17] = ([-3;-6;-2],[17]);;
splitBySign[-3;-6;0;-2;20;18] = ([-3;-6;-2],[]);;
splitBySign [] = ([],[]);;

(*zad2*)

let lengthOfList list =
  let rec lengthIter(acc, tail) =
    if tail = [] then acc
    else lengthIter(acc +1,List.tl tail)
  in lengthIter(0, list);;

lengthOfList [5;4;3;2] = 4;;
lengthOfList ['a';'b'] = 2;;
lengthOfList [] = 0;;

(*zad3*)
let rec joinLists list1 list2 =
  match (list1, list2) with
    ([],[]) -> []
  | ([],hd :: tl) -> hd :: joinLists [] tl
  | (hd :: tl,[]) -> hd :: joinLists tl []
  | (h1 :: t1, h2 :: t2) -> h1 ::h2 :: joinLists t1 t2;;

joinLists [5;4;3;2] [1;2;3;4;5;6] = [5;1;4;2;3;3;2;4;5;6];;
joinLists ['a';'a';'a';'o'] ['l';'m';'k';'t';'a'] = ['a';'l';'a';'m';'a';'k';'o';'t';'a'];;
joinLists [] ["Hell";"o";" ";"world"] = ["Hell";"o";" ";"world"];;
joinLists [] [] = [];;
                
