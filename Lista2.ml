let sum list =
  let rec sumIter (list,sum) = 
    if list = [] then sum
    else sumIter(List.tl list, sum + List.hd list)
  in sumIter(list,0)
;;

sum [1;2;3];;
sum [];;
sum [-1;-2;2;1;0];;

let sentencer words =
  let rec sentencerIter (words, sentence) = 
    if words = [] then sentence ^ "."
    else if List.hd words = "." || List.hd words = "?" || List.hd words = "!"
    then sentence ^ List.hd words
    else sentencerIter(List.tl words,sentence ^ List.hd words ^ " ")
  in sentencerIter(words,"")
;;     

sentencer ["Ala";"ma";"kota";"."];;
sentencer ["Ala";"ma";"dosc"];;
sentencer ["Ile";"jeszcze";"?"];;
sentencer ["Juz";"tyle";"!"];;

let graterZeroChecker list = 
  let rec graterZeroCheckerHlp(list, noEmpty) =
    if list = [] then noEmpty
    else if List.hd list < 0 then false
    else graterZeroCheckerHlp(List.tl list, true)
  in graterZeroCheckerHlp(list, false)    
;;

graterZeroChecker [2;1;1;5;0];;
graterZeroChecker [-1;-2;0;3;4];;
graterZeroChecker [];;

let factorial x = 
  let rec factroialIter (x, res) = 
    if x = 0 then res
    else if x < 0 then -1
    else factroialIter(x-1,res*x)
  in factroialIter(x,1)
;;

factorial 6;;
factorial 0;;
factorial(-3);;
