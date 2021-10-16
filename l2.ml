(*l2 zadanie 1.*)
print_string "Zad 1";;
let list1 = 3::4::1::[];;
let list2 = [];;
let list3 = -3::-4::1::[];;
let sumList xs =
	let rec recSumList (xs, listSum) =
		if xs =[] then listSum
		else recSumList (List.tl xs, listSum + List.hd xs)
		in recSumList(xs, 0);;
print_string "List 1 sum:\n";;
sumList list1;;
print_string "List 2 sum:\n";;
sumList list2;;
print_string "List 3 sum:\n";;
sumList list3;;
(*l2 zadanie 2.*)
print_string "Zad 2";;
let words = "How"::"are"::"you"::"today"::[];;
let words1 = "Have"::"a"::"nice"::"day"::[];;
let words2 = "Oh"::"my"::[];;
let words3 = [];;
let space = " ";;
let createSentence (wordList, endMark) =
    let rec recCreateSentence (wordList, endMark, sentence) =
        if List.length wordList >0
        then recCreateSentence (List.tl wordList, endMark, sentence^space^List.hd wordList)
        else sentence^endMark
    in recCreateSentence (wordList, endMark, "");;
createSentence(words, "?");;
createSentence(words1, "!");;
createSentence(words2, "...");;
createSentence(words3, "");;
(*l2 zadanie 3. - zakladam ze pusta lista tez ma zwracac true*)
print_string "Zad 3";;
let numbers1 = 3::4::8::[];;
let numbers2 = -3::5::1::[];;
let numbers3 = [];;
let numbers4 = 20::4::0::[];;
let rec isPositive numbers =
    if List.length numbers<1 then true
    else if List.hd numbers>0 then isPositive (List.tl numbers)
    else false;;
isPositive numbers1;;
isPositive numbers2;;
isPositive numbers3;;
isPositive numbers4;;
(*l2 zadanie 4.*)
print_string "Zad 4";;
let x1 = 3;;
let x2 = 0;;
let x3 = -2;;
let rec strongF x =
    if x < 0 then failwith "The number is smaller than 0"
    else if x = 0 then x + 1
    else x * strongF (x - 1);;
print_string "Strong of x1:\n";;
strongF x1;;
print_string "Strong of x2:\n";;
strongF x2;;
print_string "Strong of x3:\n";;
strongF x3;;
