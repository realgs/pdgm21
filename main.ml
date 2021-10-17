let rec summ i_list = 
  if i_list=[] then 0
  else List.hd i_list + summ(List.tl i_list);;


let rec concatenationStrings s_list end_symbol =
  if List.tl s_list=[] then List.hd s_list ^ end_symbol
  else List.hd s_list ^ " " ^ concatenationStrings (List.tl s_list) end_symbol;;


let rec isBiggerThanZero i_list =
  if i_list = [] then true
  else 
  if List.hd i_list > 0 then isBiggerThanZero (List.tl i_list)
  else false;;


let rec factorial number =
  if number < 0 then -1
  else if number = 0 then 1
  else factorial number*(number-1);;


(*Test cases for 1 task*)
summ [10; 15] = 25;;
summ [-100; 1000;345;46;23;43;-34] = 1323;;
summ []= 0;;


(*Test cases for 2 task*)
concatenationStrings ["Ola"; "Ma"; "Kota"] "!" = "Ola Ma Kota!";;
concatenationStrings ["Hello"; "World"] "?" = "Hello World?";;
concatenationStrings ["Example"; "Phrase"; "dodo"] "." = "Example Phrase dodo.";;


(*Test cases for 3 task*)
isBiggerThanZero [0; 2; 3; -1] = false;;
isBiggerThanZero [0; 0; 0; 0] = false;;
isBiggerThanZero [5; 5; 5; 6] = true;;


(*Test cases for 4 task*)
factorial (-10) = -1;;
factorial 0 = 1;;
factorial 3 = 6;;
factorial 1 = 1;;

