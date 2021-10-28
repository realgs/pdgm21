let rec reverse l = 
        let rec rev (ll, acc) =
                if ll = [] then acc
                else rev(List.tl ll, (List.hd ll) :: acc) in
        rev(l, []);;

(* Task 1 *)
let splitBySign l =
        let rec split (l, negative, positive) = match l with
         [] ->
                 (negative, positive)
         | (h::t) ->
                    if h < 0 then split((List.tl l), h :: negative, positive)
                    else if h > 0 && (h mod 2 != 0) then split((List.tl l), negative, h :: positive)
                    else split((List.tl l), negative, positive) in

        let r = split(l, [], []) in 
        (reverse(fst r), reverse(snd r));;

let l1 = [-3; -6; 7; -9; 13; 14];;
let l2 = [-3; -6];;
let l3 = [];;

print_string(string_of_bool(splitBySign(l1) = ([-3; -6; -9], [7; 13]))^"\n");;
print_string(string_of_bool(splitBySign(l2) = ([-3; -6], []))^"\n");;
print_string(string_of_bool(splitBySign(l3) = ([], []))^"\n");;



(* Task 2 *)
let rec lengthOfList l = match l with
        [] -> 0
         | _ -> 1 + lengthOfList(List.tl l);;

print_string(string_of_bool(lengthOfList(l1) = List.length l1)^"\n");;
print_string(string_of_bool(lengthOfList(l2) = List.length l2)^"\n");;
print_string(string_of_bool(lengthOfList(l3) = List.length l3)^"\n");;


(* Task 3 *)
let joinLists (l1, l2) =
        let rec join (first, second, res) = match (first, second) with
                (h::t, _) -> h :: join(second, t, res)
                 | ([], _) -> second in

        join(l1, l2, []);;

print_string(string_of_bool(joinLists(l1, l2) = [-3; -3; -6; -6; 7; -9; 13; 14])^"\n");;
print_string(string_of_bool(joinLists(l2, l3) = [-3; -6])^"\n");;
print_string(string_of_bool(joinLists(l3, l3) = [])^"\n");;
