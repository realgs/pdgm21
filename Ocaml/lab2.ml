(*Task 1*)
let rec sum list =
  if list = [] then 0
  else List.hd list + sum (List.tl list)
;;

(*Helper for Task 2*)

print_int (sum [1; 3; 6]);;
print_int (sum [-4; 89; 1; -56]);;
print_int (sum [])

  
