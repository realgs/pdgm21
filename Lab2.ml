let rec sum xs=
  if(xs!=[]) then List.hd xs+sum(List.tl xs)
  else 0;;

sum([]);;
sum([2;4;5]);;
sum([-1;2;7;8;-2]);;

let rec stringsCon(xs,x)=
  if(xs!=[]) then
    if(List.hd xs=x) then List.hd xs
    else List.hd xs^" "^stringsCon(List.tl xs,x)
  else "pusta lista";;

stringsCon(["Ala";"ma";"kota";".";"aaa"],".");;
stringsCon([],",");;

let rec checkZero xs=
  if(xs!=[]) then
      if(List.hd xs>0) then checkZero(List.tl xs)
      else false
    else true;;

checkZero([2;3;5;6]);;
checkZero([0;2;-1]);;


let rec silnia x=
  if(x<=1) then 1
      else x*silnia(x-1);;

silnia(9);;
silnia(6);;
         
  
  
         
