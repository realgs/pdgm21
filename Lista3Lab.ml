let rec length(list) = (
    if list = [] then 0 else 1 + length(List.tl list)
  );;

(*

My first idea with worse complexity

let splitBySign(list) = 
    let rec splitBySignNeg(list) = 
      if list = [] then []
      else if List.hd list < 0 then List.hd list::splitBySignNeg(List.tl list)
	    else splitBySignNeg(List.tl list)
      in
    let rec splitBySignPos(list) = 
      if list = [] then []
      else if List.hd list >= 0 then List.hd list::splitBySignPos(List.tl list)
      else splitBySignPos(List.tl list)
    in
    (splitBySignNeg(list), splitBySignPos(list));;

*)

let reverse(list) = 
  let rec reverseRec(list, newList ) = 
    match list with 
    [] -> newList
    |h::t -> reverseRec(t, h::newList)
    in
  reverseRec(list, []);;

let splitBySign(l) = 
    let rec splitBySignRec(l, accNeg, accPos) = 
      match l with
        | [] -> (reverse(accNeg),reverse(accPos))
        | h::t -> if (h < 0) then splitBySignRec(t, h :: accNeg, accPos)
                  else splitBySignRec(t, accNeg, h :: accPos) in
    splitBySignRec(l, [], []);;
  

  


splitBySign([1;2;3;-1;-2;3;-2]);;       
splitBySign([]);;
splitBySign([1;2;3;4;5]);;
splitBySign([-1;-2;-3;-6]);;             

(length([])=0);;
  (length([1;2;3;4;5])=5);;
  (length([1])=1);;
  (length([-1;-2;-3;-4])=4);;

  let rec addLists(list1, list2) =          
    match (list1, list2) with
      |([],[]) -> []
      |(h::t,h2::t2) -> h::h2::addLists(t,t2)
      |([],h::t) -> h::addLists(t, [])  
      |(h::t,[]) -> h::addLists(t, []);;
        
addLists([],[]);;
addLists([], [1;2;3;4]);;
addLists([1;2;3;4], []);;
addLists([1;2;3], [6;5;4]);;

             


