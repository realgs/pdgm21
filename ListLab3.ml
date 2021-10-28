(*Mateusz Pietrych*)

let rec splitList xs =
   match xs with
     ([]) -> ([],[])
     |  (h::t) -> let x = (splitList(t))
     in if h < 0 then (h :: fst x, snd x)
        else if h > 0 && h mod 2 = 1 then (fst x, h :: snd x)
        else (fst x, snd x)
     ;;

splitList[-3;-6;7;-9;13];;
splitList[];;
splitList[0;1;2;3;-1;4;6];;
splitList[0;2;4;6];;
splitList[-3;-1;-5];;

let length xs =
  let rec lengthRec (xs, k) =
    match xs with
    ([]) -> k
    |  (h::t) -> lengthRec(t, k+1)
  in lengthRec(xs, 0)
;;

length [6;7;4;45;6];;
length ["A";"B";"C";"D";"E"];;
length ["To ja"];;

let rec combine (xs,ys) =
  match (xs, ys) with
  (h::t, h2::t2) -> h :: combine(ys, t)
  | (h::t, _) -> xs
  | (_, h::t) -> ys
  | (_,_) -> []
;;

combine([5;4;3;2], [1;2;3;4;5;6]);;
combine([], []);;
combine([], [1;1;1;1;1]);;
combine([1;1;1;1;1], []);;

