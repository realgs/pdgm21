def sum[A](x:List[Int]):Int={
  if(x!=Nil) then x.head+sum(x.tail)
  else 0
}

sum(List())
sum(List(2,4,5))
sum(List(-1,2,7,8,-2))

def stringsCon[A](x:List[String],znak:String):String={
  if(x!=Nil) then
    if(x.head==znak) then x.head
    else x.head+" "+stringsCon(x.tail,znak)
  else "Lista jest pusta"
}

stringsCon(List("Ala","ma","kota",".","aaa"),".")
stringsCon(List(),",")


def checkZero[A](x:List[Int]):Boolean={
  if(x!=Nil) then
    if(x.head>0) then checkZero(x.tail)
    else false
  else true
}

checkZero(List(2,3,5,6))
checkZero(List(0,2,-1))

def silnia[A](x:Int):Int={
  if(x<=1) then 1
  else x*silnia(x-1)
}

silnia(9)
silnia(6)

