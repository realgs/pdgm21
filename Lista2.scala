object Lista2 {

  def len[A](xs:List[A]):Int=(
    if xs.isEmpty then 0
    else 1+len(xs.tail)
  )
  
  //zadanie 1
  def summarize(xs:List[Int]):Int={
  if xs.isEmpty then 0
  else xs.head+summarize(xs.tail)
  }
  //zadanie 2
  def sentence(xs: List[String], x:String):String={
    if xs.isEmpty then x
    else if len(xs)==1 then xs.head+x
    else xs.head+" "+sentence(xs.tail, x)
  }
  //zadanie 3
  def greaterThanZero(xs:List[Int]):Boolean={
    if xs.isEmpty then true
    else if xs.head>0 then greaterThanZero(xs.tail)
    else false
  }
  //zadanie 4
  def factorial(x:Int):Int={
    if x<0 then throw new Exception("Number has to be greater or equal zero.")
    else if x==0 then 0
    else if x==1 then 1
    else x*factorial(x-1)
  }
}
