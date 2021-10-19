//Filip Brzeziak 260400

//Zadanie 1
def sumOfInts(list: List[Int]): Int=
  if list==Nil then 0
  else
      list.head + sumOfInts(list.tail)

sumOfInts(List(-1,5,-5))==(-1);
sumOfInts(List(0,2,3))==5;
sumOfInts(List())==0;

//Zadanie 2
def listOfStrings(list: List[String]): String=
    if list==Nil then ""
    else if list.head=="$" then "."
    else
        " " + list.head + listOfStrings(list.tail)

listOfStrings(List("Filip","ma","psa","$")) == " Filip ma psa.";
listOfStrings(List()) == "";
listOfStrings(List("Filip","ma","54$","$")) == " Filip ma 54$.";

//Zadanie 3
def greaterThanZero(list: List[Double]): Boolean =
    if list == Nil then false
    else if list.head <= 0.0 then false
    else
        if list.tail == Nil then true
        else greaterThanZero(list.tail)

greaterThanZero(List(-2,4,1))==false;
greaterThanZero(List(1,4,1))==true;
greaterThanZero(List())==false;


//Zadanie 4
def factorial(n: Double): Double=
    if n<0 then throw new Exception("Negative number!")
    else if n==0 then 1 else n*factorial(n-1)

factorial(4)==24;
factorial(0)==1;
//factorial(-1);
