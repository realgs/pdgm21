import scala.concurrent.*
import scala.concurrent.duration.Duration
import concurrent.ExecutionContext.Implicits.global

// Maciej Olejnik 260444
// first task

class User (p_email: String, p_password: String):
  def email: String = p_email
  def password: String = p_password

val databaseOfUsers = Array(User("example_email", "p"), User("another_mail", "m"), User("maily_maily", "i"));

val leakedDatabase = Array("example_email", "another_mail", "maily_maily")

def logIn(email: String, password: String): Boolean =
  var loggedIn = false;
  for (user <- databaseOfUsers)
    if user.email == email then
      if user.password == password then
        loggedIn = true
  loggedIn

val arrayOfSymbols = Array('0', '1', '2', '3', '4', '5', '6', '7', '8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','!','@','#','$','%','^','&','*','(',')','_','+','-','=','[',']','{','}','|','?','.','>',',','<',';',':','`','~','"')

def generate(email: String, arrayOfSymbols: Array[Char], index: Int, result: String, length: Int): String =
    var notFound = true;
    var innerIndex = 0
    var resultingString = ""

    if index == 0 && logIn(email, result) then
      result

    else if (index != 0)
      while innerIndex < length && notFound do
        val enlangeredResult = result + arrayOfSymbols(innerIndex)
        innerIndex += 1
        resultingString += generate(email, arrayOfSymbols, index - 1, enlangeredResult, length)
        if (resultingString != "") then notFound = false
      resultingString
    else
      resultingString

def hackAccount(email: String, minLength: Int, maxLength: Int): String =
  println("function started!")
  var index = minLength;
  var password = "";
  while index <= maxLength do
    password += generate(email, arrayOfSymbols, index, "", arrayOfSymbols.length)
    index += 1
  password


var startingFutureTime = System.nanoTime()

var futures = Array(Future {hackAccount(leakedDatabase(0), 2, 4)}, Future {hackAccount(leakedDatabase(1), 2, 4)}, Future {hackAccount(leakedDatabase(2), 2, 4)})
for (future <- futures)
  println(Await.result(future, Duration.Inf))

println(System.nanoTime() - startingFutureTime) // 4646547443

var startingSynchronousTime = System.nanoTime()

hackAccount(leakedDatabase(0), 2, 4)
hackAccount(leakedDatabase(1), 2, 4)
hackAccount(leakedDatabase(2), 2, 4)

println(System.nanoTime() - startingSynchronousTime) // 9825822092

// 9825822092 / 4646547443 = 2.11


// second task

def countFib(n: Int): Long =
  def fib(n: Int): Long =
    if n == 0 then 0
    else if n == 1 then 1
    else fib(n - 1) + fib(n - 2)
  fib(n)

var startingFutureTime = System.nanoTime()

var f1 = Future{countFib(41)}
var f2 = Future{countFib(43)}
var f3 = Future{countFib(45)}
var f4 = Future{countFib(47)}

Await.result(f4, Duration.Inf)
Await.result(f3, Duration.Inf)
Await.result(f2, Duration.Inf)
Await.result(f1, Duration.Inf)

println((System.nanoTime() - startingFutureTime)) // 28237309073

var startingSynchronousTime = System.nanoTime()

countFib(41)
countFib(43)
countFib(45)
countFib(47)

println((System.nanoTime() - startingSynchronousTime)) // 43501144586

// 43501144586 / 28237309073 = 1.54

startingFutureTime = System.nanoTime()

f1 = Future{countFib(3)}
f2 = Future{countFib(4)}
f3 = Future{countFib(5)}
f4 = Future{countFib(6)}

Await.result(f4, Duration.Inf)
Await.result(f3, Duration.Inf)
Await.result(f2, Duration.Inf)
Await.result(f1, Duration.Inf)

println((System.nanoTime() - startingFutureTime)) // 1381941

startingSynchronousTime = System.nanoTime()

countFib(3)
countFib(4)
countFib(5)
countFib(6)

println((System.nanoTime() - startingSynchronousTime)) // 9365

// 9365 / 1381941 = 0.006 !!
