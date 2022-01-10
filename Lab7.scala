import jdk.jfr.Threshold
import scala.concurrent.*
import ExecutionContext.Implicits.global
import duration.*

object Lab7 {

  class Employee(n:Int, s:List[Int]){
    var id: Int = n
    var salaries: List[Int] = s
    var avarageSalary: Double = 0
  }

  def countAvarage(salaries: List[Int]):Double =
    def inner(salaries: List[Int],result: Int,count:Int):Double =
      //Thread.sleep(1)
      salaries match {
        case h::t => inner(t,result+h,count+1)
        case _ => result/count
      }
    inner(salaries,0,1)

  def addEmployees(list:List[Employee],number:Int):List[Employee] =
    if(number>0)
      addEmployees(new Employee( (Math.random()*1000).toInt , generateSalaries(1000))::list, number-1 )
    else
      list

  def generateSalaries(count:Int):List[Int] =
    if count>0 then (Math.random()*10000).toInt::generateSalaries(count-1)
    else Nil

  def separate(list:List[Employee]):(List[Employee],List[Employee]) =
    def separateInner(list:List[Employee], list1:List[Employee],list2:List[Employee]):(List[Employee],List[Employee]) =
      list match {
        case h1::h2::t => separateInner(t,h1::list1,h2::list2)
        case h1::t => separateInner(t,h1::list1,list2)
        case Nil => (list1,list2)
      }
    separateInner(list,List(),List())

////////////////////////////////////

  def countAvarageSalary(employees:List[List[Employee]]): List[List[Employee]] =
    for (list <- employees) {
      for (employee <- list)  employee.avarageSalary = countAvarage(employee.salaries)
    }
    employees

  def countAvarageSalaryFutures(employees:List[List[Employee]]): List[List[Employee]] = {
    val futures = for (list <- employees) yield Future{
      for (employee <- list) employee.avarageSalary = countAvarage(employee.salaries)
    }
    futures.map(Await.result(_, Duration.Inf))
    employees
  }

  ////////////////////////////////////

  def countAvarageSalary2(employees:List[Employee]): List[Employee] =
    for (employee <- employees)  employee.avarageSalary = countAvarage(employee.salaries)
    employees

  def countAvarageSalaryFutures2(employees:List[Employee]): List[Employee] = {
    val separatedList = separate(employees)
    val f1 = Future{for (employee <- separatedList._1)  employee.avarageSalary = countAvarage(employee.salaries)}
    val f2 = Future{for (employee <- separatedList._2)  employee.avarageSalary = countAvarage(employee.salaries)}
    Await.result(f1,Duration.Inf)
    Await.result(f2,Duration.Inf)
    employees
  }

  def countAvarageSalaryFutures4(employees:List[Employee]): List[Employee] = {
    val separatedList = separate(employees)
    val separatedList1 = separate(separatedList._1)
    val separatedList2 = separate(separatedList._2)
    val f1 = Future{for (employee <- separatedList1._1)  employee.avarageSalary = countAvarage(employee.salaries)}
    val f2 = Future{for (employee <- separatedList1._2)  employee.avarageSalary = countAvarage(employee.salaries)}
    val f3 = Future{for (employee <- separatedList2._1)  employee.avarageSalary = countAvarage(employee.salaries)}
    val f4 = Future{for (employee <- separatedList2._2)  employee.avarageSalary = countAvarage(employee.salaries)}
    Await.result(f1,Duration.Inf)
    Await.result(f2,Duration.Inf)
    Await.result(f3,Duration.Inf)
    Await.result(f4,Duration.Inf)
    employees
  }

  //////////////////////////////////////////

  def fibbFuture(number:Int):Int =
    number match {
      case 0 => 0
      case 1 => 1
      case _ =>
        val f1 = Future{fibb(number-1)}
        val f2 = Future{fibb(number-2)}
        Await.result(f1,Duration.Inf)+Await.result(f2,Duration.Inf)
    }

  def fibb(number:Int):Int =
    number match {
      case 0 => 0
      case 1 => 1
      case _ => fibb(number-1)+fibb(number-2)
    }



  def main(args: Array[String]) : Unit = {
    var ITspecialists: List[Employee] = List()
    var workers: List[Employee] = List()
    var salesmans: List[Employee] = List()

    ITspecialists = addEmployees(ITspecialists,1000)
    workers = addEmployees(workers,50000)
    salesmans = addEmployees(salesmans,10000)

    var listOfEmployees : List[List[Employee]] = List(ITspecialists,workers,salesmans)
    var connectedListOfEmployees : List[Employee] = ITspecialists:::workers:::salesmans

    //println(ITspecialists.head.avarageSalary)

    var time1 = System.nanoTime()
    countAvarageSalary(listOfEmployees)
    var time2 = System.nanoTime()
    println("List of lists:        "+(time2-time1))

    time1 = System.nanoTime()
    countAvarageSalary2(connectedListOfEmployees)
    time2 = System.nanoTime()
    println("List :                "+(time2-time1))

    //println(ITspecialists.head.avarageSalary)

    var time3 = System.nanoTime()
    countAvarageSalaryFutures(listOfEmployees)
    var time4 = System.nanoTime()
    println("List of lists future: "+(time4-time3))

    time3 = System.nanoTime()
    countAvarageSalaryFutures2(connectedListOfEmployees)
    time4 = System.nanoTime()
    println("List future2:         "+(time4-time3))

    time3 = System.nanoTime()
    countAvarageSalaryFutures4(connectedListOfEmployees)
    time4 = System.nanoTime()
    println("List future4:         "+(time4-time3))

    println(ITspecialists.head.avarageSalary)

    var time11 = System.nanoTime()
    fibb(45)
    var time22 = System.nanoTime()
    println("        "+(time22-time11))
    time11 = System.nanoTime()
    fibbFuture(45)
    time22 = System.nanoTime()
    println("Future: "+(time22-time11))

  println(s"Available processors = ${Runtime.getRuntime.availableProcessors()}")



  }


}
