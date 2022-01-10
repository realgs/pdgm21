import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter}
import java.util.concurrent.ForkJoinTask
import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object l7 {

  //przykład 1

  def calculateAverages(listOfStudents: List[List[Int]]): List[Float] = {
    @tailrec
    def averageForStudent(listOfRemainingStudents: List[List[Int]], listOfAverages: List[Float]): List[Float] = {
      listOfRemainingStudents match
        case List() => listOfAverages
        case grades :: tailListOfRemainingStudents => averageForStudent(tailListOfRemainingStudents, calAverage(grades, 0, 0) :: listOfAverages)
    }

    @tailrec
    def calAverage(listOfGrades: List[Int], sum: Int, x: Int): Float = {
      listOfGrades match
        case List() => sum.toFloat / x
        case grade :: tailListOfGrades => calAverage(tailListOfGrades, sum + grade, x + 1)
    }

    averageForStudent(listOfStudents, List()).reverse
  }


  def calculateAveragesFutures(listOfStudents: List[List[Int]], parts: Int): List[Float] = {
    if parts > 0 then
      val partyList = splitList(listOfStudents)
      calculateAveragesFutures(partyList._1, parts - 1) ::: calculateAveragesFutures(partyList._2, parts - 1)
    else
      val f = Future {
        calculateAverages(listOfStudents)
      }
      Await.result(f, Duration.Inf)
  }


  def createStudents(students: Int, grades: Int): List[List[Int]] = {
    @tailrec
    def addGrades(marks: List[Int], n: Int): List[Int] = {
      n match
        case 0 => marks
        case _ => addGrades((Math.random() * 6 + 1).toInt :: marks, n - 1)
    }

    @tailrec
    def addStudents(stud: List[List[Int]], n: Int): List[List[Int]] = {
      n match
        case 0 => stud
        case _ => addStudents(addGrades(List(), grades) :: stud, n - 1)
    }

    addStudents(List(), students)
  }


  //przykład 2

  def changeNumberSystemInFile(listOfFiles: List[File], system:Int): List[List[String]] = {
    var hexFilesList = List[List[String]]()
    if listOfFiles != List() then
      for (file <- listOfFiles)
        try {
          var hexList = List[String]()
          val reader = new BufferedReader(new FileReader(file))
          var row = reader.readLine()
          while (row != null)
            val hex = changeNumberSystem(row.toInt, system)
            row = reader.readLine()
            hexList = fromListToString(hex) :: hexList
          hexFilesList = hexList :: hexFilesList
        }
    hexFilesList
  }

  def changeNumberSystemInFileFutures(listOfFiles: List[File], system:Int, parts: Int): List[List[String]] = {
    if parts > 0 then
      val partyList = splitList(listOfFiles)
      changeNumberSystemInFileFutures(partyList._1, parts - 1, system) ::: changeNumberSystemInFileFutures(partyList._2, parts - 1, system)
    else
      val f = Future {
        changeNumberSystemInFile(listOfFiles, system)
      }
      Await.result(f, Duration.Inf)
  }

  def changeNumberSystem(number: Int, system: Int): List[Int] =
    @tailrec
    def changeTail(remaining: Int, list: List[Int]): List[Int] =
      if remaining / system < 1 then
        remaining :: list
      else
        changeTail(remaining / system, (remaining % system) :: list)
    changeTail(number, Nil)

  def fromListToString(list: List[Int]):String={
    @tailrec
    def fromListToStringInner(listIn: List[Int], stringOut:String):String =
      listIn match
        case List() => stringOut
        case first::tail => fromListToStringInner(tail, stringOut+first.toString)
    fromListToStringInner(list, "")
  }


  def getListOfFiles(dir: String):List[File] =
    val d = new File(dir)
    if (d.exists && d.isDirectory)
      d.listFiles.filter(_.isFile).toList
    else
      List[File]()

  def saveToFiles(data:List[List[String]], fileName:String):Unit={
    var counter=1
    var tempList = List[String]()
    if data != List() then
      for (d <- data)
        try {
          tempList = d
          var number = tempList.head
          val writer = new BufferedWriter(new FileWriter(fileName + counter + ".txt"))
          while (number != null)
            writer.write(number + "\n")
            if tempList.tail != List() then
              tempList = tempList.tail
              number = tempList.head
            else
              number = null
          writer.flush()
          writer.close()
          counter += 1
        }
  }


  def splitList[A](list: List[A]): (List[A], List[A])={
    @tailrec
    def innerSplit[A](firstList: List[A], secondList:List[A], len: Int): (List[A], List[A])={
      (len, firstList) match
        case (0, _) => (firstList, secondList.reverse)
        case (_, head::tail) =>innerSplit(tail, head::secondList, len-1)
        case (_, List()) => throw Exception("Unexpected error")
    }
    innerSplit(list, List(), list.length/2)
  }


  def time[T](fun: => T): Unit = {
    val start = System.nanoTime()
    val ret = fun
    val end = System.nanoTime()
    println(s"Time taken: ${(end - start) / 1000 } * 10^-6 s")
  }

  /*
  def parallel[A,B](taskA: => A, taskB: => B):(A,B)={
    val right = task {
      taskB
    }
    val left = taskA
    (left, right.join())
  }


  def task[T](body: => T):ForkJoinTask[T] ={
    //scheduler.value.schedule(body)
  }
  */


  def main(args: Array[String]): Unit = {

    println(s"Avaliable processors = ${Runtime.getRuntime.availableProcessors()}")

    println("Przykład 1 - liczenie średnich dla danych uczniów")

    val students = createStudents(10000, 1000)

    //println(students)
    time{
      //println(calculateAverages(students))
      calculateAverages(students)
    }
    time{
      //println(calculateAveragesFutures(students, 1))
      calculateAveragesFutures(students, 1)
    }
    time{
      //println(calculateAveragesFutures(students, 2))
      calculateAveragesFutures(students, 2)
    }
    time{
      //println(calculateAveragesFutures(students, 3))
      calculateAveragesFutures(students, 3)
    }
    time{
      //println(calculateAveragesFutures(students, 4))
      calculateAveragesFutures(students, 4)
    }
    time{
      //println(calculateAveragesFutures(students, 4))
      calculateAveragesFutures(students, 5)
    }

    println("Przykład 2 - konwersja liczb z systemu dziesiętnego na wybrany")
    val folder = new String("/Users/paulina/Desktop/numbers")

    time{
      changeNumberSystemInFile(getListOfFiles(folder).tail, 8)
    }
    time{
      changeNumberSystemInFileFutures(getListOfFiles(folder).tail, 8, 1)
    }

    time{
      changeNumberSystemInFileFutures(getListOfFiles(folder).tail, 8, 2)
    }

    time{
      changeNumberSystemInFileFutures(getListOfFiles(folder).tail, 8, 3)
    }

    time{
      changeNumberSystemInFileFutures(getListOfFiles(folder).tail, 8, 4)
    }

    time{
      changeNumberSystemInFileFutures(getListOfFiles(folder).tail, 8, 5)
    }

    time{
      saveToFiles(changeNumberSystemInFile(getListOfFiles(folder).tail, 8), "/Users/paulina/Desktop/results/res")
    }

  }

}
