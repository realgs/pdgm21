import scala.annotation.tailrec
import scala.concurrent.Future
import scala.io.Source
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object WordCounter {

  def countTime[A](task : => A) : Long = {
    val time = System.nanoTime()
    task
    System.nanoTime() - time
  }

  def countWord(lines : List[String], word : String) : Int = {
    @tailrec
    def countWordHelper(lines : List[String], counter : Int) : Int = {
      if lines == Nil then counter
      else countWordHelper(lines.tail, counter + processLine(lines.head.trim().split("\\s++").toList, word))
    }
    countWordHelper(lines, 0)
  }

  def countWordParallel(lines : List[String], word : String) : Int = {
    val linesCounter = for( line <- lines ) yield Future {
      processLine(line.trim().split("\\s++").toList, word)
    }
    linesCounter.map(Await.result(_, Duration.Inf)).sum
  }

  def processLine(words : List[String], word : String) : Int = {
    @tailrec
    def processLineHelper(words : List[String], counter : Int) : Int = {
      if words == Nil then counter
      else {
        if words.head == word then processLineHelper(words.tail, counter + 1)
        else processLineHelper(words.tail, counter)
      }
    }
    processLineHelper(words, 0)
  }


  def main(args: Array[String]) : Unit = {
    val source1 = Source.fromFile("sample1.txt")
    val lines1 : List[String] = source1.getLines().toList
    source1.close()
    println("SAMPLE 1 ---------------")
    println("File is medium size (581 kB)")
    print("SEQUENTIAL:  ")
    println(countTime(countWord(lines1, "this")))
    print("PARALLEL:    ")
    println(countTime(countWordParallel(lines1, "this")))
    println("")

    val source2 = Source.fromFile("sample2.txt")
    val lines2 : List[String] = source2.getLines().toList
    source2.close()
    println("SAMPLE 2 ---------------")
    println("File is large (26 MB)")
    print("SEQUENTIAL:  ")
    println(countTime(countWord(lines2, "this")))
    print("PARALLEL:    ")
    println(countTime(countWordParallel(lines2, "this")))
    println("")

    val source3 = Source.fromFile("sample3.txt")
    val lines3 : List[String] = source3.getLines().toList
    source3.close()
    println("SAMPLE 3 ---------------")
    println("File is small (1,6 kB)")
    print("SEQUENTIAL:  ")
    println(countTime(countWord(lines3, "this")))
    print("PARALLEL:    ")
    println(countTime(countWordParallel(lines3, "this")))
    println("------------------------")
  }

}
