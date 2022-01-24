import java.io._ // file, file exceptions, fileWriter, fileReader, ...
import scala.io.Source

import javax.naming.spi.DirStateFactory.Result
import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

import scala.annotation.tailrec

object L7 {

  def time[T](f: => T): T = {
    val start = System.nanoTime()
    val ret = f
    val end = System.nanoTime()
    println(s"Time taken: ${(end - start) / 1000 / 1000} ms")
    ret
  }

  //creating ping files ------------------------------------------------------------------------------------------------
  def savePingFromShellToFileFor256Adresses(firstPartOfIP:Int,secondPartOfIP:Int,thirdPartOfIP:Int,path:String):Unit =
    :sh cd path       //enter where path leads
    :sh md L7_PINGS   //create folder
    :sh del L7_PINGS  //clear folder
    :sh T             //agree - yes
    :sh cd L7_PINGS   //enter the folder
    @tailrec
    def recSavePingFromShellToFile(fourthPartOfIP:Int):Unit =
      :sh ping s"$firstPartOfIP.$secondPartOfIP.$thirdPartOfIP.$fourthPartOfIP > " //ping, save result ->
        + s"ping$firstPartOfIP-$secondPartOfIP-$thirdPartOfIP-$fourthPartOfIP.txt" //create a file and save there result
      if fourthPartOfIP<256 then recSavePingFromShellToFile(fourthPartOfIP+1)
      else ()
    recSavePingFromShellToFile(fourthPartOfIP = 0)

    // CONCURRENT VERSION (but we know shell can't check 2 pings at the same time so they must be done in order) . . . .
    def savePingFromShellToFileFor256AdressesConcurrently(firstPartOfIP:Int, secondPartOfIP:Int,
                                              thirdPartOfIP:Int, path:String):Unit=
    :sh cd path
    :sh md L7_PINGS
    :sh del L7_PINGS
    :sh T
    :sh cd L7_PINGS
    var fourthPartOfIP:Int = 0
    def nextPing (fourthPartOfIP:Int):Unit =
      :sh ping s"$firstPartOfIP.$secondPartOfIP.$thirdPartOfIP.$fourthPartOfIP > "
      + s"ping$firstPartOfIP-$secondPartOfIP-$thirdPartOfIP-$fourthPartOfIP.txt"
    @tailrec
    def recSavePingFromShellToFile():Unit =
      Future(nextPing(fourthPartOfIP))
      if fourthPartOfIP<256 then {
        fourthPartOfIP = fourthPartOfIP + 1
        recSavePingFromShellToFile() }
      else ()
    recSavePingFromShellToFile()

  //length methods ------------------------------------------------------------------------------------------------------
  def listLeng[A] (listA:List[A]):Int =
    @tailrec
    def recListLeng[A] (listA:List[A], result:Int):Int=
      if listA == Nil then result
      else recListLeng(listA.tail, result+1)
    recListLeng(listA,0)

  def stringLeng (StringA:String):Int ={
    @tailrec
    def recStringLeng (StringA:String, result:Int):Int={
      if StringA == "" then result
      else recStringLeng(StringA.tail, result+1)
    }
    recStringLeng(StringA,0)
  }

  // checking if the word has the pattern inside -----------------------------------------------------------------------
  def checkIfTheWordHasPattern(sourceWord:String, patternWord:String):Boolean=
    if patternWord == "" then false
    else
      @tailrec
      def recCheckWord (sourceWordTail:String, patternWordTail:String):Boolean =
        if stringLeng(sourceWordTail) < stringLeng(patternWordTail) then false
        else if patternWordTail == "" then true
        else if patternWordTail.head == sourceWordTail.head
          then recCheckWord(sourceWordTail.tail, patternWordTail.tail)
        else if patternWordTail.head != sourceWordTail.head
          then recCheckWord(sourceWordTail.tail, patternWord)
        else false
      recCheckWord(sourceWord, patternWord, false)

  //create list --------------------------------------------------------------------------------------------------------
  def createTheSameElementsList(elem:Int, length:Int):List[Int]=
    def recCreateList(index:Int, result:List[Int]):List[Int]=
      if index<length then recCreateList(index+1,result:::List(elem))
      else result
    recCreateList(0, Nil)

  def createNextIndexesList(length:Int):List[Int]=
    def recCreateList(index:Int, result:List[Int]):List[Int]=
      if index<length then recCreateList(index+1,index::result)
      else result
    recCreateList(0, Nil)


  //take n-th elem of a list -------------------------------------------------------------------------------------------
  def getNthElemOfList(elemIndex:Int, list:List[Int]):Int=
    if elemIndex>= listLeng(list) then throw IndexOutOfBoundsException()
    def recGetNthElemOfList(currentIndex:Int, listTail:List[Int]):Int=
      if currentIndex<elemIndex then recGetNthElemOfList(currentIndex+1,listTail.tail)
      else listTail.head
    recGetNthElemOfList(0, list)

  //set n-th elem of a list --------------------------------------------------------------------------------------------
  def setNthElemOfList(elemIndex:Int, elemValue:Int, list:List[Int]):List[Int]=
    if elemIndex>= listLeng(list) then throw IndexOutOfBoundsException()
    def recSetNthElemOfList(currIndex:Int,  listB:List[Int], listT:List[Int]):List[Int]=
      if currIndex<elemIndex then recSetNthElemOfList(currIndex+1, listB:::List(listT.head), listT.tail)
      else listB:::List(elemValue):::listT.tail
    recSetNthElemOfList(0, Nil, list)

  // reading ping files ------------------------------------------------------------------------------------------------
  def readPingFiles(firstPartOfIP:Int, secondPartOfIP:Int, thirdPartOfIP:Int, path:String):List[Int] =
    var resultList:List[Int] = createTheSameElementsList(-1,1)
    var indexesList[Int] = createNextIndexesList(1)

    def recEmptyIndexesList():Unit =
      if listLeng(indexesList)>0 then {
        val index:Int = indexesList.head
        try {
          val line:String = Source.fromFile(path+s"/ping$firstPartOfIP-$secondPartOfIP-" +
            s"$thirdPartOfIP-$index.txt").getLines().mkString()
          indexesList = indexesList.tail
          if checkIfTheWordHasPattern(line,"Odebrane = 0") then
            resultList = setNthElemOfList(index,0,resultList) //addresses in network
          else if checkIfTheWordHasPattern(line,"Odebrane = 1") || checkIfTheWordHasPattern(line,"Odebrane = 2")
            || checkIfTheWordHasPattern(line,"Odebrane = 3") || checkIfTheWordHasPattern(line,"Odebrane = 4") then
            resultList = setNthElemOfList(index,1,resultList) //addresses out of network
          else
            indexesList = indexesList ::: List(index)
        }
        catch {
          case e: FileNotFoundException () => indexesList = indexesList.tail ::: List(index)
        }
        recEmptyIndexesList()
      }
    recEmptyIndexesList()
    resultList

  //create array with indexes having the same value --------------------------------------------------------------------
  def getIndexesHavingTheValue (value:Int, list:List[Int]):List[Int]=
    @tailrec
    def recGetIndexesHavingTheValue(index:Int, listT:List[Int], result:List[Int]):List[Int]=
      if listT == Nil then result
      if listT.head == value then recGetIndexesHavingTheValue(index+1,listT.tail,result:::List(index))
      else recGetIndexesHavingTheValue(index+1,listT.tail,result)
    recGetIndexesHavingTheValue(0,list,Nil)

  //print list ---------------------------------------------------------------------------------------------------------
  def printList ( list:List[Int]):Unit=
    print("[")
    @tailrec
    def recPrintList(listT:List[Int]):Unit=
      if listT == Nil then ()
      else
        print(listT.head)
        if listLeng(listT)>2 then  print(", ")
        recPrintList(listT.tail)
    recPrintList(list)
    print("]")

  // create file where leads the path, with the name and add to it the text --------------------------------------------
  def createFileWithText (filePath:String, fileName:String, text:String):Unit =
    val writer = new PrintWriter(new File(filePath+"/"+fileName))
    writer.write(text)
    writer.close()

  // create files -----------------------------------------------------------------------------------------------------
  def createFilesWithTexts (filePath:String, defaultName:String, defaultText:String, filesAmount:Int):Unit =
    @tailrec
    def recCreateFiles(index:Int):Unit =
      if index<filesAmount then
        createFileWithText(filePath, defaultName+s"$index.txt", defaultText+s"$index")

  // read file ---------------------------------------------------------------------------------------------------------
  def readFile (filePath:String, fileName:String):Boolean=
    try {
      val line:String = Source.fromFile(filePath+"/" +fileName+"")
      println(line)
      true
    }
    catch {
      case e: FileNotFoundException () => indexesList = indexesList.tail ::: List(index)
      false
    }

  // read files --------------------------------------------------------------------------------------------------------
  def readFiles (path:String, defaultName:String,filesAmount:Int):Unit =
    var list = createNextIndexesList(filesAmount)
    def recReadFile():Unit =
      if list == Nil then ()
      else
        vae index = list.head
        if readFile(path, defaultName+s"$index.txt") then list = list.tail
        else list = list.tail:::List(list.head)
      if list != Nil then recReadFile()
    recReadFile()


  //--------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------
  //--------------------------------------------------------------------------------------------------------------------
  def main(args: Array[String]):Unit =

    // NORMAL VERSION . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    println("Pings normal version: ")
    time {
      savePingFromShellToFileFor256Adresses(127, 0, 0,
        "C:\\Users\\Monia\\Desktop\\STUDIA\\PARADYGMATY PROGRAMOWANIA\\PPro LABORATORIA\\PPro_lab_l7\\src")
      val resultList = readPingFiles(127, 0, 0,
        "C:\\Users\\Monia\\Desktop\\STUDIA\\PARADYGMATY PROGRAMOWANIA\\PPro LABORATORIA\\PPro_lab_l7\\src")
      val succesPingAdressesList: List[Int] = getIndexesHavingTheValue(1, resultList)
      val failPingAdressesList: List[Int] = getIndexesHavingTheValue(0, resultList)
      val notFoundPingAdressesList: List[Int] = getIndexesHavingTheValue(-1, resultList)
      print("SUCCESS: ")
      printList(succesPingAdressesList)
      print("FAILURE: ")
      printList(failPingAdressesList)
      print("NOT FOUND: ")
      printList(notFoundPingAdressesList)
    }
    // CONCURRENT VERSION . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    println("Pings concurrent version: ")
    time {
      Future(savePingFromShellToFileFor256Adresses(127, 0, 0,
        "C:\\Users\\Monia\\Desktop\\STUDIA\\PARADYGMATY PROGRAMOWANIA\\PPro LABORATORIA\\PPro_lab_l7\\src"))
      val resultList = Future(readPingFiles(127, 0, 0,
        "C:\\Users\\Monia\\Desktop\\STUDIA\\PARADYGMATY PROGRAMOWANIA\\PPro LABORATORIA\\PPro_lab_l7\\src"))
      val succesPingAdressesList: List[Int] = Future(getIndexesHavingTheValue(1, resultList))
      val failPingAdressesList: List[Int] = Future(getIndexesHavingTheValue(0, resultList))
      val notFoundPingAdressesList: List[Int] = Future(getIndexesHavingTheValue(-1, resultList))
      print("SUCCESS: ")
      printList(Await.result(succesPingAdressesList, Duration.Inf))
      print("FAILURE: ")
      printList(Await.result(failPingAdressesList, Duration.Inf))
      print("NOT FOUND: ")
      printList(Await.result(notFoundPingAdressesList, Duration.Inf))
    }

    // NORMAL VERSION . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    println("Files normal version: ")
    createFilesWithTexts("C:\\Users\\Monia\\Desktop\\STUDIA\\PARADYGMATY PROGRAMOWANIA\\PPro LABORATORIA\\PPro_lab_l7\\" +
      "src\\FilesTask2", "File", "Text ", 10)
    readFiles("C:\\Users\\Monia\\Desktop\\STUDIA\\PARADYGMATY PROGRAMOWANIA\\PPro LABORATORIA\\PPro_lab_l7\\" +
      "src\\FilesTask2", "Text ", 10)

    // CONCURRENT VERSION . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    println("Files concurrent version: ")

    Future(createFilesWithTexts("C:\\Users\\Monia\\Desktop\\STUDIA\\PARADYGMATY PROGRAMOWANIA\\PPro LABORATORIA\\PPro_lab_l7\\" +
      "src\\FilesTask2", "File", "Text ", 10))
    Future(readFiles("C:\\Users\\Monia\\Desktop\\STUDIA\\PARADYGMATY PROGRAMOWANIA\\PPro LABORATORIA\\PPro_lab_l7\\" +
      "src\\FilesTask2", "Text ", 10))


}