import MinBTree.*
import QuickSort.*
import TwoSequences.*
import ParallelComponents.*
import MergeSort.*

object Main
{
  def main(args: Array[String]) : Unit =
  {
    println("\n\nFinding min in binary tree, depth to split: 4")
    val tree1 = MinBTree.generateTree(1)
    val tree2 = MinBTree.generateTree(5)
    val tree3 = MinBTree.generateTree(10)
    val tree4 = MinBTree.generateTree(15)
    val tree5 = MinBTree.generateTree(20)

    println("\ntree, depth: 1")
    println("normal: " +ParallelComponents.measureTime(MinBTree.minTree(tree1)))
    println("future: " +ParallelComponents.measureTime(MinBTree.minTreeMixedFuture(tree1, 4)))
    println("parallel: " +ParallelComponents.measureTime(MinBTree.minTreeMixedParallel(tree1, 4)))

    println("\ntree, depth: 5")
    println("normal: " +ParallelComponents.measureTime(MinBTree.minTree(tree2)))
    println("future: " +ParallelComponents.measureTime(MinBTree.minTreeMixedFuture(tree2, 4)))
    println("parallel: " +ParallelComponents.measureTime(MinBTree.minTreeMixedParallel(tree2, 4)))

    println("\ntree, depth: 10")
    println("normal: " +ParallelComponents.measureTime(MinBTree.minTree(tree3)))
    println("future: " +ParallelComponents.measureTime(MinBTree.minTreeMixedFuture(tree3, 4)))
    println("parallel: " +ParallelComponents.measureTime(MinBTree.minTreeMixedParallel(tree3, 4)))

    println("\ntree, depth: 15")
    println("normal: " +ParallelComponents.measureTime(MinBTree.minTree(tree4)))
    println("future: " +ParallelComponents.measureTime(MinBTree.minTreeMixedFuture(tree4, 4)))
    println("parallel: " +ParallelComponents.measureTime(MinBTree.minTreeMixedParallel(tree4, 4)))

    println("\ntree, depth: 20")
    println("normal: " +ParallelComponents.measureTime(MinBTree.minTree(tree5)))
    println("future: " +ParallelComponents.measureTime(MinBTree.minTreeMixedFuture(tree5, 4)))
    println("parallel: " +ParallelComponents.measureTime(MinBTree.minTreeMixedParallel(tree5, 4)))

    println("\n\nFinding min in binary tree, depth to split: 2")

    println("\ntree, depth: 1")
    println("normal: " +ParallelComponents.measureTime(MinBTree.minTree(tree1)))
    println("future: " +ParallelComponents.measureTime(MinBTree.minTreeMixedFuture(tree1, 2)))
    println("parallel: " +ParallelComponents.measureTime(MinBTree.minTreeMixedParallel(tree1, 2)))

    println("\ntree, depth: 5")
    println("normal: " +ParallelComponents.measureTime(MinBTree.minTree(tree2)))
    println("future: " +ParallelComponents.measureTime(MinBTree.minTreeMixedFuture(tree2, 2)))
    println("parallel: " +ParallelComponents.measureTime(MinBTree.minTreeMixedParallel(tree2, 2)))

    println("\ntree, depth: 10")
    println("normal: " +ParallelComponents.measureTime(MinBTree.minTree(tree3)))
    println("future: " +ParallelComponents.measureTime(MinBTree.minTreeMixedFuture(tree3, 2)))
    println("parallel: " +ParallelComponents.measureTime(MinBTree.minTreeMixedParallel(tree3, 2)))

    println("\ntree, depth: 15")
    println("normal: " +ParallelComponents.measureTime(MinBTree.minTree(tree4)))
    println("future: " +ParallelComponents.measureTime(MinBTree.minTreeMixedFuture(tree4, 2)))
    println("parallel: " +ParallelComponents.measureTime(MinBTree.minTreeMixedParallel(tree4, 2)))

    println("\ntree, depth: 20")
    println("normal: " +ParallelComponents.measureTime(MinBTree.minTree(tree5)))
    println("future: " +ParallelComponents.measureTime(MinBTree.minTreeMixedFuture(tree5, 2)))
    println("parallel: " +ParallelComponents.measureTime(MinBTree.minTreeMixedParallel(tree5, 2)))


    println("\n\nFinding min in binary tree, depth to split: 1")

    println("\ntree, depth: 1")
    println("normal: " +ParallelComponents.measureTime(MinBTree.minTree(tree1)))
    println("future: " +ParallelComponents.measureTime(MinBTree.minTreeMixedFuture(tree1, 1)))
    println("parallel: " +ParallelComponents.measureTime(MinBTree.minTreeMixedParallel(tree1, 1)))

    println("\ntree, depth: 5")
    println("normal: " +ParallelComponents.measureTime(MinBTree.minTree(tree2)))
    println("future: " +ParallelComponents.measureTime(MinBTree.minTreeMixedFuture(tree2, 1)))
    println("parallel: " +ParallelComponents.measureTime(MinBTree.minTreeMixedParallel(tree2, 1)))

    println("\ntree, depth: 10")
    println("normal: " +ParallelComponents.measureTime(MinBTree.minTree(tree3)))
    println("future: " +ParallelComponents.measureTime(MinBTree.minTreeMixedFuture(tree3, 1)))
    println("parallel: " +ParallelComponents.measureTime(MinBTree.minTreeMixedParallel(tree3, 1)))

    println("\ntree, depth: 15")
    println("normal: " +ParallelComponents.measureTime(MinBTree.minTree(tree4)))
    println("future: " +ParallelComponents.measureTime(MinBTree.minTreeMixedFuture(tree4, 1)))
    println("parallel: " +ParallelComponents.measureTime(MinBTree.minTreeMixedParallel(tree4, 1)))

    println("\ntree, depth: 20")
    println("normal: " +ParallelComponents.measureTime(MinBTree.minTree(tree5)))
    println("future: " +ParallelComponents.measureTime(MinBTree.minTreeMixedFuture(tree5, 1)))
    println("parallel: " +ParallelComponents.measureTime(MinBTree.minTreeMixedParallel(tree5, 1)))

    //------------------------------------------------------------------------------------------------------
    println("\n\nMergesort, depth 4")
    val list1 = MergeSort.generateList(10, 1, 20)
    val list2 = MergeSort.generateList(100, 1, 200)
    val list3 = MergeSort.generateList(1000, 1, 2000)
    val list4 = MergeSort.generateList(10000, 1, 20000)
    val list5 = MergeSort.generateList(100000, 1, 200000)
    val list6 = MergeSort.generateList(1000000, 1, 2000000)
    val list7 = MergeSort.generateList(10000000, 1, 20000000)

    println("\nlist, elements: 10")
    println("normal: " +ParallelComponents.measureTime(MergeSort.mergesort(list1)))
    println("always future: " +ParallelComponents.measureTime(MergeSort.mergesortAlwaysFuture(list1)))
    println("mixed future: " +ParallelComponents.measureTime(MergeSort.mergesortMixedFuture(list1, 4)))
    println("mixed parallel: " +ParallelComponents.measureTime(MergeSort.mergesortMixedParallel(list1, 4)))

    println("\nlist, elements: 100")
    println("normal: " +ParallelComponents.measureTime(MergeSort.mergesort(list2)))
    println("always future: " +ParallelComponents.measureTime(MergeSort.mergesortAlwaysFuture(list2)))
    println("mixed future: " +ParallelComponents.measureTime(MergeSort.mergesortMixedFuture(list2, 4)))
    println("mixed parallel: " +ParallelComponents.measureTime(MergeSort.mergesortMixedParallel(list2, 4)))

    println("\nlist, elements: 1 000")
    println("normal: " +ParallelComponents.measureTime(MergeSort.mergesort(list3)))
    println("always future: " +ParallelComponents.measureTime(MergeSort.mergesortAlwaysFuture(list3)))
    println("mixed future: " +ParallelComponents.measureTime(MergeSort.mergesortMixedFuture(list3, 4)))
    println("mixed parallel: " +ParallelComponents.measureTime(MergeSort.mergesortMixedParallel(list3, 4)))

    println("\nlist, elements: 10 000")
    println("normal: " +ParallelComponents.measureTime(MergeSort.mergesort(list4)))
    println("always future: " +ParallelComponents.measureTime(MergeSort.mergesortAlwaysFuture(list4)))
    println("mixed future: " +ParallelComponents.measureTime(MergeSort.mergesortMixedFuture(list4, 4)))
    println("mixed parallel: " +ParallelComponents.measureTime(MergeSort.mergesortMixedParallel(list4, 4)))

    println("\nlist, elements: 100 000")
    println("normal: " +ParallelComponents.measureTime(MergeSort.mergesort(list5)))
    println("always future: " +ParallelComponents.measureTime(MergeSort.mergesortAlwaysFuture(list5)))
    println("mixed future: " +ParallelComponents.measureTime(MergeSort.mergesortMixedFuture(list5, 4)))
    println("mixed parallel: " +ParallelComponents.measureTime(MergeSort.mergesortMixedParallel(list5, 4)))

    println("\nlist, elements: 1 000 000")
    println("normal: " +ParallelComponents.measureTime(MergeSort.mergesort(list6)))
    println("always future: " +ParallelComponents.measureTime(MergeSort.mergesortAlwaysFuture(list6)))
    println("mixed future: " +ParallelComponents.measureTime(MergeSort.mergesortMixedFuture(list6, 4)))
    println("mixed parallel: " +ParallelComponents.measureTime(MergeSort.mergesortMixedParallel(list6, 4)))

    println("\nlist, elements: 10 000 000")
    println("normal: " +ParallelComponents.measureTime(MergeSort.mergesort(list7)))
    println("always future: " +ParallelComponents.measureTime(MergeSort.mergesortAlwaysFuture(list7)))
    println("mixed future: " +ParallelComponents.measureTime(MergeSort.mergesortMixedFuture(list7, 4)))
    println("mixed parallel: " +ParallelComponents.measureTime(MergeSort.mergesortMixedParallel(list7, 4)))


    println("\n\nMergesort, depth 2")
    println("\nlist, elements: 10")
    println("normal: " +ParallelComponents.measureTime(MergeSort.mergesort(list1)))
    println("always future: " +ParallelComponents.measureTime(MergeSort.mergesortAlwaysFuture(list1)))
    println("mixed future: " +ParallelComponents.measureTime(MergeSort.mergesortMixedFuture(list1, 2)))
    println("mixed parallel: " +ParallelComponents.measureTime(MergeSort.mergesortMixedParallel(list1, 2)))

    println("\nlist, elements: 100")
    println("normal: " +ParallelComponents.measureTime(MergeSort.mergesort(list2)))
    println("always future: " +ParallelComponents.measureTime(MergeSort.mergesortAlwaysFuture(list2)))
    println("mixed future: " +ParallelComponents.measureTime(MergeSort.mergesortMixedFuture(list2, 2)))
    println("mixed parallel: " +ParallelComponents.measureTime(MergeSort.mergesortMixedParallel(list2, 2)))

    println("\nlist, elements: 1 000")
    println("normal: " +ParallelComponents.measureTime(MergeSort.mergesort(list3)))
    println("always future: " +ParallelComponents.measureTime(MergeSort.mergesortAlwaysFuture(list3)))
    println("mixed future: " +ParallelComponents.measureTime(MergeSort.mergesortMixedFuture(list3, 2)))
    println("mixed parallel: " +ParallelComponents.measureTime(MergeSort.mergesortMixedParallel(list3, 2)))

    println("\nlist, elements: 10 000")
    println("normal: " +ParallelComponents.measureTime(MergeSort.mergesort(list4)))
    println("always future: " +ParallelComponents.measureTime(MergeSort.mergesortAlwaysFuture(list4)))
    println("mixed future: " +ParallelComponents.measureTime(MergeSort.mergesortMixedFuture(list4, 2)))
    println("mixed parallel: " +ParallelComponents.measureTime(MergeSort.mergesortMixedParallel(list4, 2)))

    println("\nlist, elements: 100 000")
    println("normal: " +ParallelComponents.measureTime(MergeSort.mergesort(list5)))
    println("always future: " +ParallelComponents.measureTime(MergeSort.mergesortAlwaysFuture(list5)))
    println("mixed future: " +ParallelComponents.measureTime(MergeSort.mergesortMixedFuture(list5, 2)))
    println("mixed parallel: " +ParallelComponents.measureTime(MergeSort.mergesortMixedParallel(list5, 2)))

    println("\nlist, elements: 1 000 000")
    println("normal: " +ParallelComponents.measureTime(MergeSort.mergesort(list6)))
    println("always future: " +ParallelComponents.measureTime(MergeSort.mergesortAlwaysFuture(list6)))
    println("mixed future: " +ParallelComponents.measureTime(MergeSort.mergesortMixedFuture(list6, 2)))
    println("mixed parallel: " +ParallelComponents.measureTime(MergeSort.mergesortMixedParallel(list6, 2)))

    println("\nlist, elements: 10 000 000")
    println("normal: " +ParallelComponents.measureTime(MergeSort.mergesort(list7)))
    println("always future: " +ParallelComponents.measureTime(MergeSort.mergesortAlwaysFuture(list7)))
    println("mixed future: " +ParallelComponents.measureTime(MergeSort.mergesortMixedFuture(list7, 2)))
    println("mixed parallel: " +ParallelComponents.measureTime(MergeSort.mergesortMixedParallel(list7, 2)))

    //------------------------------------------------------------------------------------------------------
    println("\n\nQuicksort")
    /*val array1 =  QuickSort.generateArray(10, 1, 20)
    val array2 =  QuickSort.generateArray(100, 1, 200)
    val array3 =  QuickSort.generateArray(1000, 1, 2000)
    val array4 =  QuickSort.generateArray(10000, 1, 20000)
    val array5 =  QuickSort.generateArray(100000, 1, 200000)
    val array6 =  QuickSort.generateArray(1000000, 1, 2000000)
    val array7 =  QuickSort.generateArray(10000000, 1, 20000000)*/

    val array1 = MergeSort.generateList(10, 1, 20)
    val array2 = MergeSort.generateList(100, 1, 200)
    val array3 = MergeSort.generateList(1000, 1, 2000)
    val array4 = MergeSort.generateList(10000, 1, 20000)
    val array5 = MergeSort.generateList(100000, 1, 200000)
    val array6 = MergeSort.generateList(1000000, 1, 2000000)
    val array7 = MergeSort.generateList(10000000, 1, 20000000)

    println("\narray, elements: 10")
    println("normal: " +ParallelComponents.measureTime(QuickSort.quicksort(array1.toArray)))
    println("always future: " +ParallelComponents.measureTime(QuickSort.quicksortAlwaysFuture(array1.toArray)))
    println("mixed future: " +ParallelComponents.measureTime(QuickSort.quicksortMixedFuture(array1.toArray)))
    println("mixed parallel: " +ParallelComponents.measureTime(QuickSort.quickMixedParallel(array1.toArray)))

    println("\narray, elements: 100")
    println("normal: " +ParallelComponents.measureTime(QuickSort.quicksort(array2.toArray)))
    println("always future: " +ParallelComponents.measureTime(QuickSort.quicksortAlwaysFuture(array2.toArray)))
    println("mixed future: " +ParallelComponents.measureTime(QuickSort.quicksortMixedFuture(array2.toArray)))
    println("mixed parallel: " +ParallelComponents.measureTime(QuickSort.quickMixedParallel(array2.toArray)))

    println("\narray, elements: 1 000")
    println("normal: " +ParallelComponents.measureTime(QuickSort.quicksort(array3.toArray)))
    //println("always future: " +ParallelComponents.measureTime(QuickSort.quicksortAlwaysFuture(array3)))
    println("mixed future: " +ParallelComponents.measureTime(QuickSort.quicksortMixedFuture(array3.toArray)))
    println("mixed parallel: " +ParallelComponents.measureTime(QuickSort.quickMixedParallel(array3.toArray)))

    println("\narray, elements: 10 000")
    println("normal: " +ParallelComponents.measureTime(QuickSort.quicksort(array4.toArray)))
    //println("always future: " +ParallelComponents.measureTime(QuickSort.quicksortAlwaysFuture(array4)))
    println("mixed future: " +ParallelComponents.measureTime(QuickSort.quicksortMixedFuture(array4.toArray)))
    println("mixed parallel: " +ParallelComponents.measureTime(QuickSort.quickMixedParallel(array4.toArray)))

    println("\narray, elements: 100 000")
    println("normal: " +ParallelComponents.measureTime(QuickSort.quicksort(array5.toArray)))
    //println("always future: " +ParallelComponents.measureTime(QuickSort.quicksortAlwaysFuture(array5)))
    println("mixed future: " +ParallelComponents.measureTime(QuickSort.quicksortMixedFuture(array5.toArray)))
    println("mixed parallel: " +ParallelComponents.measureTime(QuickSort.quickMixedParallel(array5.toArray)))

    println("\narray, elements: 1 000 000")
    println("normal: " +ParallelComponents.measureTime(QuickSort.quicksort(array6.toArray)))
    //println("always future: " +ParallelComponents.measureTime(QuickSort.quicksortAlwaysFuture(array6)))
    println("mixed future: " +ParallelComponents.measureTime(QuickSort.quicksortMixedFuture(array6.toArray)))
    println("mixed parallel: " +ParallelComponents.measureTime(QuickSort.quickMixedParallel(array6.toArray)))

    println("\narray, elements: 10 000 000")
    println("normal: " +ParallelComponents.measureTime(QuickSort.quicksort(array7.toArray)))
    //println("always future: " +ParallelComponents.measureTime(QuickSort.quicksortAlwaysFuture(array7)))
    println("mixed future: " +ParallelComponents.measureTime(QuickSort.quicksortMixedFuture(array7.toArray)))
    println("mixed parallel: " +ParallelComponents.measureTime(QuickSort.quickMixedParallel(array7.toArray)))

    //---------------------------------------------------------------------------------------------------------------------------

    println("\n\nSum of two sequences")
    println("\nsequence, index: 10")
    println("normal: " +ParallelComponents.measureTime(TwoSequences.sumTwoSeq(1, x =>  x + 2, 1, x => 2 * x, 10)))
    println("future: " +ParallelComponents.measureTime(TwoSequences.sumTwoSeqFuture(1, x =>  x + 2, 1, x => 2 * x, 10)))
    println("parallel through name: " +ParallelComponents.measureTime(TwoSequences.sumTwoSeqParallel(1, x =>  x + 2, 1, x => 2 * x, 10)))

    println("\nsequence, index: 100")
    println("normal: " +ParallelComponents.measureTime(TwoSequences.sumTwoSeq(1, x =>  x + 2, 1, x => 2 * x, 100)))
    println("future: " +ParallelComponents.measureTime(TwoSequences.sumTwoSeqFuture(1, x =>  x + 2, 1, x => 2 * x, 100)))
    println("parallel through name: " +ParallelComponents.measureTime(TwoSequences.sumTwoSeqParallel(1, x =>  x + 2, 1, x => 2 * x, 100)))

    println("\nsequence, index: 1 000")
    println("normal: " +ParallelComponents.measureTime(TwoSequences.sumTwoSeq(1, x =>  x + 2, 1, x => 2 * x, 1000)))
    println("future: " +ParallelComponents.measureTime(TwoSequences.sumTwoSeqFuture(1, x =>  x + 2, 1, x => 2 * x, 1000)))
    println("parallel through name: " +ParallelComponents.measureTime(TwoSequences.sumTwoSeqParallel(1, x =>  x + 2, 1, x => 2 * x, 1000)))

    println("\nsequence, index: 10 000")
    println("normal: " +ParallelComponents.measureTime(TwoSequences.sumTwoSeq(1, x =>  x + 2, 1, x => 2 * x, 10000)))
    println("future: " +ParallelComponents.measureTime(TwoSequences.sumTwoSeqFuture(1, x =>  x + 2, 1, x => 2 * x, 10000)))
    println("parallel through name: " +ParallelComponents.measureTime(TwoSequences.sumTwoSeqParallel(1, x =>  x + 2, 1, x => 2 * x, 10000)))

    println("\nsequence, index: 100 000")
    println("normal: " +ParallelComponents.measureTime(TwoSequences.sumTwoSeq(1, x =>  x + 2, 1, x => 2 * x, 100000)))
    println("future: " +ParallelComponents.measureTime(TwoSequences.sumTwoSeqFuture(1, x =>  x + 2, 1, x => 2 * x, 100000)))
    println("parallel through name: " +ParallelComponents.measureTime(TwoSequences.sumTwoSeqParallel(1, x =>  x + 2, 1, x => 2 * x, 100000)))

    println("\nsequence, index: 1 000 000")
    println("normal: " +ParallelComponents.measureTime(TwoSequences.sumTwoSeq(1, x =>  x + 2, 1, x => 2 * x, 1000000)))
    println("future: " +ParallelComponents.measureTime(TwoSequences.sumTwoSeqFuture(1, x =>  x + 2, 1, x => 2 * x, 1000000)))
    println("parallel through name: " +ParallelComponents.measureTime(TwoSequences.sumTwoSeqParallel(1, x =>  x + 2, 1, x => 2 * x, 1000000)))

  }
}
