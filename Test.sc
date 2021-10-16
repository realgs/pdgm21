import Lista2.{factorial, greaterThanZero, len, sentence, summarize}
summarize(List(1,2,3,4))
summarize(List(-5,7,0,3,34))
summarize(List())

sentence(List("Some","sentence"),"?")
sentence(List("Another", "sentence","a","little","bit","longer"),".")
sentence(List(),";")

greaterThanZero(List(1,2,3,4))
greaterThanZero(List())
greaterThanZero(List(1,-12,5))

factorial(-3)
factorial(5)
factorial(8)
