package model.decisionTreeModel

sealed trait DecisionTree[+A]
case object Empty extends DecisionTree[Nothing]
case class Node[+A](elem:A, children: List[() => DecisionTree[A]]) extends DecisionTree[A]
