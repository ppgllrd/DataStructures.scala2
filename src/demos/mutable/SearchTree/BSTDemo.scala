package demos.mutable.SearchTree

object BSTDemo extends App {
  import dataStructures.mutable.SearchTree.BST._

  val bst1 = BST[Int]()
  bst1.insert(10)
  bst1.insert(15)
  bst1.insert(5)
  bst1.insert(3)
  bst1.insert(18)
  bst1.insert(20)

  for(x <- bst1.inOrder())
    println(x)

  println

  val bst2 = BST(5,8,2,9,1,13,22,6)
  bst2.inOrder().foreach(println(_))
}
