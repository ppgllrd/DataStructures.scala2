/******************************************************************************
  * Weight Biased Leftist Heaps in Scala
  *
  * Data Structures in Scala
  * Pepe Gallardo, 2015.
  ******************************************************************************/

package dataStructures.immutable.Heap

sealed trait WBLHeap[+T] {
  def isEmpty : Boolean
  def minElem : T
  def delMin : WBLHeap[T]
  def insert[E >: T](x : E)(implicit ord : Ordering[E]) : WBLHeap[E]
  def weight : Int
}

object WBLHeap {
  // O(1)
  def apply[T]() : WBLHeap[T] =
    Empty

  // O(n)
  def apply[T](xs : T*)(implicit ord : Ordering[T]) : WBLHeap[T] = {
    def pairwise(xs : List[WBLHeap[T]]) : List[WBLHeap[T]] = xs match {
      case (h1::h2::hs) =>
        Node.merge(h1,h2)::pairwise(hs)
      case _            =>
        xs
    }

    def merges(xs : List[WBLHeap[T]]) : WBLHeap[T] = xs match {
      case List(h) =>
        h
      case _ =>
        merges(pairwise(xs))
    }

    merges(xs.toList.map(Node.singleton(_)))
  }
}


private case object Empty extends WBLHeap[Nothing] {
  override def isEmpty =
    true

  override def insert[E](x: E)(implicit ord : Ordering[E]) =
    Node.singleton(x)

  override def minElem =
    throw HeapException("minElem on empty heap")

  override def delMin =
    throw HeapException("delMin on empty heap")

  val weight = 0
}


private object Node {

  def node[T](elem : T, left : WBLHeap[T], right : WBLHeap[T])
             (implicit ordering: Ordering[T]) = {
    val w = 1 + left.weight + right.weight
    if (left.weight >= right.weight)
      Node(elem, w, left, right)
    else
      Node(elem, w, right, left)
  }

  def singleton[T](elem : T)(implicit ord : Ordering[T]) =
    node(elem, Empty, Empty)

  def merge[T](h1 : WBLHeap[T], h2 : WBLHeap[T])(implicit ord : Ordering[T]) : WBLHeap[T] = {
    (h1, h2) match {
      case (Empty, _) =>
        h2
      case (_, Empty) =>
        h1
      case (Node(e1,_,l1,r1), Node(e2,_,l2,r2)) =>
        if(ord.lt(e1, e2))
          node(e1, merge(r1,h2), l1)
        else
          node(e2, merge(r2,h1), l2)
    }
  }
}


private case class Node[+T](elem : T, weight : Int, left : WBLHeap[T], right : WBLHeap[T])
                           (implicit ord: Ordering[T]) extends WBLHeap[T] {
  override def isEmpty =
    false

  override def minElem =
    elem

  override def delMin =
    Node.merge(left, right)

  override def insert[E >: T](x: E)(implicit ord: Ordering[E]) =
    Node.merge(this, Node.singleton(x))
}
