/******************************************************************************
 * Priority Queues implemented with an ordered linked linear structure
 *
 * Data Structures in Scala
 * Pepe Gallardo, 2015.
 ******************************************************************************/


package dataStructures.immutable.PriorityQueue

trait LinearPriorityQueue[+T] extends PriorityQueue[T] {
  override def dequeue : LinearPriorityQueue[T]
  override def enqueue[E >: T](e : E)(implicit ord : Ordering[E]) : LinearPriorityQueue[E]
  override def toString =
    elems.reverse.mkString("LinearPriorityQueue(",",",")")


  def canEqual(o : Any) = o.isInstanceOf[LinearPriorityQueue[T]]
  override def equals(that : Any) = that match {
    case that : LinearPriorityQueue[T] => that.canEqual(this) && this.elems==that.elems
    case _                             => false
  }

  override def hashCode : Int = {
    val p = 31
    var h = 17
    for(e <- elems)
      h += p*h + e.hashCode
    return h
  }
}


object LinearPriorityQueue {
  // O(1)
  def apply[T]() : LinearPriorityQueue[T] = EmptyPQ

  // O(n^2)
  def apply[T](xs : T*)(implicit ord : Ordering[T]) : LinearPriorityQueue[T] = {
    var q = LinearPriorityQueue[T]()
    for(x <- xs)
      q = q.enqueue(x)
    return q
  }
}

private object EmptyPQ extends LinearPriorityQueue[Nothing] {
  def isEmpty = true

  def first = throw new PriorityQueueException("first on empty queue")

  def dequeue = throw new PriorityQueueException("dequeue on empty queue")

  def enqueue[E](e : E)(implicit ord : Ordering[E]) = NodePQ(e, EmptyPQ)
}

private case class NodePQ[+T](hd : T, tl : LinearPriorityQueue[T]) extends LinearPriorityQueue[T] {
  // O(1)
  def isEmpty = false

  // O(1)
  def first = hd

  // O(1)
  def dequeue = tl

  // O(n)
  def enqueue[E >: T](e : E)(implicit ord : Ordering[E]) =
    if(ord.compare(e,hd) < 0)
      NodePQ(e, this)
    else
      NodePQ(hd, tl.enqueue(e))
}
