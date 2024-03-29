/******************************************************************************
 * Priority Queues implemented with an ordered linked linear structure
 *
 * Data Structures in Scala
 * Pepe Gallardo, 2015.
 ******************************************************************************/

package dataStructures.immutable.PriorityQueue

import dataStructures.utils.Utils
import dataStructures.utils.Utils.cmpIter

sealed trait LinearPriorityQueue[+T] extends PriorityQueue[T] with IsPriorityQueue[T,LinearPriorityQueue] with Utils[T] {
  protected val className = "LinearPriorityQueue"

  def canEqual(o : Any) = o.isInstanceOf[LinearPriorityQueue[T]]

  override def equals(that : Any) = that match {
    case that : LinearPriorityQueue[T] => that.canEqual(this) && cmpIter(this.iter,that.iter)
    case _                             => false
  }
}

object LinearPriorityQueue {
  // O(1)
  def apply[T]() : LinearPriorityQueue[T] = Empty

  // O(n^2)
  def apply[T](xs : T*)(implicit ord : Ordering[T]) : LinearPriorityQueue[T] = {
    var q = LinearPriorityQueue[T]()
    for(x <- xs)
      q = q.enqueue(x)
    return q
  }
}

private object Empty extends LinearPriorityQueue[Nothing] {
  def isEmpty = true
  def first = throw new PriorityQueueException("first on empty queue")
  def dequeue = throw new PriorityQueueException("dequeue on empty queue")
  def enqueue[E](x : E)(implicit ord : Ordering[E]) = Node(x,this)
}

private case class Node[+T](hd : T, tl : LinearPriorityQueue[T]) extends LinearPriorityQueue[T] {
  def isEmpty = false
  def first = hd
  def dequeue = tl
  def enqueue[E >: T](x : E)(implicit ord : Ordering[E]) =
    if(ord.compare(x,hd)<0)
      Node(x,this)
    else
      Node(hd,tl.enqueue(x))
}