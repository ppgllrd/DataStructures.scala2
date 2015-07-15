/*****************************************************************************
 * Trait for Priority Queues
 *
 * Data Structures in Scala
 * Pepe Gallardo, 2015.
 ******************************************************************************/

package dataStructures.immutable.PriorityQueue

trait PriorityQueue[+T] {
  def isEmpty : Boolean
  def first : T
  def dequeue : PriorityQueue[T]
  def enqueue[E >: T](e : E)(implicit ord : Ordering[E]) : PriorityQueue[E]

  protected def elems : List[T] =
    if(isEmpty)
      List()
    else
      first :: dequeue.elems
}

