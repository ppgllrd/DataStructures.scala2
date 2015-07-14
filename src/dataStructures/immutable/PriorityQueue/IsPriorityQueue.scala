/******************************************************************************
 * Basic signature for Priority Queues
 *
 * Data Structures in Scala
 * Pepe Gallardo, 2015.
 ******************************************************************************/

package dataStructures.immutable.PriorityQueue

trait IsPriorityQueue[+T, +Repr[+X] <: IsPriorityQueue[X,Repr]] {
  def isEmpty : Boolean
  def first : T
  def enqueue[E >: T](x : E)(implicit ord : Ordering[E]) : Repr[E]
  def dequeue : Repr[T]
}