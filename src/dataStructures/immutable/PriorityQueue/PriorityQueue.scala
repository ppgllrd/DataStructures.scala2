/*****************************************************************************
 * Trait for Priority Queues
 *
 * Data Structures in Scala
 * Pepe Gallardo, 2015.
 ******************************************************************************/

package dataStructures.immutable.PriorityQueue

trait PriorityQueue[+T] extends IsPriorityQueue[T, PriorityQueue] {

  protected def elems : List[T] =
    if(isEmpty)
      List()
    else
      first :: dequeue.elems
}
