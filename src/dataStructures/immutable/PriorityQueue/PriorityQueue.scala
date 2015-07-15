/*****************************************************************************
 * Trait for Priority Queues
 *
 * Data Structures in Scala
 * Pepe Gallardo, 2015.
 ******************************************************************************/

package dataStructures.immutable.PriorityQueue

trait PriorityQueue[+T] extends IsPriorityQueue[T, PriorityQueue] {

  private class PQIter[T](var q : PriorityQueue[T]) extends Iterator[T] {
    def hasNext() = !q.isEmpty

    def next() : T =
      if(!hasNext())
        throw new UnsupportedOperationException("next on exhausted iterator")
      else {
        val f = q.first
        q = q.dequeue
        return f
      }
  }

  protected def iter : Iterator[T] = new PQIter[T](this)
}
