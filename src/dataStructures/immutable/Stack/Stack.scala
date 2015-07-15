/*****************************************************************************
 * Trait for Stacks
 *
 * Data Structures in Scala
 * Pepe Gallardo, 2015.
 ******************************************************************************/

package dataStructures.immutable.Stack

trait Stack[+T] extends IsStack[T, Stack] {

  private class StackIter[T](var s : Stack[T]) extends Iterator[T] {
    def hasNext() = !s.isEmpty

    def next() : T =
      if(!hasNext())
        throw new UnsupportedOperationException("next on exhausted iterator")
      else {
        val t = s.top
        s = s.pop
        return t
      }
  }

  protected def iter : Iterator[T] = new StackIter[T](this)
}
