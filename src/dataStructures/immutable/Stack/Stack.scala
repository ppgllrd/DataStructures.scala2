/*****************************************************************************
 * Trait for Stacks
 *
 * Data Structures in Scala
 * Pepe Gallardo, 2015.
 ******************************************************************************/

package dataStructures.immutable.Stack

trait Stack[+T] extends IsStack[T, Stack] {

  protected def elems : List[T] =
    if(isEmpty)
      List()
    else
      top :: pop.elems
}
