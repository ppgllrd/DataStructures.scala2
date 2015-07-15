/******************************************************************************
 * Basic signature for Stacks
 *
 * Data Structures in Scala
 * Pepe Gallardo, 2015.
 ******************************************************************************/

package dataStructures.immutable.Stack

trait IsStack[+T, +Repr[+X] <: IsStack[X,Repr]] {
  def isEmpty : Boolean
  def top : T
  def push[E >: T](x : E) : Repr[E]
  def pop : Repr[T]
}
