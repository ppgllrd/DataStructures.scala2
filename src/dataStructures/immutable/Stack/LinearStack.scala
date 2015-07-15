/******************************************************************************
 * Stacks implemented with a linked linear structure
 *
 * Data Structures in Scala
 * Pepe Gallardo, 2015.
 ******************************************************************************/

package dataStructures.immutable.Stack

import dataStructures.utils.Utils
import dataStructures.utils.Utils.cmpIter

sealed trait LinearStack[+T] extends Stack[T] with IsStack[T,LinearStack] with Utils[T] {
  protected val className = "LinearStack"

  def canEqual(o : Any) = o.isInstanceOf[LinearStack[T]]

  override def equals(that : Any) = that match {
    case that : LinearStack[T] => that.canEqual(this) && cmpIter(this.iter,that.iter)
    case _                     => false
  }
}

object LinearStack {
  def apply[T]() : LinearStack[T] = Empty
}

private object Empty extends LinearStack[Nothing] {
  def isEmpty = true
  def top = throw new StackException("top on empty stack")
  def pop = throw new StackException("pop on empty stack")
  def push[E](x :E) = Node(x, this)
}

private case class Node[+T](t : T, s : LinearStack[T]) extends LinearStack[T] {
  def isEmpty = false
  def top = t
  def pop = s
  def push[E >: T](x : E) = Node(x, this)
}
