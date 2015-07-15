/******************************************************************************
 * Stacks implemented with a linked linear structure
 *
 * Data Structures in Scala
 * Pepe Gallardo, 2015.
 ******************************************************************************/

package dataStructures.immutable.Stack

sealed trait LinearStack[+T] extends Stack[T] with IsStack[T,LinearStack] {
  override def toString =
    elems.mkString("LinearStack(",",",")")

  def canEqual(o : Any) = o.isInstanceOf[LinearStack[T]]
  override def equals(that : Any) = that match {
    case that : LinearStack[T] => that.canEqual(this) && this.elems==that.elems
    case _                     => false
  }

  override def hashCode : Int = {
    val p = 31
    var h = 17
    for(e <- elems)
      h += p*h + e.hashCode
    return h
  }
}

object LinearStack {
  def apply[T]() : LinearStack[T] = EmptyS
}

private object EmptyS extends LinearStack[Nothing] {
  def isEmpty = true
  def top = throw new StackException("top on empty stack")
  def pop = throw new StackException("pop on empty stack")
  def push[E](x :E) = NodeS(x, this)
}

private case class NodeS[+T](t : T, s : LinearStack[T]) extends LinearStack[T] {
  def isEmpty = false
  def top = t
  def pop = s
  def push[E >: T](x : E) = NodeS(x, this)
}
