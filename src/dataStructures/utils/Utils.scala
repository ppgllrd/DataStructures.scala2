/******************************************************************************
 * Common utilities used by several classes
 *
 * Data Structures in Scala
 * Pepe Gallardo, 2015.
 ******************************************************************************/

package dataStructures.utils

object Utils {

  def cmpIter[T](it1 : Iterator[T], it2 : Iterator[T]) : Boolean = {
    var eq = true
    while(eq) {
      if(!it1.hasNext)
        return !it2.hasNext
      else if(!it2.hasNext)
        return !it1.hasNext
      else
        eq = it1.next() == it2.next()
    }
    return eq
  }

  def hashCodeIter[T](it : Iterator[T]) : Int = {
    val p = 31
    var h = 17
    while(it.hasNext)
      h += p*h + it.next().hashCode
    return h
  }

  def toSringIter[T](className : String, it : Iterator[T]) : String = {
    var s = className+"("
    while(it.hasNext) {
      s += it.next()
      if(it.hasNext)
        s += ","
    }
    return s+")"
  }

}

trait Utils[+T] {
  import dataStructures.utils.Utils.{hashCodeIter, toSringIter}

  protected val className : String

  protected def iter : Iterator[T]

  override def toString = toSringIter(className, this.iter)

  override def hashCode = hashCodeIter(this.iter)
}
