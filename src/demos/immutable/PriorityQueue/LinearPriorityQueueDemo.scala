/******************************************************************************
 * Simple demos of Linear Priority Queues usage
 *
 * Data Structures in Scala
 * Pepe Gallardo, 2015.
 ******************************************************************************/

package demos.immutable.PriorityQueue

import dataStructures.immutable.PriorityQueue.{PriorityQueue, LinearPriorityQueue}

object LinearPriorityQueueDemo extends App {

  val q1: LinearPriorityQueue[Int] = LinearPriorityQueue().enqueue(1).enqueue(5).enqueue(0)

  val q2: PriorityQueue[Int] = LinearPriorityQueue().enqueue(1).enqueue(5).enqueue(0)

  val q3: LinearPriorityQueue[Int] = LinearPriorityQueue(1,5,0)

  val q4: PriorityQueue[Int] = LinearPriorityQueue(1,5,0)

  val q5: PriorityQueue[Int] = q1

  val q6: PriorityQueue[Int] = q3.dequeue

  val q7: LinearPriorityQueue[Int] = q1.dequeue


  println(q1==q2)
  println(q1==q3)
  println(q1==q6)



  case class A(val x: Int)

  class B(override val x: Int) extends A(x) {
    override def toString = "B("+x+")"
  }

  implicit object AOrdering extends Ordering[A] {
    def compare(b1: A, b2: A) = b1.x.compareTo(b2.x)
  }

  implicit object BOrdering extends Ordering[B] {
    def compare(b1: B, b2: B) = b1.x.compareTo(b2.x)
  }


  val q8: LinearPriorityQueue[A] = LinearPriorityQueue(new A(1), new B(2))

  val q9: LinearPriorityQueue[B] = LinearPriorityQueue(new B(1))

  val q10: PriorityQueue[A] = q8

  println(q9)
  println(q10)
  println(q1)
  println(q1.first)

}
