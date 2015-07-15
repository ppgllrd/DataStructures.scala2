/******************************************************************************
 * Simple demos of Stacks usage
 *
 * Data Structures in Scala
 * Pepe Gallardo, 2015.
 ******************************************************************************/

package demos.Stack

import dataStructures.immutable.Stack.{Stack, LinearStack}

object StackDemo extends App {

  val s1 : Stack[Int] = LinearStack().push(3).push(2).push(1)

  val s2 : LinearStack[Int] = LinearStack().push(3).push(2).push(1)

  val s3 = s2.pop

  val s4 : Stack[Int] = s2

  println(s1)
  println(s2)
  println(s3)
  println(s4)
  println(s1==s2)
  println(s2==s3)

}
