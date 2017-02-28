package demos.immutable.Heap

import dataStructures.immutable.Heap.WBLHeap

object WBLHeapDemo extends App {
  val h1 = WBLHeap(1,6,2,3,0,5,1)
  println(h1)
  var h = h1
  while(!h.isEmpty) {
    println(h.minElem)
    h = h.delMin
  }


}