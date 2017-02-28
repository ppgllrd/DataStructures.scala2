/******************************************************************************
  * Mutable Binary Search Trees in Scala
  *
  * Data Structures in Scala
  * Pepe Gallardo, 2017.
  ******************************************************************************/

package dataStructures.mutable.SearchTree.BST

class BST[T](implicit val ord : Ordering[T]) {

  private trait Tree {
    def isEmpty() : Boolean
    def insert(e : T) : Tree
    def search(e : T) : Option[T]
    def delete(e : T) : Tree
  }

  private case object Empty extends Tree {
    override def isEmpty() = true

    override def insert(e : T) = {
      sz += 1
      Node(e, this, this)
    }

    override def search(e : T) = None

    override def delete(e: T) = this
  }

  private case class Node(var e : T, var left : Tree, var right:Tree) extends Tree {
    override def isEmpty() = false

    override def insert(e: T) = {
      val cmp = ord.compare(e, this.e)
      if(cmp < 0)
        left = left.insert(e)
      else if(cmp > 0)
        right = right.insert(e)
      else
        this.e = e
      this
    }

    override def search(e: T) = {
      val cmp = ord.compare(e, this.e)
      if(cmp < 0)
        left.search(e)
      else if(cmp > 0)
        right.search(e)
      else
        Some(this.e)
    }

    override def delete(e: T) = {
      def split(node: Node, temp: Node) : Tree = node.left match {
        case Empty => {
          temp.e = node.e
          node.right
        }
        case left : Node => {
          node.left = split(left, temp)
          node
        }
      }

      val cmp = ord.compare(e, this.e)
      if(cmp < 0) {
        left = left.delete(e)
        this
      } else if(cmp > 0) {
        right = right.delete(e)
        this
      } else {
        sz -= 1
        this match {
          case Node(x, Empty, right) => right
          case Node(x, left, Empty) => left
          case Node(x, left, right) => {
            right match {
              case right: Node =>
                this.right = split(right, this)
                this
            }
          }
        }
      }
    }
  }

  private abstract class TreeIterator extends Iterator[T] {
    val stack = scala.collection.mutable.Stack[Either[Node,Node]]()
    root match {
      case node : Node => save(node)
    }

    def save(node : Node)

    override def hasNext = stack.nonEmpty

    override def next() = {
      if(!hasNext)
        throw new NoSuchElementException

      var either = stack.top
      stack.pop()
      while (either.isRight) {
        val node = either.right.get
        save(node)
        either = stack.top
        stack.pop()
      }
      either.left.get.e
    }
  }

  def inOrder() : Iterable[T] = new Iterable[T] {
    override def iterator = new TreeIterator {
      override def save(node: Node) {
        node.right match {
          case node: Node => stack.push(Right(node))
          case _ =>
        }
        stack.push(Left(node))
        node.left match {
          case node: Node => stack.push(Right(node))
          case _ =>
        }
      }
    }
  }

  def preOrder() : Iterable[T] = new Iterable[T] {
    override def iterator = new TreeIterator {
      override def save(node: Node) {
        node.right match {
          case node: Node => stack.push(Right(node))
          case _ =>
        }
        node.left match {
          case node: Node => stack.push(Right(node))
          case _ =>
        }
        stack.push(Left(node))
      }
    }
  }

  def postOrder() : Iterable[T] = new Iterable[T] {
    override def iterator = new TreeIterator {
      override def save(node: Node) {
        stack.push(Left(node))
        node.right match {
          case node: Node => stack.push(Right(node))
          case _ =>
        }
        node.left match {
          case node: Node => stack.push(Right(node))
          case _ =>
        }
      }
    }
  }

  private var sz : Int = 0
  private  var root : Tree = Empty

  def size() : Int = sz

  def isEmpty() : Boolean = root.isEmpty()

  def insert(e : T) { root = root.insert(e) }

  def search(e : T) : Option[T] = root.search(e)

  def delete(e : T) { root = root.delete(e) }
}

object BST {
  def apply[T](implicit ord : Ordering[T]) : BST[T] = new BST[T]()(ord)

  def apply[T](xs : T*)(implicit ord : Ordering[T]) : BST[T] = {
    val bst = new BST[T]()(ord)
    xs.foreach(bst.insert)
    bst
  }
}