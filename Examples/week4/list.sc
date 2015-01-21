trait List[T] {
  def head: T
  def tail: List[T]
  def isEmpty: Boolean
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  def isEmpty = false
  override def toString = head + ", " + tail
}

class Nil[T] extends List[T] {
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  def isEmpty = true
  override def toString = "#"
}

object List {
  // List() = List.apply()
  def apply[T](): List[T] = new Nil
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
}

List()
List(1, 2)
