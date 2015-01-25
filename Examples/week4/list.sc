trait List[+T] {
  def head: T
  def tail: List[T]
  def isEmpty: Boolean
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  def isEmpty = false
  override def toString = head + ", " + tail
}

// QUESTION: why not make Nil an object?
// ANSER:    since object is a value, could not have generics
class Nil[T] extends List[T] {
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  def isEmpty = true
  override def toString = "#"
}

//  this is the alternative implementation using bounds and variants
object Nil extends List[Nothing] {
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  def isEmpty = true
  override def toString = "#"
}

/* Test the List[T] type */
object List {
  // version 1:
  //  def apply[T](): List[T] = new Nil
  //  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))

  // version 2:
  def apply[T](): List[T] = Nil
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, Nil))
}

List()      // List.apply()
List(1, 2)  // List.apply(1, 2)
val x: List[String] = Nil  // List is covariant
