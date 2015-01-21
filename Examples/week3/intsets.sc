abstract class IntSet {
  def contains(x: Int): Boolean
  def incl(x: Int): IntSet
  def union(that: IntSet): IntSet
}

object Empty extends IntSet { // singleton, no 'new'
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(that: IntSet): IntSet = that
  override def toString = ""
}

class NonEmpty(val elem: Int, val leftSet: IntSet, val rightSet: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x == elem) true
    else if (x < elem) leftSet.contains(x)
    else rightSet.contains(x)
  }
  def incl(x: Int): IntSet = {
    if (x == elem) this
    else if (x < elem) new NonEmpty(elem, leftSet.incl(x), rightSet)
    else new NonEmpty(elem, leftSet, rightSet.incl(x))
  }
  def union(that: IntSet): IntSet = {
    ((leftSet union rightSet) union that) incl elem
  }
  override def toString = "{" + leftSet + elem + rightSet + "}"
}

val s1 = new NonEmpty(3, Empty, Empty)
val s2 = s1 incl 4
