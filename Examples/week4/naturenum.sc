// Demo of construction Nature numbers using ONLY FIRST PRINCIPLE
// this is called peano numbers

abstract class Nat {
  def isZero: Boolean
  def pred: Nat
  def succ: Nat = new Succ(this)
  def +(that: Nat): Nat
  def -(that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = true
  def pred: Nothing = throw new Exception("0.predecessor")
  def +(that: Nat): Nat = that
  def -(that: Nat): Nat = {
    if (that.isZero) this
    else throw new Exception("negative number")
  }
}

class Succ(val pred: Nat) extends Nat {
  def isZero: Boolean = true
  def +(that: Nat): Nat = that match {
    case Zero => that
    case _ => pred + new Succ(that)  // or new Succ(pred + that)
  }
  def -(that: Nat): Nat = {
    if (that.isZero) this
    else pred - that.pred
  }
}