class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y


  def add(that: Rational) = {
    new Rational(numer * that.denom + denom * that.numer, denom * that.denom)
  }
  def + (that: Rational) = add(that)

  def mul(that: Rational) = {
    new Rational(numer * that.numer, denom * that.denom)
  }
  def * (that: Rational) = mul(that)

  def neg = new Rational(-numer, denom)
  def unary_- = neg  // need an extra space after the symbol

  def sub(that: Rational) = add(that.neg)
  def - (that: Rational) = sub(that)

  override def toString = x + "/" + y
}

val x = new Rational(1, 2)
val y = new Rational(5, 6)
x.numer
y.denom

x.add(y)
x.mul(y)
-x + y * x


