object NewtonSqrt {
  def abs(x: Double) = if (x < 0) -x else x

  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double = {
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))
    }

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.0001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    // initialize a guess with value 1
    sqrtIter(1.0)
  }

  // test cases
  sqrt(5)
  sqrt(9)
  sqrt(0.0001)

  sqrt(1e-6)      // not accurate
  sqrt(1e60)      // two guesses are far away from each other
}