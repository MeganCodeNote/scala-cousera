// This is a tail-recursion version of factorial
object Exercise {
  def factorial(n: Int) = {
    def iter(result: Int, n: Int): Int = {
      if (n <= 1) result
      else iter(result * n, n - 1)
    }
    iter(1, n)
  }
  factorial(5)
}