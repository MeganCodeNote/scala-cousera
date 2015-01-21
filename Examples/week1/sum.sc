// A tail recursion version of sum
// sum: sum up integers from a through b

object Sum {
  def sum(fun: Int => Int, a: Int, b: Int): Int = {
    def helper(base: Int, acc: Int): Int = {
      if (base > b) acc
      else helper(base + 1, acc + fun(base))
    }

    if (a > b) sum(fun, b, a)
    else helper(a, 0)
  }

  // test
  sum(_ * 1, 3, 1)
}