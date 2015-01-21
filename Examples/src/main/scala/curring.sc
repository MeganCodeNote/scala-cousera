object Curring {
  // Exercise 1: product (asert a <= b)
  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }
  product(2 * _)(4, 5)

  // Exercise 2: define factorial using product
  def factorial(n: Int): Int = product(x => x)(1, n)
  factorial(6)

  // Exercise 3: generalize sum and product
  def mapReduce(mapper: Int => Int, reducer:(Int, Int) => Int, acc: Int)(a: Int, b: Int): Int = {
    if (a > b) acc
    else reducer(mapper(a), mapReduce(mapper, reducer, acc)(a + 1, b))
  }
  def sum(a: Int, b: Int) = mapReduce(x => x, _ + _, 0)(a, b)
  def product2(a: Int, b: Int) = mapReduce(_ * 1, _ * _, 1)(a, b)
  sum(1, 5)
  product2(1, 5)
}