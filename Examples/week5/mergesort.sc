import math.Ordering

object mergesort {
  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (xs, Nil) => xs
        case (Nil, ys) => ys
        case (x :: xss, y :: yss) =>
          if (ord.lt(x, y)) x :: merge(xss, ys)
          else y :: merge(xs, yss)
      }
      val(fst, snd)= xs splitAt n
      // merge(msort(fst)(ord), msort(snd)(ord))
      merge(msort(fst), msort(snd))
    }
  }
}

val nums = List(-1, 3, 9, 2, 10, 33, 5)
mergesort.msort(nums)

val strs = List("strawberry", "apple", "banana", "pineapple", "lemon")
mergesort.msort(strs)