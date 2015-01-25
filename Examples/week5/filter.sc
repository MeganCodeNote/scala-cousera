object filter {
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case (x :: xss) => {
      val (fst, snd) = xss span (_ == x)
      (x :: fst) :: pack(snd)
    }
  }
  
  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (x => (x.head, x.length))
}

val x = List("a", "a", "a", "b", "b", "a", "a", "e")
filter.pack(x)
filter.encode(x)