object fold {

  def reverse[T](xs: List[T]): List[T] =
    (xs foldLeft List[T]())((z, x) => x :: z)

  def mapFun[T, U](xs: List[T])(op: T => U): List[U] =
    (xs foldRight List[U]())((x, z) => op(x) :: z)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((x, z) => 1 + z)
}

val nums = List(-1, 3, 9, 5)

val reverse = fold.reverse(nums)
val double = fold.mapFun(nums)(_ * 2)
val len = fold.lengthFun(nums)