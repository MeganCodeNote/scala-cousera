object nqueens {
  def nqueens(n: Int): Set[List[Int]] = {
    def solve(k: Int): Set[List[Int]] = k match {
      case 0 => Set(Nil)
      case k => for {
        queens <- solve(k - 1)   // this line comes first of the nested loop
        col <- 0 until n         // this line could not be before line 6, type error
        if isSafe(col, queens)
      } yield col :: queens
    }
    solve(n)
  }

  def isSafe(col: Int, res: List[Int]): Boolean = {
    val row = res.length
    val queensWithRows = (res.length - 1 to 0 by -1) zip res
    queensWithRows forall {
      case (r, c) =>  (col != c) && (row - r != math.abs(col - c))
    }
  }
}

def show(queens: List[Int])  {
  val lines =
    for (col <- queens.reverse)
    yield {
      // Vector.fill(10)("* ").updated(3, "Q ").mkString
      // res2: String = "* * * Q * * * * * * "

      // Vector.fill(10)("* ").updated(3, "Q ").toString
      // res4: String = Vector(* , * , * , Q , * , * , * , * , * , * )

      Vector.fill(queens.length)("* ").updated(col, "Q ").mkString + "\n"
    }

  println("\n" + (lines mkString "\n"))
}

nqueens.nqueens(4) map show