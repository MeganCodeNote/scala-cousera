object polynomials {
  class Poly(val terms: Map[Int, Double]) {  // primary constructor
    // auxiliary construction, takes arbitrary parameters
    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    // addition -- version 1 using ++
    val termsWithDefaults = terms.withDefaultValue(0.0)
    def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + termsWithDefaults(exp))
    }

    // addition -- version 2 using foldLeft
    def add (other: Poly) = new Poly((terms foldLeft other.termsWithDefaults)(addTerm))
    def addTerm(termsWithDefaults: Map[Int, Double], term: (Int, Double)) = {
      val (exp, coeff) = term
      val updatedTerm = exp -> (coeff + termsWithDefaults(exp))
      termsWithDefaults + updatedTerm
    }

    // to string
    override def toString = {
      val sortedTerms = terms.toList.sorted.reverse
      val sortedTermsStr = for ((exp, coeff) <- sortedTerms)
                           yield coeff + "x^" + exp
      sortedTermsStr mkString " + "
    }
  }

  p1.termsWithDefaults(9)  // show the default value
  val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
  val p2 = new Poly(0 -> 3.0, 3 -> 7.0)
  p1 + p2
  p1 add p2
}
