import scala.io.Source

object phonemnem {
  /* Get the dictionary of words(all letters) from url */
  val url = "http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt"
  val in = Source.fromURL(url)
  val words = in.getLines.toList filter (wrd => wrd forall (ch => ch.isLetter))

  /* Define the mnem */
  val mnem = Map( '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
                  '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  /* Maps a char 'A-Z' -> '2-9' */
  val charCode = for ((digit, letters) <- mnem; letter <- letters) yield (letter -> digit)

  /* Maps a word to digits */
  def wordCode(word: String) = word.toUpperCase map charCode  // map is a fuction

  /* Group words with same digits together */
  val wordsForNum = (words groupBy wordCode) withDefaultValue Nil

  /* Covert a phoneNumber into a list of words */
  def translate(nums: String): Set[List[String]] = {
    if (nums.isEmpty) Set(Nil)
    else {
      for {
        split <- 1 to nums.length
        word <- wordsForNum(nums take split)
        restWords <- translate(nums drop split)
      } yield word :: restWords
    }.toSet
  }

  translate("7225247386")
}