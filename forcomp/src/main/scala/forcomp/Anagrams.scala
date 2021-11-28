package forcomp


object Anagrams {

  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]

  val dictionary: List[Word] = loadDictionary

  def wordOccurrences(w: Word): Occurrences = w.toLowerCase.groupBy(identity).mapValues(_.length).toList.sorted

  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString)

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(wordOccurrences)

  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.getOrElse(wordOccurrences(word), List())

  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case List() => List(Nil)
    case (char, num) :: tail =>
      val tails = combinations(tail)
      tails ::: (for {
        j <- tails
        i <- 1 to num
      } yield (char, i) :: j)
  }

  def subtract(x: Occurrences, y: Occurrences): Occurrences =
    ((y foldLeft x.toMap) ((r, c) => {
      r.updated(c._1, r(c._1) - c._2)
    }) filter (_._2 > 0)).toList.sortWith(_._1 < _._1)

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def sentenceAnagramsInner(o: Occurrences): List[Sentence] = {
      if (o.isEmpty) List(Nil)
      else
        for {
          combinations <- combinations(o)
          word <- dictionaryByOccurrences.getOrElse(combinations, List())
          anagrams <- sentenceAnagramsInner(subtract(o, combinations))
        } yield word :: anagrams
    }

    sentenceAnagramsInner(sentenceOccurrences(sentence))
  }
}
