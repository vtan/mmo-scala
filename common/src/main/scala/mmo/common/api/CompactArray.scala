package mmo.common.api

sealed trait CompactArray {
  def uncompact(size: Int): Iterator[Option[Int]]
}

object CompactArray {

  def chooseSmallest(data: Seq[Option[Int]], emptyValue: Int): CompactArray = {
    val sparse = SparseCompactArray.from(data)
    val rle = RunLengthCompactArray.from(data, emptyValue)

    if (sparse.entries.length < rle.runs.length) {
      sparse
    } else {
      rle
    }
  }
}

final case class SparseCompactArray(
  entries: Array[SparseCompactArray.Entry]
) extends CompactArray {

  override def uncompact(size: Int): Iterator[Option[Int]] = {
    val map = entries.map(e => e.index -> e.value).toMap
    (0 until size).iterator.map(map.get)
  }
}

object SparseCompactArray {

  final case class Entry(
    value: Int,
    index: Int
  )

  def from(data: Seq[Option[Int]]): SparseCompactArray =
    SparseCompactArray(data.iterator
      .zipWithIndex
      .collect { case (Some(value), index) => Entry(value, index) }
      .toArray
    )
}

final case class RunLengthCompactArray(
  emptyValue: Int,
  runs: Array[RunLengthCompactArray.Entry]
) extends CompactArray {

  override def uncompact(size: Int): Iterator[Option[Int]] =
    runs.iterator.flatMap { entry =>
      val elem = Some(entry.value).filter(_ != emptyValue)
      Iterator.fill(entry.runLength)(elem)
    }
}

object RunLengthCompactArray {

  final case class Entry(
    runLength: Int,
    value: Int
  )

  def from(data: Seq[Option[Int]], emptyValue: Int): RunLengthCompactArray =
    RunLengthCompactArray(
      emptyValue = emptyValue,
      runs = runLength(data.map(_.getOrElse(emptyValue)))
        .map((Entry.apply _).tupled)
        .toArray
    )

  private def runLength[T](xs: Seq[T]): Seq[(Int, T)] = xs match {
    case head +: tail =>
      val (before, after) = tail.span(_ == head)
      (before.length + 1, head) +: runLength(after)
    case Nil => Nil
  }
}
