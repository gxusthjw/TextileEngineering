package cn.edu.gxust.jiweihuang.scala.utils

class DoubleSeq(protected val data: Vector[Double]) extends Iterable[Double] {
  override def iterator: Iterator[Double] = data.iterator
}
