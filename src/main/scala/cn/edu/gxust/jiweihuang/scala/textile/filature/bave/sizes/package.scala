package cn.edu.gxust.jiweihuang.scala.textile.filature.bave

import cn.edu.gxust.jiweihuang.scala.textile._
import cn.edu.gxust.jiweihuang.scala.utils.DoubleSeq

package object sizes {

  final class Size(value: Double,
                   units: LengthBased, val index: Int,
                   val testLength: Double)
    extends Fineness(value, units) {

    override def toString: String = units match {
      case Denier => s"Size($value, Denier, $index, $testLength)"
      case Tex => s"Size($value, Tex, $index, $testLength)"
      case DTex => s"Size($value, DTex, $index, $testLength)"
      case _: LengthBased => s"Size($value, $units, $index, $testLength)"
    }

    override def canEqual(other: Any): Boolean = other.isInstanceOf[Size]

    override def equals(other: Any): Boolean = other match {
      case that: Size =>
        super.equals(that) &&
          (that canEqual this) &&
          value == that.value &&
          units == that.units &&
          index == that.index &&
          testLength == that.testLength
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(super.hashCode(), value, units, index, testLength)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }

  object Size {
    def apply(value: Double, units: LengthBased,
              index: Int, testLength: Double): Size = new Size(value, units, index, testLength)

    def apply(value: Double, units: String,
              index: Int, testLength: Double): Size = {
      units match {
        case "Denier" => new Size(value, Denier, index, testLength)
        case "Tex" => new Size(value, Tex, index, testLength)
        case "DTex" => new Size(value, DTex, index, testLength)
        case _: String => throw new IllegalArgumentException(s"$units are not currently supported.")
      }
    }

    def unapply(arg: Size): Option[(Double, FinenessUnits, Int, Double)] =
      if (arg == null) None else Some(arg.value, arg.units, arg.index, arg.testLength)

    def toDenier(size: Size): Size = {
      size.units match {
        case Denier => new Size(size.value, Denier, size.index, size.testLength)
        case Tex => new Size(size.value * (Denier.length / Tex.length), Denier, size.index, size.testLength)
        case DTex => new Size(size.value * (Denier.length / DTex.length), Denier, size.index, size.testLength)
        case _: LengthBased => throw new IllegalArgumentException(s"${size.units} are not currently supported.")
      }
    }

    def toTex(size: Size): Size = {
      size.units match {
        case Denier => new Size(size.value * (Tex.length / Denier.length), Tex, size.index, size.testLength)
        case Tex => new Size(size.value, Tex, size.index, size.testLength)
        case DTex => new Size(size.value * (Tex.length / DTex.length), Tex, size.index, size.testLength)
        case _: LengthBased => throw new IllegalArgumentException(s"${size.units} are not currently supported.")
      }
    }

    def toDTex(size: Size): Size = {
      size.units match {
        case Denier => new Size(size.value * (DTex.length / Denier.length), DTex, size.index, size.testLength)
        case Tex => new Size(size.value * (DTex.length / Tex.length), DTex, size.index, size.testLength)
        case DTex => new Size(size.value, DTex, size.index, size.testLength)
        case _: LengthBased => throw new IllegalArgumentException(s"${size.units} are not currently supported.")
      }
    }
  }

  class Sizes(data: Vector[Double], val units: LengthBased,
              val testLength: Double) extends DoubleSeq(data) {

  }

  object Sizes {

  }

  class SizesGroup(val name: String, val units: LengthBased,
                   val testLength: Double) {

  }

  object SizesGroup {

  }

}
