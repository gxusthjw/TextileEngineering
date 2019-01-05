package cn.edu.gxust.jiweihuang.scala

package object textile {

  /**
    * The fineness units of yarn, silk or fiber.
    * The keywords {{{abstract}}} is used to
    * prevent instantiation of class {{{FinenessUnits}}},
    *
    * @param weight the weight component of fineness units.
    * @param length the length component of fineness units.
    */
  abstract class FinenessUnits(val weight: Double, val length: Double) {

    override def toString: String = s"FinenessUnits($weight, $length)"

    def canEqual(other: Any): Boolean = other.isInstanceOf[FinenessUnits]

    override def equals(other: Any): Boolean = other match {
      case that: FinenessUnits =>
        (that canEqual this) &&
          weight == that.weight &&
          length == that.length
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(weight, length)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }

  }

  /** similarly to enumeration */
  object FinenessUnits {
    val METRIC_NUMBER: FinenessUnits = MetricNumber
    val DENIER: FinenessUnits = Denier
    val TEX: FinenessUnits = Tex
    val DTEX: FinenessUnits = DTex
  }

  /** Weight Based System */
  abstract class WeightBased(weight: Double, length: Double)
    extends FinenessUnits(weight, length) {

    override def toString: String = s"WeightBased($weight, $length)"

    override def canEqual(other: Any): Boolean = other.isInstanceOf[WeightBased]

    override def equals(other: Any): Boolean = other match {
      case that: WeightBased =>
        super.equals(that) &&
          (that canEqual this) &&
          weight == that.weight &&
          length == that.length
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(super.hashCode(), weight, length)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }

  /** Length Based System */
  abstract class LengthBased(weight: Double, length: Double)
    extends FinenessUnits(weight, length) {

    override def toString: String = s"LengthBased($weight, $length)"

    override def canEqual(other: Any): Boolean = other.isInstanceOf[LengthBased]

    override def equals(other: Any): Boolean = other match {
      case that: LengthBased =>
        super.equals(that) &&
          (that canEqual this) &&
          weight == that.weight &&
          length == that.length
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(super.hashCode(), weight, length)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }

  /** Linear Density System */
  abstract class LinearDensity(weight: Double, length: Double)
    extends LengthBased(weight, length) {

    override def toString: String = s"LinearDensity($weight, $length)"

    override def canEqual(other: Any): Boolean = other.isInstanceOf[LinearDensity]

    override def equals(other: Any): Boolean = other match {
      case that: LinearDensity =>
        super.equals(that) &&
          (that canEqual this) &&
          weight == that.weight &&
          length == that.length
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(super.hashCode(), weight, length)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }

  /** 1 N = 1 m / 1 g */
  object MetricNumber extends WeightBased(1.0, 1.0) {
    override def toString: String = f"MetricNumber($weight%.0f, $length%.0f)"
  }

  /** 1 D = 1 g / 9000 m */
  object Denier extends LengthBased(1.0, 9000.0) {
    override def toString: String = f"Denier($weight%.0f, $length%.0f)"
  }

  /** 1 tex = 1 g / 1000 m */
  object Tex extends LinearDensity(1.0, 1000.0) {
    override def toString: String = f"Tex($weight%.0f, $length%.0f)"
  }

  /** 1 dtex = 1 g / 10000 m */
  object DTex extends LinearDensity(1.0, 10000.0) {
    override def toString: String = f"DTex($weight%.0f, $length%.0f)"
  }

  class Fineness(val value: Double, val units: FinenessUnits) {
    def apply: Double = value

    override def toString: String = units match {
      case MetricNumber => s"Fineness($value, MetricNumber)"
      case Denier => s"Fineness($value, Denier)"
      case Tex => s"Fineness($value, Tex)"
      case DTex => s"Fineness($value, DTex)"
      case _: FinenessUnits => s"Fineness($value, $units)"
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[Fineness]

    override def equals(other: Any): Boolean = other match {
      case that: Fineness =>
        (that canEqual this) &&
          value == that.value &&
          units == that.units
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(value, units)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }

  object Fineness {
    def apply(value: Double, units: FinenessUnits): Fineness = new Fineness(value, units)

    def apply(value: Double, units: String): Fineness = {
      units match {
        case "MetricNumber" => new Fineness(value, MetricNumber)
        case "Denier" => new Fineness(value, Denier)
        case "Tex" => new Fineness(value, Tex)
        case "DTex" => new Fineness(value, DTex)
        case _: String => throw new IllegalArgumentException(s"$units are not currently supported.")
      }
    }

    def unapply(arg: Fineness): Option[(Double, FinenessUnits)] = if (arg == null) None else Some(arg.value, arg.units)

    def toMetricNumber(fineness: Fineness): Fineness = {
      fineness.units match {
        case MetricNumber => Fineness(fineness.value, MetricNumber)
        case Denier => Fineness(MetricNumber.weight / (fineness.value / Denier.length), MetricNumber)
        case Tex => Fineness(Tex.length / fineness.value, MetricNumber)
        case DTex => Fineness(DTex.length / fineness.value, MetricNumber)
        case _: FinenessUnits => throw new IllegalArgumentException(s"${fineness.units} are not currently supported.")
      }
    }

    def toDenier(fineness: Fineness): Fineness = {
      fineness.units match {
        case MetricNumber => Fineness((MetricNumber.weight / fineness.value) * (Denier.length / MetricNumber.length), Denier)
        case Denier => Fineness(fineness.value, Denier)
        case Tex => Fineness(fineness.value * (Denier.length / Tex.length), Denier)
        case DTex => Fineness(fineness.value * (Denier.length / DTex.length), Denier)
        case _: FinenessUnits => throw new IllegalArgumentException(s"${fineness.units} are not currently supported.")
      }
    }

    def toTex(fineness: Fineness): Fineness = {
      fineness.units match {
        case MetricNumber => Fineness((MetricNumber.weight / fineness.value) * (Tex.length / MetricNumber.length), Tex)
        case Denier => Fineness(fineness.value * (Tex.length / Denier.length), Tex)
        case Tex => Fineness(fineness.value, Tex)
        case DTex => Fineness(fineness.value * (Tex.length / DTex.length), Tex)
        case _: FinenessUnits => throw new IllegalArgumentException(s"${fineness.units} are not currently supported.")
      }
    }

    def toDTex(fineness: Fineness): Fineness = {
      fineness.units match {
        case MetricNumber => Fineness((MetricNumber.weight / fineness.value) * (DTex.length / MetricNumber.length), DTex)
        case Denier => Fineness(fineness.value * (DTex.length / Denier.length), DTex)
        case Tex => Fineness(fineness.value * (DTex.length / Tex.length), DTex)
        case DTex => Fineness(fineness.value, DTex)
        case _: FinenessUnits => throw new IllegalArgumentException(s"${fineness.units} are not currently supported.")
      }
    }
  }

}
