package cn.edu.gxust.jiweihuang.scala.textile

import cn.edu.gxust.jiweihuang.scala.test.UnitSpec

class TextilePackageTest extends UnitSpec {
  "MetricNumber, Denier, DTex and Tex" should "be correctly println" in {
    println(s"MetricNumber = $MetricNumber")
    println(s"Denier = $Denier")
    println(s"DTex = $DTex")
    println(s"Tex = $Tex")
  }

  "FinenessUnits" should "be correctly println." in {
    println(s"FinenessUnits.METRIC_NUMBER = ${FinenessUnits.METRIC_NUMBER}")
    println(s"FinenessUnits.DENIER = ${FinenessUnits.DENIER}")
    println(s"FinenessUnits.DTEX = ${FinenessUnits.DTEX}")
    println(s"FinenessUnits.TEX = ${FinenessUnits.TEX}")
  }

  "The constants of FinenessUnits" should "be equals to MetricNumber, Denier, DTex and Tex" in {
    Denier === FinenessUnits.DENIER
    MetricNumber === FinenessUnits.METRIC_NUMBER
    DTex === FinenessUnits.DTEX
    Tex === FinenessUnits.TEX
  }

  "Fineness.to*** method" should "be equals to " in {
    println(Fineness(1, Denier) + "=" + Fineness.toDenier(Fineness(1, Denier)))
    println(Fineness(1, MetricNumber) + "=" + Fineness.toDenier(Fineness(1, MetricNumber)))
    println(Fineness(1, DTex) + "=" + Fineness.toDenier(Fineness(1, DTex)))
    println(Fineness(1, Tex) + "=" + Fineness.toDenier(Fineness(1, Tex)))

    println(Fineness(1, Denier) + "=" + Fineness.toMetricNumber(Fineness(1, Denier)))
    println(Fineness(1, MetricNumber) + "=" + Fineness.toMetricNumber(Fineness(1, MetricNumber)))
    println(Fineness(1, DTex) + "=" + Fineness.toMetricNumber(Fineness(1, DTex)))
    println(Fineness(1, Tex) + "=" + Fineness.toMetricNumber(Fineness(1, Tex)))

    println(Fineness(1, Denier) + "=" + Fineness.toDTex(Fineness(1, Denier)))
    println(Fineness(1, MetricNumber) + "=" + Fineness.toDTex(Fineness(1, MetricNumber)))
    println(Fineness(1, DTex) + "=" + Fineness.toDTex(Fineness(1, DTex)))
    println(Fineness(1, Tex) + "=" + Fineness.toDTex(Fineness(1, Tex)))

    println(Fineness(1, Denier) + "=" + Fineness.toTex(Fineness(1, Denier)))
    println(Fineness(1, MetricNumber) + "=" + Fineness.toTex(Fineness(1, MetricNumber)))
    println(Fineness(1, DTex) + "=" + Fineness.toTex(Fineness(1, DTex)))
    println(Fineness(1, Tex) + "=" + Fineness.toTex(Fineness(1, Tex)))
  }
}
