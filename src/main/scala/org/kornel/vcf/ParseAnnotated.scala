package org.kornel.vcf

import scala.util.Try

object ParseAnnotated extends App {

  import Parse._

  val inputPath = "/Users/kornel.kielczewski/maize/AGPv3.fasta.mp.bcf.MUT.filt.annotated.vcf"

  val file = scala.io.Source.fromFile(inputPath)

  val lines = skipHeader(file.getLines()).flatMap(line => VC(line.split("\t")))
    .filter(_.chromosome == 10)
    .flatMap(v => v.info().eff.map(e => v -> e))
    .filter(_._2.name == "frameshift_variant")
    .map {
    case (vc, eff) => vc.position -> (vc.hds1.value - vc.hds2.value)
  }
  //.map(v => (v._1.hds1, v._1.hds2) -> 1).toSeq.groupBy(_._1).mapValues(_.foldLeft(0)((a,b) => a + b._2))

  lines.foreach(println)

}

case class EFF(name: String, content: String, geneName: String)

case class Info(eff: Seq[EFF])

object Info {

  private def parseFields(fields: String) = fields.split(";").map {
    x => val split = x.split("=")
      if (split.length > 1) split(0) -> split(1) else split(0) -> ""
  }.toMap

  private def extractEff(fields: Map[String, String]) = fields("EFF")

  private def parseEffs(field: String): Seq[EFF] = {
    val effs = field.split(",")
    effs.map {
      effRaw =>
        val contentStart = effRaw.indexOf('(')
        val name = effRaw.substring(0, contentStart)
        val content = effRaw.substring(contentStart, effRaw.length - 1)
        EFF(name, content)
    }
  }

  def apply(raw: String): Info = {
    Info(parseEffs(extractEff(parseFields(raw))))
  }
}

sealed trait IsChange {
  val value: Int
}

case object ChangeA extends IsChange {
  val value = 1
}

case object ChangeB extends IsChange {
  val value = 1
}

case object ChangeBoth extends IsChange {
  val value = 1
}

case object NoChange extends IsChange {
  val value = 2
}

case class VC(chromosome: Int, position: Int, info: () => Info, hds1: IsChange, hds2: IsChange)

object VC {

  def extractChromosome(fields: Array[String]) = fields(0)

  def extractPosition(fields: Array[String]) = fields(1)

  def extractFormat(fields: Array[String]) = fields(7)

  def extractHds(rawHds: String) = rawHds.substring(0, 3) match {
    case "0/0" => NoChange
    case "1/0" => ChangeA
    case "0/1" => ChangeB
    case "1/1" => ChangeBoth
    case x => throw new IllegalStateException(s"Unexpected hds $x")
  }

  def extractHds1(fields: Array[String]) = extractHds(fields(9))

  def extractHds2(fields: Array[String]) = extractHds(fields(10))

  def apply(tokens: Array[String]): Option[VC] = {
    Try(extractChromosome(tokens).toInt).toOption.map {
      chromosome =>
        val position = extractPosition(tokens).toInt
        lazy val _info = Info(extractFormat(tokens))
        val info = () => _info
        val hds1 = extractHds1(tokens)
        val hds2 = extractHds2(tokens)
        VC(chromosome, position, info, hds1, hds2)
    }
  }

}

object Parse {

  def skipHeader(lines: Iterator[String]) = lines.dropWhile(_.startsWith("#"))

}

