package org.kornel.vcf

import scala.util.Try

object ParseAnnotated extends App {

  import Parse._

  val inputPath = "/Users/kornel.kielczewski/maize/AGPv3.fasta.mp.bcf.MUT.filt.annotated.vcf"

  val file = scala.io.Source.fromFile(inputPath)

  val lines = skipHeader(file.getLines()).flatMap(line => VC(line.split("\t")))
    .filter(_.chromosome == 10)
    .flatMap(v => v.format().eff.map(e => v.position -> e))
    .filter(_._2.name == "frameshift_variant")
    .map(_._1)
    .foreach(println)

}

case class EFF(name: String, content: String)

case class Format(eff: Seq[EFF])

object Format {

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

  def apply(raw: String): Format = {
    Format(parseEffs(extractEff(parseFields(raw))))
  }
}

case class VC(chromosome: Int, position: Int, format: () => Format)

object VC {

  def extractChromosome(fields: Array[String]) = fields(0)

  def extractPosition(fields: Array[String]) = fields(1)

  def extractFormat(fields: Array[String]) = fields(7)

//  def extractHds1(fields: Array[String]) = fields(8)
//
//  def extractHds2(fields: Array[String]) = fields(9)

  def apply(tokens: Array[String]): Option[VC] = {
    Try(extractChromosome(tokens).toInt).toOption.map {
      chromosome =>
        val position = extractPosition(tokens).toInt
        lazy val _format = Format(extractFormat(tokens))
        val format = () => _format
        VC(chromosome, position, format)
    }
  }

}

object Parse {

  def skipHeader(lines: Iterator[String]) = lines.dropWhile(_.startsWith("#"))

}

