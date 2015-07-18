package org.kornel.vcf

object ParseAnnotated extends App {

  import Parse._
  import Format._
  import EFFFormat._

  val inputPath = "/Users/kornel.kielczewski/maize/AGPv3.fasta.mp.bcf.MUT.filt.annotated.vcf"

  val file = scala.io.Source.fromFile(inputPath)

  val lines = skipHeader(file.getLines()).map(_.split("\t"))
    .filter(x => chromosome(x) == "10")

  val x = format _ andThen parseFields andThen extractEff andThen parseEff

  lines.map(x).toSeq.distinct.foreach(println)


}

object Parse {

  def skipHeader(lines: Iterator[String]) = lines.dropWhile(_.startsWith("#"))

  def chromosome(fields: Array[String]) = fields(0)

  def position(fields: Array[String]) = fields(1)

  def format(fields: Array[String]) = fields(7)

  def hds1(fields: Array[String]) = fields(8)

  def hds2(fields: Array[String]) = fields(9)
}

object Format {

  def parseFields(fields: String) = fields.split(";").map {
    x => val split = x.split("=")
      if (split.length > 1) split(0) -> split(1) else split(0) -> ""
  }.toMap


}

object EFFFormat {

  def extractEff(fields: Map[String, String]) = fields("EFF")

  def parseEff(field: String) = {
    val name = field.substring(0, field.indexOf("("))
    name
  }

}
