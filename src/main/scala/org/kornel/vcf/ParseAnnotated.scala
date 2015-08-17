package org.kornel.vcf

import java.io.FileWriter
import scalikejdbc._

object ParseAnnotated extends App {

  import Parse._

  val files = Types().map {
    v => v -> new FileWriter(s"/tmp/xx/$v.csv", false)
  }.toMap

  files.foreach(_._2.write("chromosome,gene.ID,hds1,hds2\n"))

  val inputPath = "/Users/kornel.kielczewski/maize/AGPv3.fasta.mp.bcf.MUT.filt.annotated.vcf"

  val file = scala.io.Source.fromFile(inputPath)

  val lines = skipHeader(file.getLines())
    .flatMap(VC.fromLine)
    .flatMap(v => v.info().eff.map(e => v -> e))
    .filter(_._2.name == "frameshift_variant")
    .map {
    case (vc, eff) => (vc.chromosome, vc.position, eff.name, vc.hds1.value, vc.hds2.value)
  }

  var prev: Option[(String, String)] = None
  var sum1 = 0
  var sum2 = 0
  var currLine = 0

  while (lines.hasNext) {

    currLine = currLine + 1

    val (chrom, pos, variantName, value1, value2) = lines.next()

    val geneName = s"$chrom,${FindGeneName(chrom, pos)}"

    val curr = (geneName, variantName)

    if (!lines.hasNext) {
      prev match {
        case Some(x) if x == curr =>
          sum1 += value1
          sum2 += value2
          files(variantName).write(s"$geneName,$sum1,$sum2\n")
        case Some((g, n)) =>
          files(n).write(s"$g,$sum1,$sum2\n")
          files(variantName).write(s"$geneName,$value1,$value2\n")
      }
    } else {
      prev match {
        case Some(x) if x == curr =>
          sum1 += value1
          sum2 += value2
        case Some(x) if x != curr =>
          files(x._2).write(s"${x._1},$sum1,$sum2\n")
          sum1 = value1
          sum2 = value2
        case None =>
          sum1 = value1
          sum2 = value2
      }
    }

    prev = Some(geneName, variantName)

  }

  files.foreach(_._2.close())

}

object Types {

  def apply() =
    Seq("3_prime_UTR_truncation+exon_loss",
      "3_prime_UTR_variant",
      "5_prime_UTR_premature_start_codon_gain_variant",
      "5_prime_UTR_truncation+exon_loss_variant",
      "5_prime_UTR_variant",
      "disruptive_inframe_deletion",
      "disruptive_inframe_insertion",
      "downstream_gene_variant",
      "frameshift_variant",
      "frameshift_variant+start_lost",
      "frameshift_variant+stop_gained",
      "frameshift_variant+stop_lost",
      "inframe_deletion",
      "inframe_insertion",
      "initiator_codon_variant",
      "intergenic_region",
      "intragenic_variant",
      "intron_variant",
      "missense_variant",
      "non_coding_exon_variant",
      "splice_acceptor_variant",
      "splice_donor_variant",
      "splice_region_variant",
      "start_lost",
      "start_lost+disruptive_inframe_insertion",
      "start_lost+inframe_deletion",
      "start_lost+inframe_insertion",
      "stop_gained",
      "stop_gained+disruptive_inframe_deletion",
      "stop_gained+disruptive_inframe_insertion",
      "stop_gained+inframe_insertion",
      "stop_lost",
      "stop_lost+disruptive_inframe_deletion",
      "stop_lost+inframe_deletion",
      "stop_lost+inframe_insertion",
      "stop_retained_variant",
      "synonymous_variant",
      "upstream_gene_variant")
}

object FindGeneName {

  Class.forName("com.mysql.jdbc.Driver")

  ConnectionPool.singleton("jdbc:mysql://localhost/maize", "", "")

  GlobalSettings.loggingSQLAndTime = LoggingSQLAndTimeSettings(enabled = false)

  def apply(chrom: Int, pos: Int): String = DB readOnly { implicit session =>
    val names = sql"select gene_c from indices_v3 where what = 'gene' and start <= $pos and stop >= $pos and chromosome = $chrom".map(_.string(1)).list().apply()
    if (names.length != 1) {
      println(s"Failed to fetch name for position $pos and chromosome $chrom - names = $names")
      if (names.length == 0) {
        "Unknown"
      } else {
        names.head
      }
    } else {
      names.head
    }
  }

}