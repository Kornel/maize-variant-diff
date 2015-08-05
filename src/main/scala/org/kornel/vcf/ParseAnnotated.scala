package org.kornel.vcf

import java.io.FileWriter

object ParseAnnotated extends App {

  import Parse._

  val files = Types().map {
    v => v -> new FileWriter(s"/tmp/ax-$v.csv", false)
  }.toMap

  val inputPath = "/Users/kornel.kielczewski/maize/AGPv3.fasta.mp.bcf.MUT.filt.annotated.vcf"

  val file = scala.io.Source.fromFile(inputPath)

  val lines = skipHeader(file.getLines())
    .flatMap(VC.fromLine)
    .flatMap(v => v.info().eff.map(e => v -> e))
    .map {
    case (vc, eff) => (vc.chromosome, vc.position, eff.name, vc.hds1.value - vc.hds2.value)
  }

  var prev: Option[(Int, Int, String)] = None
  var sum = 0
  var currLine = 0

  while (lines.hasNext) {

    currLine = currLine + 1

    val (chrom, pos, name, value) = lines.next()
    val curr = (chrom, pos, name)

    if (!lines.hasNext) {
      prev match {
        case Some(x) if x == curr =>
          sum += value
          files(name).write(s"$chrom,$pos,$sum\n")
        case Some((c, p, n)) =>
          files(n).write(s"$c,$p,$sum\n")
          files(name).write(s"$chrom,$pos,$value\n")
      }
    } else {
      prev match {
        case Some(x) if x == curr =>
          sum += value
        case Some(x) if x != curr =>
          files(x._3).write(s"${x._1},${x._2},$sum\n")
          sum = value
        case None =>
          sum = value
      }
    }

    prev = Some(chrom, pos, name)

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