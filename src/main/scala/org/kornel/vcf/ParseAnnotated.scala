package org.kornel.vcf

import java.io.FileWriter

object ParseAnnotated extends App {

  import Parse._

  val inputPath = "/Users/kornel.kielczewski/maize/AGPv3.fasta.mp.bcf.MUT.filt.annotated.vcf"

  val file = scala.io.Source.fromFile(inputPath)

  val lines = skipHeader(file.getLines())
    .flatMap(VC.apply)
    .flatMap(v => v.info().eff.map(e => v -> e))
    .filter(_._2.name == "frameshift_variant")
    .flatMap {
    case (vc, eff) =>
      if (vc.hds1.value - vc.hds2.value == 0) None else Some((vc.chromosome, vc.position, vc.hds1.value - vc.hds2.value))
  }

  val all = lines.toList
    .groupBy(x => (x._1, x._2))
    .mapValues(_.map(_._3).sum)
    .toList
    .sortBy {
    case ((chrom, pos), _) => (chrom, pos)
  }
    .map {
    case ((chrom, pos), value) => s"$chrom;$pos;$value\n"
  }

  val fw = new FileWriter("frameshfit_variant-positions.csv", false)

  all.foreach(fw.write)

  fw.close()

}

