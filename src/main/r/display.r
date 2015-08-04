data <- read.csv("frameshift_variant-positions.csv",
                 sep=";", 
                 header=FALSE, 
                 col.names=c("Chrom", "Pos", "Value"))

require(dplyr)
require(ggplot2)
require(scales)

source("multiplot.r")

plots <- NULL
chrom.data <- NULL

for (i in 1:10) {
  chrom.data[[i]] <- filter(data, Chrom == i)
  
  p <- ggplot(chrom.data[[i]], aes(x=Pos, y=Value, color=Value)) + 
    geom_point(shape=1) + 
    scale_x_continuous(trans=log2_trans()) +
    ggtitle(paste("chromosome", i))
  
  ggsave(sprintf("plot%d.png", i), p)
  
  plots[[i]] <- p
}

#multiplot(plotlist=plots[seq(1,3)])
#multiplot(plotlist=plots[seq(4,6)])
#multiplot(plotlist=plots[seq(7,10)])

