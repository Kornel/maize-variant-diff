require(dplyr)
require(ggplot2)
require(scales)

plot.positions <- function(filename) {
  
  data <- read.csv("/tmp/ax/ax-frameshift_variant.csv",
                   sep=",",
                   header=FALSE, 
                   col.names=c("Chrom", "Pos", "Value"))
  number_ticks <- function(n) {function(limits) pretty(limits, n)}
  
  plots <- NULL
  chrom.data <- NULL
  
  for (i in 1:10) {
    chrom.data[[i]] <- filter(data, Chrom == i)
    
    p <- ggplot(chrom.data[[i]], aes(x=Pos, y=Value, color=Value)) + 
      geom_point(shape=1) + 
      geom_smooth(method="loess") +
      scale_x_continuous(breaks=number_ticks(7)) +
      ggtitle(paste(filename, "\nchromosome", i))
    
    ggsave(sprintf("plot%d.png", filename, i), p)
    
    plots[[i]] <- p
  }

}

files <- list.files("/tmp/bx")

for (f in files) {
  plot.positions(sprintf("/tmp/bx/%s", f)) 
}