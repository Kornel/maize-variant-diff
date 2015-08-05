require(dplyr)
require(ggplot2)
require(scales)
require(tools)

plot.positions <- function(filename) {
  
  print(sprintf("Working on %s", filename))
  
  data <- read.csv(filename,
                   sep=",",
                   header=FALSE, 
                   col.names=c("Chrom", "Pos", "Value"))
  number_ticks <- function(n) {function(limits) pretty(limits, n)}
  
  plots <- NULL
  chrom.data <- NULL
  
  for (i in 1:10) {
    chrom.data[[i]] <- filter(data, Chrom == i)
    
    variant.type <- file_path_sans_ext(basename(filename))
    
    output.filename <- sprintf("plots/%s/plot%.2d.png", variant.type, i)
    
    if (file.exists(output.filename)) {
      print(sprintf("File %s already exists, skipping...", output.filename))
      next
    }
    
    title <- paste(variant.type, "\n", "Chromosome", i)
    
    tryCatch( {
      p <- ggplot(chrom.data[[i]], aes(x=Pos, y=Value, color=Value)) + 
        geom_point(shape=1) + 
        geom_smooth(method="loess") +
        ylab("hds1 - hds2") + 
        xlab("Position") +
        scale_x_continuous(breaks=number_ticks(7)) +
        ggtitle(title)
      
      dir.create(sprintf("plots/%s/", variant.type))
      ggsave(output.filename, p)
      
      plots[[i]] <- p
    }, error = function(e) {
      print(e$message)
    })
  }

}

files <- list.files("variants")

for (f in files) {
  plot.positions(sprintf("variants/%s", f)) 
}