require(dplyr)
require(ggplot2)
require(scales)
require(tools)

PlotPositions <- function(filename, overwrite=FALSE, doNotSave=FALSE) {
  
  print(sprintf("Working on %s", filename))
  
  data <- read.csv(filename,
                   sep=",",
                   header=FALSE, 
                   col.names=c("Chrom", "Pos", "Value"))
  number_ticks <- function(n) {function(limits) pretty(limits, n)}
  
  plots <- NULL
  chrom.data <- NULL
  
  yscale = max(abs(min(data$Value)), max(data$Value))
  
  for (i in 1:10) {
    chrom.data[[i]] <- filter(data, Chrom == i)
    
    variant.type <- file_path_sans_ext(basename(filename))
    
    output.filename <- sprintf("plots/%s/plot%.2d.png", variant.type, i)
    
    if (!overwrite && file.exists(output.filename) && !doNotSave) {
      print(sprintf("File %s already exists, skipping...", output.filename))
      next
    }
    
    title <- paste(variant.type, "\n", "Chromosome", i)
    
    tryCatch( {
      
      p <- ggplot(chrom.data[[i]], aes(x=Pos, y=Value, color=Value)) + 
        geom_point(shape=1) + 
        geom_smooth(method="loess") +
        geom_abline(slope = 0, intercept = 0, colour = 'red', alpha = 0.3) + 
        ylab("hds1 - hds2") + 
        ylim(-yscale, yscale) + 
        xlab("Position") +
        scale_x_continuous(breaks=number_ticks(7)) +
        ggtitle(title)

      
      if (!doNotSave) {
        dir.create(sprintf("plots/%s/", variant.type))
        print(sprintf("Creating plot %s", output.filename))
        ggsave(output.filename, p)
      }
      
      plots[[i]] <- p
    }, error = function(e) {
      print(e$message)
    })
  }
  
  return(plots)

}

files <- list.files("variants")

for (f in files) {
  PlotPositions(sprintf("variants/%s", f), overwrite=TRUE) 
}



data <- read.csv('variants/frameshift_variant.csv',
                 sep=",",
                 header=FALSE, 
                 col.names=c("Chrom", "Pos", "Value"))

chrom1 <- filter(data, Chrom == 1)




# PlotPositions("variants/frameshift_variant.csv", doNotSave=TRUE)