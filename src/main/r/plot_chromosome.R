
load_data <- function() {
  library(dplyr)
  library(ggplot2)
  library(reshape2)
  library(mvtnorm)
  
  expr.path <- '/Users/kornel.kielczewski/maize/dane ekspresji z numerami GRMZM.csv'
  expr <- read.csv(expr.path, sep = ';', dec = ',', stringsAsFactors=FALSE)[ ,c('gene.ID', 'hds1.c.k', 'hds2.c.k', 'hds1.p_c.k', 'hds2.p_c.k')]
  
  hds.diff.path <- '/Users/kornel.kielczewski/maize/hds_diff_frameshift_variant.csv'
  hds.diff <- read.csv(hds.diff.path, stringsAsFactors=FALSE)
  
  join <- inner_join(expr, hds.diff, by = "gene.ID")
  
  df <- data.frame(chromosome = join$chromosome,
                   gene.ID = join$gene.ID, 
                   expr.diff = join$hds1.c.k - join$hds2.c.k, 
                   snp.diff = join$hds1 - join$hds2)
  
  return(df)
}

filter_and_plot <- function(df, chromNum) {
  filtered <- filter(df, chromosome == chromNum)
  with.prob <- add_prob(filtered)
  title <- sprintf("Chromosome %d", chromNum)
  return(plot_chromosome(with.prob, title))
}

plot_all <- function(df) {
  df_ <- add_prob(df)
  return(plot_chromosome(df_, 'All chromosomes'))
}

add_prob <- function(df) {
  mean <- c(mean(df$expr.diff), mean(df$snp.diff))
  sigma <- matrix(c(sd(df$expr.diff), 0, 0, sd(df$snp.diff)), 2)
  
  df$prob <- mapply(function(e,s) {
    x <- matrix(c(e, s), nrow = 1, ncol = 2)
    dmvnorm(x, mean, sigma)
  }, df$expr.diff, df$snp.diff)
  
  return(df)
}

number_ticks <- function(n) {
  function(limits) pretty(limits, n)
}

plot_chromosome <- function(df, title) {
  
  p <- ggplot(df, aes(x=expr.diff, y=snp.diff, color=prob)) + 
    geom_point(shape=1) + 
    geom_abline(slope = 0, intercept = 0, colour = 'red', alpha = 0.5)  + 
    geom_vline(xintercept = 0, colour = 'red', alpha = 0.5)  +
    scale_x_continuous(name = "expr 1 - expr 2", limits = c(-5, 5), breaks = number_ticks(10)) +
    scale_y_continuous(name = "variants 1 - variants 2", limits = c(-5, 5), breaks = number_ticks(10)) +
    ggtitle(title)
  
  return(list(plot=p, summary=least_prob(df)))
}

least_prob <- function(df) {
  n <- 10
  top <- head(df[with(df, order(prob)), ], n)[, c('chromosome', 'gene.ID', 'expr.diff', 'snp.diff')]
  names(top) <- c('Chromosome', 'Gene name', 'expr 1 - expr 2', 'variants 1 - variants 2')
  return(top)
}

