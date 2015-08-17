library(dplyr)
library(ggplot2)
library(reshape2)
library(mvtnorm)

expr.path <- '/Users/kornel.kielczewski/maize/dane ekspresji z numerami GRMZM.csv'
expr <- read.csv(expr.path, sep = ';', dec = ',', stringsAsFactors=FALSE)[ ,c('gene.ID', 'hds1.c.k', 'hds2.c.k', 'hds1.p_c.k', 'hds2.p_c.k')]
# expr$gene <- sprintf('GRMZM%s', substring(expr$probe.ID, 3))

# Non GRMZM's
# count(expr, grepl('GRMZM', gene.ID))

hds.diff.path <- '/Users/kornel.kielczewski/maize/hds_diff_frameshift_variant.csv'
hds.diff <- read.csv(hds.diff.path, stringsAsFactors=FALSE)

join <- inner_join(expr, hds.diff, by = "gene.ID")

ggplot(join, aes(x=hds1.c.k, y=hds1, color=hds1)) + 
  geom_point(shape=1) + 
  geom_abline(slope = 0, intercept = 0, colour = 'red', alpha = 0.5)  + 
  geom_vline(xintercept = 0, colour = 'red', alpha = 0.5) +
  ylim(-10, 10) + 
  xlim(-3, 3)

ggplot(join, aes(x = hds1.c.k)) + geom_density() + geom_density(data=data.frame(rnorm(dim(join)[1])))

m.c1 = mean(join$hds1.c.k)
sigma.c1 = sd(join$hds1.c.k)

m.1 = mean(join$hds1)
sigma.1 = sd(join$hds1)

ggplot(join, aes(x=hds2.c.k, y=hds2, color=hds2)) + 
  geom_point(shape=1) + 
  geom_abline(slope = 0, intercept = 0, colour = 'red', alpha = 0.5)  + 
  geom_vline(xintercept = 0, colour = 'red', alpha = 0.5) +
  ylim(-10, 10) + 
  xlim(-3, 3)

ggplot(join, aes(x=(hds1.c.k - hds2.c.k), y=(hds1 - hds2), color=(hds1 - hds2))) + 
  geom_point(shape=1) + 
  geom_abline(slope = 0, intercept = 0, colour = 'red', alpha = 0.5)  + 
  geom_vline(xintercept = 0, colour = 'red', alpha = 0.5)  +
  ylim(-5, 5) + 
  xlim(-3, 3)

df <- data.frame(gene.ID = join$gene.ID, expr.diff = join$hds1.c.k - join$hds2.c.k, snp.diff = join$hds1 - join$hds2)

n <- length(df$expr.diff)[1]
expr.density <- data.frame(expr = df$expr.diff, estimated=rnorm(n, mean(df$expr.diff), sd(df$expr.diff)))
ggplot(melt(expr.density), aes(x=value, fill=variable)) + geom_density(alpha = .5)

snp.density <- data.frame(expr = df$snp.diff, estimated=rnorm(n, mean(df$snp.diff), sd(df$snp.diff)))
ggplot(melt(snp.density), aes(x=value, fill=variable)) + geom_density(alpha = .5)

#x <- 
mean <- c(mean(df$expr.diff), mean(df$snp.diff))
sigma <- matrix(c(sd(df$expr.diff),0,0,sd(df$snp.diff)), 2)

df$prob <- mapply(function(e,s) {
      x <- matrix(c(e, s), nrow=1, ncol=2)
      dmvnorm(x, mean, sigma)
    }, df$expr.diff, df$snp.diff)

ggplot(df, aes(x=expr.diff, y=snp.diff, color=prob)) + 
  geom_point(shape=1) + 
  geom_abline(slope = 0, intercept = 0, colour = 'red', alpha = 0.5)  + 
  geom_vline(xintercept = 0, colour = 'red', alpha = 0.5)  +
  ylim(-5, 5) + 
  xlim(-3, 3)

head(df[with(df, order(prob)), ])

