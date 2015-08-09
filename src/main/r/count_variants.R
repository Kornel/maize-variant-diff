filename <- '/Users/kornel.kielczewski/maize/geny_przemek (1).csv'
file <- read.csv(filename)

require(dplyr)

x <- filter(file, Gene == 'GRMZM2G003038_T03')

x$frameshift_variant_hds2