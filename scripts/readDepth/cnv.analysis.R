#library(lattice)
source('../../../R/allFunctions.R')
source('../../../R/wlr.R')
#load('full.tables.rda')
load('../../../data/full.tables.01.26.14.rda')
load('../../../data/tp.18s.rda')

tot.cov <- sapply(full.tables.01.26.14, function(x) sum(x[,'Cov']))
tot.cov[6] <- 1472320242
tot.cov[7] <- 700755596
mn.cov.18s <- sapply(tp.18s, function(x) mean(x[,'Cov']))
mn.cov.18s[6] <- 404.1999
mn.cov.18s[7] <- 146.9315
size.18s <- nrow(tp.18s[[1]])

#p <- (mn.cov.18s[7] * size.18s / tot.cov[7])
p <- 0.0003763681 ## calc for only mp data

x <- sapply(mn.cov.18s, function(x) {round(x*size.18s/25)})
n = round(tot.cov/25)

bt <- mapply(function(w,z){binom.test(w,z,p)}, x,n, SIMPLIFY=FALSE)

#save(bt, file='bt.rda', compress=TRUE)

##Density plots

if(F){

  pdf(file='tp.18s.den.pdf')
  plot(density(tp.18s[[4]][,'Cov']), col='green', pch='.', xlim=c(0,1800), main = '18S Ribosomal RNA Coverage',
       xlab = '18S Coordinates | N = 1795 | Bandwidth = 11.04', lwd = 3)
  lines(density(tp.18s[[1]][,'Cov']), col='blue', pch='.', lwd=3)
  lines(density(tp.18s[[2]][,'Cov']), col='red', pch='.', lwd=3)
  lines(density(tp.18s[[5]][,'Cov']), col='purple', pch='.', lwd=3)
  lines(density(tp.18s[[7]][,'Cov']), col='orange', pch='.', lwd=3)
  dev.off()
  
}


g.len <- nrow(full.tables.01.26.14[[1]])
g.end <- 31474424
genome.size <- 1:g.end



chunks <- split(genome.size, ceiling(seq_along(genome.size)/1795))
cov.analysis <- vector(length=7, mode='list')
for(i in 1:7){
  cov.analysis[[i]] <- vector(length=length(chunks))
  for(j in 1:length(chunks)){
    cov.analysis[[i]][j] <- sum(full.tables.01.26.14[[i]][chunks[[j]],'Cov'])
  }

}
