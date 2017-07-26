##Right now this is just a bunch of random code to do a bunch of random things...


library(lattice)
source('../../../R/allFunctions.R')
#tab = c('tp1007.tab','tp1012.tab','tp1013.tab','tp1014.tab','tp1015.tab','tp3369.tab','tp1335.tab')
load('ftdp.rda')
load('gof.results.rda')
load('full.tables.rda')
#x <- read.table(tab[1])
#ntests <- list(7)
#exmin <- list(7) # min of "expected" bins for that test
#coverage <- list(7) # how many reads cover position for a given test
#for(i in 1:7) {
#  ntests[[i]] <- length(results[[i]])
#  coverage[[i]]  <- as.numeric(names(results[[i]]))
#  exmin[[i]]     <- vector('numeric', ntests[[i]])
#  for(j in 1:ntests[[i]]){
#    exmin[[i]][j]     <- min(results[[i]][[j]]$expected)
#  }
#}
                                        #subT = TRUE
desert.begin <- 1376223
desert.end <- 1696217
desert.length <- 319995
nondesert.length <- 100000
desert.mask <- rep(c(T,F),c(desert.length, nondesert.length))

#subT = TRUE
#begin = 1376223
#end  = 1696217

load('ftdp.rda')

for(i in 1:length(full.tables.des.plus[1])){
  #x = read.table(tab[i], header=TRUE)
  #if(subT){
  #  x = x[begin:end,]
  #}

  x <- full.tables.des.plus[[i]][1:319995,]

  nonref = apply(x[c('a','g','c','t')], 1, sum)
  nonref.y <- apply(y[c('a','g','c','t')], 1, sum)

  total = nonref + x['.match'] + 1

  coverage = x[,'Cov']
  coverage.y = nonref.y+y[, '.match']

  index0 = nonref>0
  index0.y = nonref.y>0

  strain = names(full.tables.des.plus[i])

  m = mean(coverage)
  ind = which(coverage > 40  & coverage < 175 & nonref > 1)

  ind = (coverage > err.bounds[[i]][1]  & coverage < err.bounds[[i]][2] & nonref > 1)
  ind.y = (coverage.y > err.bounds[[i]][1]  & coverage.y < err.bounds[[i]][2] & nonref.y > 1)

  ratio = nonref[ind]/coverage[ind]
                                        #w = nonref[ind]/coverage[ind]
  ratio.y = nonref.y[ind.y]/coverage.y[ind.y]

  if(run.figure[1]){
    scatter = paste(strain,'-scatter',".pdf", sep='')
    pdf(scatter)
    print(xyplot(ratio~coverage[ind], pch='+',
                 panel=panel.smoothScatter, xlab='Log2 of Coverage+1'))
    dev.off()

    scatter.y = paste(strain,'-scatter-non-des',".pdf", sep='')
    pdf(scatter.y)
    print(xyplot(ratio.y~coverage.y[ind.y], pch='+',
                 panel=panel.smoothScatter))
    dev.off()}

  if(run.figure[2]){
    index = (nonref == 0)
    matchCov = x[index, '.match']
    desHist = paste(strain, '-desert-hist', '.pdf', sep='')
    pdf(desHist)
    hist(log(matchCov+1, 2))
    dev.off()

    y2 = paste(strain, '-ratio-hist', '.pdf', sep='')

    pdf(y2)
    hist(z)
    dev.off()
  }

}

save(minor.alleles, file='minor.alleles.rda', compress=TRUE)

x = minor.alleles[[2]]

apply(x[x[,'Ref2']==nt[1],c('a','g','c','t')], 1, which.max)

mis.match.err <- list(7)
nt <- c("A","G","C","T")
for(i in 1:7){
  x <- minor.alleles[[i]]
  info <- list(4)
  for(j in 1:4){
    maxPos <- cbind(apply(x[x[,'Ref2']==nt[j],c('a','g','c','t')], 1, which.max),
                    apply(x[x[,'Ref2']==nt[j],c('a','g','c','t')], 1, max))
    info[[j]] <- maxPos
  }
  names(info) <- nt
  mis.match.err[[i]] <- info
}


names(mis.match.err) <- names(minor.alleles)


##look for regions of the than 0.5 for non-ref/coverage

x <- full.tables[[1]]

nonref <- apply(x[c('a','g','c','t')], 1, sum)
cov <- x[,"Cov"]

ratio <- nonref/cov
point5 <- (ratio>0.5)
point5Tab <- x[point5,]


point5Tab <- lapply(full.tables, function(x){nonref0 <- apply(x[c('a','g','c','t')], 1, sum);
                                             cov0 <- x[,"Cov"];
                                             cov <- cov0[cov0>0];
                                             nonref <- nonref0[cov0>0]
                                             ratio <- nonref/cov;
                                             point5 <- x[which((ratio>0.5)),]
                                             print(head(point5))
                                             return(point5)
                                           })

sapply(point5Tab, function(x){print(sum(is.na(x[,"Pos"])))})


point5Tab <- lapply(full.tables, function(x){nonref0 <- apply(x[c('a','g','c','t')], 1, sum);
                                             cov0 <- x[,"Cov"];
                                             cov <- cov0[cov0>0];
                                             nonref <- nonref0[cov0>0]
                                             ratio <- nonref/cov;
                                             point5 <- x[which((ratio>0.5)),]
                                             print(head(point5))
                                             return(point5)
                                           })

mmRatio <- function(annTable, frac=0.5){
  nonref0 <- apply(annTable[,c('a','g','c','t')], 1, sum);
  cov0 <- annTable[,"Cov"];
  cov <- cov0[cov0>0];
  nonref <- nonref0[cov0>0]
  ratio <- nonref/cov;
  subTab <- annTable[cov0>0,]
  point5 <- subTab[ratio>frac,]
  #print(head(point5))
  return(point5)
}

point8 <- lapply(full.tables[c(3,6)], function(x){mmRatio(x, 0.75)})


#save(point5Tab, file='point5.rda', compress=TRUE)
test <- list(tp = full.tables[[1]][1:3000000,])
point95.Tab <- lapply(full.tables, function(x){nonref0 <- apply(x[,c('a','g','c','t')], 1, sum);
                                             cov0 <- x[,"Cov"];
                                             cov <- cov0[cov0>0];
                                             nonref <- nonref0[cov0>0];
                                             ratio <- nonref/cov;
                                             subTab <- x[cov0>0,]
                                             point95 <- subTab[ratio>0.95,]
                                             print(head(point95))
                                             return(point95)
                                           })
save(point95.Tab, file='point95.Tab.rda', compress=TRUE)


twoAlleles <- lapply(point95.Tab.g5, function(x){rowSums(apply(x[,8:11], 2, function(y) y>5)) > 1})

point95.Tab.g5 <- lapply(point95.Tab, function(x){rs <- rowSums(x[,8:12]);
                                                return(x[rs>5,])})

point8.g5 <- lapply(point8, function(x){rs <- rowSums(x[,8:12]);
                                                return(x[rs>5,])})


twoAlleles <- lapply(point95.Tab.g5, function(x){rowSums(apply(x[,8:11], 2, function(y) y>5)) > 1})



a2gORc2t <- function(dataTable, a2g=TRUE){

  if(a2g){
    index1 <- (dataTable[,"Ref2"] == "A")
    index2 <- (dataTable[,"Ref2"] == "G")
  }
  else{
    index1 <- (dataTable[,"Ref2"] == "C")
    index2 <- (dataTable[,"Ref2"] == "T")
  }

  a2gCount <- dataTab[c(index1),]



}



test <- lapply(full.tables, function(x){ind <- x[,'.match']<2;
                                        nz <- x[,'Cov'] > 0;
                                        notN <- x[,'Ref2'] != "N";
                                        ratio <- rowSums(x[ind&nz&notN,8:11])/x[ind&nz&notN,'Cov'];
                                        ratio})

ind <- lapply(test, function(x) {as.numeric(names(x[x>0.9]))})

ones <- mapply(function(x,y){z = x[y,'Cov'];
                           print(table(z));
                           z}, full.tables, ind)



##Getting the indels into R
load('full.tables.rda')
wd <- getwd()
setwd('/share/projects/Thaps-strain-genomes/polymorphism/vcf_20111027_noBAQ_qualfilt/')
#path = '/share/projects/Thaps-strain-genomes/polymorphism/vcf_20111027_noBAQ_qualfilt/'
indel.files <- dir()[grep('*indels*',dir())]
indel.files <- indel.files[c(2:6,1,7)]
indel.tab <- lapply(indel.files, read.table)
setwd(wd)

indel.pos <- lapply(indel.tab, function(x) {return(x[,2])})

full.tables.indel <- mapply(function(x,y){x[y,]}, full.tables, indel.pos)

##Analyzing the 18S
load('tp.18s.rda')

cls <- colors()[c(23, 134, 169, 47, 400, 255, 383)]



plot(rep(0,nrow(tp.18s[[1]])), col='white', ylim=c(0,45))

for(i in 1:7){
  rs <- rowSums(tp.18s[[i]][,8:11])
  print(sum(rs>10))
  print('======================')
  #print(c(max(rs), which(rs==max(rs))))
  #print(which(rs==max(rs)))
  ind <- rs > 5
  #print(max(which(ind)))
  for(nt in c('a','g','c','t')){
    points(tp.18s[[i]][, nt], col=cls[i])
  }
}

rs <- lapply(tp.18s, function(x){rowSums(x[,8:11])})
rs10 <- lapply(rs, function(x){x>10})
lapply(rs5, which)


