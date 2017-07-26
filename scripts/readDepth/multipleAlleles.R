#library(lattice)
source('../../../R/allFunctions.R')
source('../../../R/wlr.R')
#load('full.tables.rda')
load('point5.rda')
load('../../../data/full.tables.01.26.14.rda')


cts <- c(5,10,15,20,25,30,35,40,45,50)
rn <- paste('coverage > ', cts, sep='')
multipleAlleles <- vector(mode='list', length=length(cts))
for(i in 1:length(cts)){

  ##need to change to which ever dataset you load
    multipleAlleles[[i]] <- sapply(point5Tab, function(x)
                            {sum(threeAlleles(x, counts=cts[i], refCt=5)>1)})
    multAllelesP5 <- do.call(rbind, multipleAlleles)

}

rownames(multAllelesP5) <- rn

#save(multAllelesP5, file='multAllelesP5.rda', compress=TRUE)

snp1335.index <- (full.tables.01.26.14[[7]][,'snp'] == 1)
#snp1013.index <- (full.tables.01.26.14[[3]][,'snp'] == 1)
snp3367.index <- (full.tables.01.26.14[[6]][,'snp'] == 1)

non.intersect <- (snp1335.index & !snp3367.index)

nref.tab.1335 <- full.tables.01.26.14[[7]][non.intersect, c('a','g','c','t')]
#nref.tab.1013 <- full.tables.01.26.14[[3]][non.intersect, c('a','g','c','t')]
nref.tab.3367 <- full.tables.01.26.14[[6]][non.intersect, c('a','g','c','t')]

nref.nuc.new <- function(tables = tab, mask=T, thresh.count=0, thresh.rate=0.0){
	# get read count for max nonref nuc
	nref <- apply(tables[mask, c("a", "g", "c", "t")], 1, max)
	# where does nref count match a (g,c,t, resp) count
	as <- ifelse(nref == tables[mask,'a'],1,0)
	gs <- ifelse(nref == tables[mask,'g'],2,0)
	cs <- ifelse(nref == tables[mask,'c'],3,0)
	ts <- ifelse(nref == tables[mask,'t'],4,0)
	# most positions will show 3 zeros and one of 1:4, so max identifies max nonref count;
	# ties broken arbitrarily  (a<g<c<t)
	merge <- pmax(as,gs,cs,ts)
	# but if max nonref count is zero or below threshold, return 0
	merge[nref == 0 | nref < thresh.count] <- 0
	merge[nref/tables[mask,'Cov'] < thresh.rate] <- 0
	return(merge)
}

nt.1335 <- nref.nuc.new(nref.tab.1335)

counts2 <- vector(length=length(nt.1335))
for(i in 1:length(nt.1335)){
    counts2[i] <- nref.tab.3367[i, nt.1335[i]]
}

