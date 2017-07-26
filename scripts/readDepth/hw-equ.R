#library(lattice)
source('../../../R/allFunctions.R')
load('../../../data/full.tables.rda')
#load('full.tables.rda')


##HW - test
if(F){


    tp1007 <- full.tables[[1]]

    ##delete positions w/o coverage

    no.cov <- tp1007[,7] == 0

    tp1007 <- tp1007[!no.cov, ]

    alt.alleles <- rowSums(tp1007[,8:11])
    alt.ratio <- alt.alleles/tp1007[,7]

    f2s <- (alt.ratio>=0.5 & alt.ratio<0.6)
    s2s <- (alt.ratio>=0.6 & alt.ratio<0.7)
    s2e <- (alt.ratio>=0.7 & alt.ratio<0.8)
    e2n <- (alt.ratio>=0.8 & alt.ratio<0.95)

    dom.allele <- list(fifty2sixty=f2s, sixty2seventy=s2s,
                       seventy2eighty=s2e, eighty2ninety=e2n)

    dom.allele.cts <- sapply(dom.allele, sum)

    q <- c(0.5, 0.6, 0.7, 0.8)

    expected.fixed <- q^2 * dom.alleles.cts
    exp.fixed.cts <- sum(expected.fixed)
}


##test2
test1013 <- full.tables[[1]]
pos.cov1013 <- test1013[,7] > 10
test1013.1 <- test1013[pos.cov1013, ]
max.alt1013 <- apply(test1013.1[,8:11], 1, max)
alt.ratio1013 <- max.alt1013/test1013.1[,7]
fixed1013 <- sum(alt.ratio1013>0.95)

test3367 <- full.tables[[6]]
pos.cov3367 <- test3367[,7] > 10
test3367.1 <- test3367[pos.cov3367, ]
max.alt3367 <- apply(test3367.1[,8:11], 1, max)
alt.ratio3367 <- max.alt3367/test3367.1[,7]
fixed3367 <- sum(alt.ratio3367>0.95)



##sum(alt.ratio > 0.9) is only 118

alt.ratio <- alt.ratio1013
q.squared5 <- alt.ratio^(2*5)
q.squared6 <- alt.ratio^(2*6)
q.squared7 <- alt.ratio^(2*7)
q.squared8 <- alt.ratio^(2*8)
q.squared9 <- alt.ratio^(2*9)
q.squared10 <- alt.ratio^(2*10)


exp.fixed.cts <- function(full.tab, cov.thresh=10,
                          cell.count=1,
                          fixed.ratio=0.95){
    pos.cov <- full.tab[,7] > cov.thresh
    cov.tab <- full.tab[pos.cov,]
    alt.allele <- apply(full.tab[,8:11], 1, max)
    #alt.allele <- rowSums(cov.tab[,8:11])
    
    alt.ratio <- alt.allele/(alt.allele+cov.tab[,7])

    fixed <- (alt.ratio>fixed.ratio)
    num.fixed <- sum(fixed)

    exp.fixed.cts <- vector(length=length(cell.count))
    names(exp.fixed.cts) <- cell.count
    
    for(i in 1:length(cell.count)){
      #alt.ratio.cell <- ((floor(alt.ratio * 2 * cell.count))/(2*cell.count))
      
      alt.ratio <- alt.ratio[(alt.ratio>=.5)]
      
      q.squared <- alt.ratio^(2*cell.count)
      exp.fixed.cts[i] <- sum(q.squared)
    }

    fixed.stats <- list(number.fixed=num.fixed, expected=exp.fixed.cts)

    return(fixed.stats)
}

fixed.allele.stats1013 <- lapply(full.tables[c(1,3)], function(x){exp.fixed.cts(full.tab=x,
                                                                            cov.thres=5,cell.count=1:10,
                                                                            fixed.ratio=.99)})
save(fixed.allele.stats, file='fixed.allele.stats.rda')
