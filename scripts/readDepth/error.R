
source('../../../R/allFunctions.R')
build=FALSE
global.error.rate <- FALSE
files <- c('tp1007.tab')#,'tp1012.tab','tp1013.tab','tp1014.tab',
           #'tp1015.tab','tp3369.tab','tp1335.tab')
snpfiles <- c('tp1007.pos.tab')#,'tp1012.pos.tab','tp1013.pos.tab',
              #'tp1014.pos.tab','tp1015.pos.tab','tp3369.pos.tab',
              #'tp1335.pos.tab')


if(build){
  source(buildTabs.R)
}

load('ftdp.rda')
full.tables.des <- lapply(full.tables.des.plus, function(x){y = x[1:319995,]})

nt.stats <- vector(mode='list', length=7)

for(i in 1:length(full.tables.des)){
  a = which(full.tables.des[[i]][,'Ref2']=='A')
  g = which(full.tables.des[[i]][,'Ref2']=='G')
  c = which(full.tables.des[[i]][,'Ref2']=='C')
  t = which(full.tables.des[[i]][,'Ref2']=='T')
  n = which(full.tables.des[[i]][,'Ref2']=='N')
  a.stats <- colSums(full.tables.des[[i]][a,8:13])
  g.stats <- colSums(full.tables.des[[i]][g,8:13])
  c.stats <- colSums(full.tables.des[[i]][c,8:13])
  t.stats <- colSums(full.tables.des[[i]][t,8:13])
  n.stats <- colSums(full.tables.des[[i]][n,8:13])


  nt.stats[[i]] <- do.call(rbind, list(a.stats, g.stats, c.stats, t.stats))
  rownames(nt.stats[[i]]) <- c('a','g','c','t')


}




###Larry...start looking at the code here:
error.rates <- vector(mode='list', length=7)
nt.rates <- vector(mode='list', length=7)
for(i in 1:length(nt.stats)){

  error.tab <- matrix(0,nrow=4, ncol=5)
  for(j in 1:4){
    error.tab[,j] <- (nt.stats[[i]][,j]/(rowSums(nt.stats[[i]])))
  }

  error.tab[,5] <- (rowSums(nt.stats[[i]][,1:4])/rowSums(nt.stats[[i]]))

  rownames(error.tab) <- rownames(nt.stats[[i]])
  colnames(error.tab) <- c(colnames(nt.stats[[i]])[1:4], 'p')
  error.rates[[i]] <- error.tab

  nt.rates[[i]] <- matrix(as.vector(nt.stats[[i]])/
                          rep(apply(nt.stats[[i]],1,sum),6),4)
}

read.count <- lapply(full.tables.des, function(x) {return(x$Cov)})
num.nonref <- lapply(full.tables.des, function(x){rowSums(x[,8:12])})

error <- mapply(function(x,y){sum(x)/sum(y)},num.nonref,read.count)


##Setting up the GOF tests
#parameters: k, ref = (global,a,g,c,t), rate = (global or rate from error.rates),
#ftd is one of the tables from full.tables.des

global.errors <- mapply(function(x,y){sum(x)/sum(y)},
                        num.nonref, read.count)
ref.errors <- lapply(error.rates, function(x)
                     {return(x[,'p'])})

##now we can call gof tests with all parameters

no.cov <- lapply(full.tables.des, function(x)
                 {which(!is.na(x[,"Ref2"]))})

test <- full.tables.des
for(i in 1:length(no.cov)){
  test[[i]] <- full.tables.des[[i]][no.cov[[i]],]
}

snp.pos <- lapply(test, function(x)
                  {which(x[,'snp']!=1)})

test2 <- test
for(i in 1:length(snp.pos)){
  test2[[i]] <- test[[i]][snp.pos[[i]],]
}

full.tables.des <- test2

total.coverage <- lapply(full.tables.des, function(x)
                         {as.numeric(names(table(x[,'Cov'])))})
num.total.cov <-  lapply(full.tables.des, function(x)
                         {as.vector(table(x[,'Cov']))})


nt.cov.stats <- vector(mode='list', length=7)
for(i in 1:length(nt.cov.stats)){
  tmp <- vector(length=length(total.coverage[[i]]))
  for(j in 1:length(total.coverage[[i]])){
    ind <- full.tables.des[[i]][,'Cov'] == total.coverage[[i]][j]
    x <- (rowSums(full.tables.des[[i]][ind, 8:12])/full.tables.des[[i]][ind, 7])
    ##x is a vector of error rates for a given coverage of $k$
    x.m <- mean(x)
    tmp[j] <- x.m
    names(tmp) <- total.coverage[[i]]
  }
  nt.cov.stats[[i]] <- tmp
}
names(nt.cov.stats) <- names(total.coverage)

nt <- c('A','G','C','T')
nt.cov.ref.stats <- vector(mode='list', length=7)
for (i in 1:length(nt.cov.ref.stats)){
  tmp <- list(A = rep(NA, length=length(total.coverage[[i]])))
  tmp1 <- rep(tmp, 4)
  for(j in 1:length(total.coverage[[i]])){
    for(k in 1:length(nt)){
      ind <- (full.tables.des[[i]][, 'Cov'] == total.coverage[[i]][j] &
              full.tables.des[[i]][, 'Ref2'] == nt[k])
      x <- (rowSums(full.tables.des[[i]][ind, 8:12])/full.tables.des[[i]][ind, 7])
      x.m <- mean(x)
      tmp1[[k]][[j]] <- x.m
      names(tmp1[[k]]) <- total.coverage[[i]]
    }
    names(tmp1) <- nt
  }
  nt.cov.ref.stats[[i]] <- tmp1
}
names(nt.cov.ref.stats) <- names(total.coverage)


print('do i get here????')

##k comes from total.coverage, n comes from num.total.cov, no longer need ref I believe
##rate either comes from error (global) or from ref.errors and ftd is each entry in full.tables.des
ll <- sapply(total.coverage, length)
results <- vector(mode='list', length=7)
names(results) <- names(full.tables.des)

if(global.error.rate){
  print('I should not see this')
  for (i in 1:length(results)){
    results[[i]] <- vector(mode='list', length = ll[i])
    names(results[[i]]) <- total.coverage[[i]]
  }
} else{
  print('I should see this')
  for (i in 1:length(results)){
    results[[i]] <- vector(mode='list', length=4)
    names(results[[i]]) <- names(ref.errors[[i]])
    for(j in 1:4){
      results[[i]][[j]] <- vector(mode='list', length = ll[i])
      names(results[[i]][[j]]) <- total.coverage[[i]]
    }
  }
}


##If we use ref.error, need to add 4 intermed lists between results and the total.coverage part

##First remove rows that have no coverage
##Then remove rows that are determined to be SNPs


if(global.error.rate){
  print('I should not see this neither')
  for(i in 1:length(full.tables.des)){
    for(j in 1:ll[i]){

      results[[i]][[j]] <- goodness.fit(total.coverage[[i]][j],
                                        num.total.cov[[i]][j], ref="global",
                                        rate=error[i], ftd=full.tables.des[[i]])

    }
  }
} else{
  print('I should see this too')
  for(i in 1:length(full.tables.des)){
    for(j in 1:4){
      for(k in 1:ll[i]){
        results[[i]][[j]][[k]] <- goodness.fit(k = total.coverage[[i]][k],
                                               n = num.total.cov[[i]][k],
                                               ref = toupper(names(ref.errors[[i]]))[j],
                                               rate=ref.errors[[i]][j],
                                               ftd=full.tables.des[[i]])
      }
    }
  }

}

save(results, file='gof.ref.results.rda', compress=TRUE)

###generating stats

load('gof.ref.results.rda')

x1 = sapply(results[[1]][[1]][!sapply(results[[1]][[1]], is.character)], function(x) x$p.value)
x2 = sapply(results[[1]][[2]][!sapply(results[[1]][[2]], is.character)], function(x) x$p.value)
x3 = sapply(results[[1]][[3]][!sapply(results[[1]][[3]], is.character)], function(x) x$p.value)
x4 = sapply(results[[1]][[4]][!sapply(results[[1]][[4]], is.character)], function(x) x$p.value)

x1 = x1[-1]
x2 = x2[-1]
x3 = x3[-1]
x4 = x4[-1]

y1 = sapply(results[[2]][[1]][!sapply(results[[2]][[1]], is.character)], function(x) x$p.value)
y2 = sapply(results[[2]][[2]][!sapply(results[[2]][[2]], is.character)], function(x) x$p.value)
y3 = sapply(results[[2]][[3]][!sapply(results[[2]][[3]], is.character)], function(x) x$p.value)
y4 = sapply(results[[2]][[4]][!sapply(results[[2]][[4]], is.character)], function(x) x$p.value)

y1 = y1[-1]
y2 = y2[-1]
y3 = y3[-1]
y4 = y4[-1]

z1 = sapply(results[[7]][[1]][!sapply(results[[7]][[1]], is.character)], function(x) x$p.value)
z2 = sapply(results[[7]][[2]][!sapply(results[[7]][[2]], is.character)], function(x) x$p.value)
z3 = sapply(results[[7]][[3]][!sapply(results[[7]][[3]], is.character)], function(x) x$p.value)
z4 = sapply(results[[7]][[4]][!sapply(results[[7]][[4]], is.character)], function(x) x$p.value)

z1 = z1[-1]
z2 = z2[-1]
z3 = z3[-1]
z4 = z4[-1]


load("nt.cov.stats.rda")
load("nt.cov.ref.stats.rda")


###some stats
codingRegion <-  sapply(full.tables, function(x){sum(x[,'exon'])})
noncodingRegion <- nrow(full.tables[[1]]) - codingRegion
snps <- sapply(full.tables, function(x){sum(x[,'snp'])})
codingSNP <- sapply(full.tables, function(x){sum(x[x[,'exon'],'snp'])})
noncodingSNP <- snps-coding

exonSNP <- data.frame(cr = codingRegion, ncr = noncodingRegion, snps = snps,
                      cs = codingSNP, ncs = noncodingSNP)

codingStats <- rbind(codingRegion, noncodingRegion, snps, codingSNP, noncodingSNP)

##Union of SNPS

snps7Strains <- cbind(full.tables[[1]][,'snp'],full.tables[[2]][,'snp'],full.tables[[3]][,'snp'],
                      full.tables[[4]][,'snp'],full.tables[[5]][,'snp'],full.tables[[6]][,'snp'],
                      full.tables[[7]][,'snp'])

colnames(snps7Strains) <- names(full.tables)

union5SNPS <- snps7Strains[,1] | snps7Strains[,2] | snps7Strains[,4] | snps7Strains[,5] | snps7Strains[,7]
