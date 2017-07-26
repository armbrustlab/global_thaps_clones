
source('../../../../R/allFunctions.R')
source('../get.id.pos.R')
source('../get.exon.pos.R')


#files = c('tp1007.tab','tp1012.tab','tp1013.tab','tp1014.tab','tp1015.tab',
#	'tp3367.tab','tp1335.tab')
files <- c('Tp1007-Q40.tab','Tp1012-Q40.tab','Tp1013-Q40.tab','Tp1014-Q40.tab',
           'Tp1015-Q40.tab','Tp3369-Q40.tab','Tp1335-Q40.tab')

snpfiles = c('../tableDataQ13/tp1007.pos.tab','../tableDataQ13/tp1012.pos.tab',
    '../tableDataQ13/tp1013.pos.tab','../tableDataQ13/tp1014.pos.tab',
    '../tableDataQ13/tp1015.pos.tab','../tableDataQ13/tp3367.pos.tab',
    '../tableDataQ13/tp1335.pos.tab')

                                        #vcfdir <- '/share/projects/Thaps-strain-genomes/polymorphism/vcf_20111027_noBAQ_qualfilt/'
#indel <- c('Tp1007.indels.vcf')#,'Tp1012.indels.vcf','Tp1013.indels.vcf','Tp1014.indels.vcf',
	#'Tp1015.indels.vcf','IT.indels.vcf','Tp1335.indels.vcf')
#idfiles <- paste(vcfdir, indel, sep='')
#gff <- '/share/projects/Thaps-strain-genomes/gff/thaps3.models.extended.gff'

##load R data files
#load('full.tables.rda')
#load('indel.list.rda')
#load('indel.tab.rda')

##read and parse the gff files
#g <- gffRead(gff)
#g1 <- parseGffByChrome(g)

##read and parse the indel files and then obtain the offsets

##Change trim to False if we want entire tables
trim <- FALSE
ref.tab <- FALSE

##genome positions for coding sequences
codingInd <- unique(unlist(global.coor.pos))

full.tables.02.25.15 <- vector(mode='list', length=length(files))
for(j in 1:length(files)){

    t1007 <- read.table(files[j], header=TRUE)
    ts1007 <- read.table(snpfiles[j])
    colnames(ts1007) = c('chr','pos','snp')

    if(ref.tab){
        exon <- rep(F, nrow(ts1007))
        Ref <- exon
        exon[codingInd] <- TRUE
        thaps.ref.tab <- cbind(ts1007[,1:2], Ref, exon)
        names(thaps.ref.tab) <- c('Chr','Pos','Ref','exon')

    }


    if(!trim){
        Chr <- rep(NA, nrow(ts1007))
        Pos <- Chr
        Ref <- Chr
        exon <- rep(F, nrow(ts1007))
    }
    a <- rep(0, nrow(ts1007))
    g <- a
    c <- a
    t <- a
    n <- a
    Cov <- a
    .match <- a
    indel <- rep(F, nrow(ts1007))

    if(!trim){
        append.tab <- data.frame(Chr = Chr, Pos = Pos, Ref = Ref, Cov = Cov, a = a,
                                 g = g, c = c, t = t, n = n, .match = .match, exon=exon,
                                 indel=indel)
        colnames(append.tab)[1:10] <- colnames(t1007)
        colnames(append.tab)[3] <- 'Ref'
      }
    else {
        append.tab <- data.frame(Cov = Cov, a = a,
                                 g = g, c = c, t = t, n = n, .match = .match,
                                 indel=indel)
        colnames(append.tab)[1:7] <- colnames(t1007)[4:10]
        colnames(append.tab)[3] <- 'Ref'
    }




###using chrome2genome now
    chr <- as.character(unique(t1007[,1]))
    ind = match(chr, t1007[,1])
    ind.end <- c(ind[2:length(ind)] - 1, nrow(t1007))
                                        #inds <- match(chr, ts1007[,1])
                                        #inds.end <- c(0,inds[2:length(inds)] - 1)
                                        #offset <- vector(length=nrow(t1007))
                                        #for(i in 1:length(ind.end)){
                                        #  offset[ind[i]:ind.end[i]] <- (t1007[ind[i]:ind.end[i],2] + inds.end[i])
                                        #}
                                        #for(k in 1:ncol(t1007)){
                                        #if(k==1||k==3){
                                        #    append.tab[offset,k] <- as.character(t1007[,k])
                                        #}
                                        #  else{
                                        #    append.tab[offset,k] <- t1007[,k]
                                        #  }
                                        #}

    chr.pos <- mapply(function(x,y){t1007[x:y,2]}, ind, ind.end, SIMPLIFY=FALSE)
    genome.pos <- mapply(function(x,y){chrome2genome(x,y)}, chr, chr.pos, SIMPLIFY=FALSE)
    genome.pos.vec <- unlist(genome.pos)

                                        #append.tab[genome.pos.vec,] <- t1007
                                        #append.tab[genome.pos.vec,1] <- as.character(t1007[,1])
                                        #append.tab[genome.pos.vec,3] <- as.character(t1007[,3])
                                        #append.tab[,14] <- FALSE
                                        #append.tab[,15] <- FALSE

    if(ref.tab){
        thaps.ref.tab[genome.pos.vec,"Ref"] <- t1007[,'Ref']
        save(thaps.ref.tab, file='thaps.ref.tab.rda', compress=TRUE)
    }

    for(k in 1:ncol(t1007)){
        if(!trim){
            if(k==1||k==3){
                append.tab[genome.pos.vec,k] <- as.character(t1007[,k])
            }
            else{
                append.tab[genome.pos.vec,k] <- t1007[,k]
            }
        }
        else{
            append.tab[genome.pos.vec,k-3] <- t1007[,k]
        }
        append.tab[id.global.pos[[j]],'indel'] <- TRUE
    }

    if(!trim){
        append.tab[codingInd,'exon'] <- TRUE
    }

    #tp.full.tab <- cbind(ts1007[,3], append.tab)
    #colnames(tp.full.tab)[1] <- colnames(ts1007)[3]
    full.tables.02.25.15[[j]] <- cbind(ts1007[,3], append.tab)
    colnames(full.tables.02.25.15[[j]])[1] <- colnames(ts1007)[3]

}



#names(full.tables) <- unlist(lapply(files, function(x){strsplit(x, '.tab')}))
#save(full.tables, file='full.tables.rda', compress=TRUE)
#full.tables.des.plus <- lapply(full.tables, function(x){y <- x[1376223:1796217,];
#                                                 return(y)})
#names(full.tables.des.plus) <- names(full.tables)
#load('full.tables.1335.rda')

#chr <- unique(indel.tab[[1]][,1])
#offset <- match(chr, full.tables[[1]][,1])
#offset1<- offset-1

##reorder the gff list
#g1 <- g1[chr]



#indel.chr.starts <- lapply(indel.tab, function(x) {match(chr, x[,1])})
#indel.chr.ends <- lapply(indel.chr.starts, function(x) {(x-1)[2:length(x)]})
#indel.chr.ends1 <- mapply(function(x,y){c(x,y)}, indel.chr.ends, lapply(indel.tab, nrow), SIMPLIFY=FALSE)

##can also just add the offset to the coding regions as well....
#indels <- indel.list
##using the first one just to test
##indels <- indel.list[1]
#for(i in 1:length(chr)){
                                        #print(i)
#  g1[[i]][,4:5] <- g1[[i]][,4:5]+offset1[i]
#  for(j in 1:length(indels)){
                                        #print(j)
#    indels[[j]][indel.chr.starts[[j]][i]:indel.chr.ends1[[j]][i]] <-
#      (indels[[j]][indel.chr.starts[[j]][i]:indel.chr.ends1[[j]][i]] + offset1[i])
#  }
#}

#coding <- lapply(g1, function(x){unlist(mapply(function(y,z){y:z}, x[,4], x[,5]))})
#codingInd <- unlist(coding)

save(full.tables.02.25.15, file='full.tables.02.25.15.rda', compress=TRUE)


