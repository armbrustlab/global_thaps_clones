
#source('../../../R/allFunctions.R')
#vcfdir <- '/share/projects/Thaps-strain-genomes/polymorphism/vcf_20111027_noBAQ_qualfilt/'
#indel <- c('Tp1007.indels.vcf','Tp1012.indels.vcf','Tp1013.indels.vcf','Tp1014.indels.vcf',
	#'Tp1015.indels.vcf','IT.indels.vcf','Tp1335.indels.vcf')
#idfiles <- paste(vcfdir, indel, sep='')
gff.genes <- '/share/projects/Thaps-strain-genomes/gff/thaps3.models.extended.gff'
gff.exons <- '../../../../data/thaps3.models.extended.exons.gff'
#####Getting global indices for indels and exons

exons=TRUE
genes.wo.introns=TRUE

##Getting positions for exons

if(exons){
  gff <- gff.exons
}

g <- gffRead(gff)

neg.strand <- g[,'strand']=='-'
g1 <- g
g1[neg.strand,] <- g1[neg.strand, c(1:3,5,4,6:9)]

global.coor.pos <- vector(mode='list', length=nrow(g))
for(i in 1:nrow(g1)){
  exon.bounds <- chrome2genome(g1[i,1], c(g1[i,4], g1[i,5]))
  global.coor.pos[[i]] <- exon.bounds[1]:exon.bounds[2]
  #global.coor.pos[[i]] <- exon.bounds
}

if(genes.wo.introns){
    tmp <- strsplit(g1[,9], '; transcriptId ', fixed=T)
    tmp1 <- sapply(tmp, function(x){x[1]})
    tmp2 <- as.numeric(sapply(strsplit(tmp1, 'exonNumber', fixed=TRUE), function(x) {x[2]}))
    starts1 <- which(tmp2==1)

    exons <- as.numeric(sapply(tmp, function(x){x[2]}))
    starts <- match(unique(exons), exons)
    ends <- c(starts[2:length(starts)] - 1, length(exons))

    strands <- as.character(g1[,'strand'])

    ##sanity check
    if(!(identical(starts, starts1))){
       print ("Something squirrly")
    }

    genes <- vector(mode='list', length=length(starts))

    for(i in 1:length(starts)){

        tmplist <- vector(mode='list', length=length(starts[i]:ends[i]))
        index <- 1
        if(strands[i] == '+'){
          for(j in starts[i]:ends[i]){
            tmplist[[index]] <- global.coor.pos[[j]]
            index <- index+1
          }
        }
        else{
          for(j in ends[i]:starts[i]){
            tmplist[[index]] <- global.coor.pos[[j]]
            index <- index+1
        }

        print(i)
        genes[[i]] <- unlist(tmplist)

    }

        save(genes, file='genes.01.28.14.rda', compress=TRUE)
      }

  }




