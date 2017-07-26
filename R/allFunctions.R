getAttributeField <- function (x, field, attrsep = ";") {
  ##fixme (tc) need to find where I call this function 31 Jan 12
     s = strsplit(x, split = attrsep, fixed = TRUE)
     sapply(s, function(atts) {
         a = strsplit(atts, split = "=", fixed = TRUE)
         m = match(field, sapply(a, "[", 1))
         if (!is.na(m)) {
             rv = a[[m]][2]
         }
         else {
             rv = as.character(NA)
         }
         return(rv)
     })
}

gffRead <- function(gffFile, nrows = -1) {
  ##takes in a GFF file created by Chris and returns
  ##a table (swiped off the interweb I believe)
     cat("Reading ", gffFile, ": ", sep="")
     gff = read.table(gffFile, sep="\t", as.is=TRUE, quote="",
     header=FALSE, comment.char="#", nrows = nrows,
     colClasses=c("character", "character", "character", "integer",
       "integer", "character", "character", "character", "character"))
     colnames(gff) = c("chr", "source", "feature", "start", "end",
             "score", "strand", "frame", "attributes")
     cat("found", nrow(gff), "rows with classes:",
         paste(sapply(gff, class), collapse=", "), "\n")
     stopifnot(!any(is.na(gff$start)), !any(is.na(gff$end)))
     return(gff)
}

parseGffByChrome <- function(gff){
  ##each element of the return list needs to be listed
  ##by the Chrome
  chromeGff <- as.vector(gff[,1])
  uchromeGff <- unique(chromeGff)
  gffByChrome <- vector(mode="list", length=length(uchromeGff))
  names(gffByChrome) <- uchromeGff
  for(i in 1:length(uchromeGff)){
    index <- which(chromeGff == uchromeGff[i])
    gffByChrome[[i]] <- gff[index,]
  }
  names(gffByChrome) <- uchromeGff
  return(gffByChrome)
}


parseSNPByChrome <- function(snpFile){
  ##the snpFile is a Table where the first column indexes the Chromosome
  ##the return file is subtables where the first columns will all be the same Chrome

  chromeSNP <- as.vector(snpFile[,1])
  uchromeSNP <- unique(chromeSNP)
  snpByChrome <- vector(mode="list", length=length(uchromeSNP))
  names(snpByChrome) <- uchromeSNP
  for(i in 1:length(uchromeSNP)){
    index <- which(chromeSNP == uchromeSNP[i])
    snpByChrome[[i]] <- snpFile[index,]
  }
  names(snpByChrome) <- uchromeSNP
  return(snpByChrome)
}

snpsInRegion <- function(snp, s, t){
  ##this will take a list of snp tables and return
  ##a list of subtables indexed by start s and terminal t

  #if(length(snp)!= length(s)){error("The lengths of the three argments
   #                                  should be the same")}
  res <- vector(mode="list", length=length(snp))
  for(i in 1:length(snp)){
    res[[i]] <- mapply(function(x,y){snp[[i]][x:y,3]},s[[i]],t[[i]])}
  names(res) <- names(snp)
  return(res)
}


snpsInTx <- function(nSNPinEx, sIndex, tIndex){
  ##a function that returns the snps found in exons
  res <- vector(mode="list", length=length(nSNPinEx))
  names(res) <- names(nSNPinEx)
  for (i in 1:length(nSNPinEx)){
    res[[i]] <- mapply(function(x,y){nSNPinEx[[i]][x:y]},
                       sIndex[[i]],tIndex[[i]])}
  return(res)
}

snpDist <- function(snpPosVect){
  ##function that takes a vector of snp positions
  ##and returns their distance
  v0 <- which (snpPosVect == 1)
  if(length(v0)>2){
  v1 <- v0[1:(length(v0)-1)]
  v2 <- v0[2:length(v0)]
  v3 <- v2-v1}
  else{
    v3 = v0}
  v3 <- c(v0[1], v3)
  return(v3)
}

ex2Tx <- function(exonList){
  ##code that translates exons to transcripts (??)
  tmpInd <- which(names(exonList) == "exonNumber 1")
  endInd <- c(tmpInd[2:length(tmpInd)]-1, length(exonList))
  tmp2 <- vector(mode="list", length=length(tmpInd))
  for(i in 1:length(tmp2)){
    xyz <- unlist(exonList[tmpInd[i]:endInd[i]])
    names(xyz) <- NULL
    tmp2[[i]] <- xyz
  }
  return(tmp2)
}

snpCounter <- function(snpInTx, frac=FALSE){
##returns the number or proportion of snps in chrome
  l <- length(snpInTx)
  tot <- sum(snpInTx)
  snptotal <- numeric(length=l)
  snpFrac <- snptotal

  for(i in 1:l){

    if(i==1) {snptotal[i]=snpInTx[i]}
    else {snptotal[i]=snpInTx[i]+snptotal[i-1]}
    snpFrac[i]=snptotal[i]/tot
  }
  if(frac){return(snpFrac)}
  else return(snptotal)
}


sortGffByStartPos <- function(gff){
  ##sorts a GFF file by start positions
  strt <- as.numeric(gff[, "start"])
  sortedGFF <- gff[order(strt),]
  sortedGFF
}

mergeOverlapEx <- function(gff){
  ##This merges overlapping exons into
  ##a single meta exon. This is used because
  ##we don't want to over count snps in exons
  gff <- sortGffByStartPos(gff)
  d <- dim(gff)
  s <- gff[,'start']
  e <- gff[,'end']
  boundaryMat <- matrix(0,d[1],2)
  boundaryMat[1,] <- c(s[1], e[1])
  ##trying to debug...double check this!!! tc checked...
  ind <- 1
  for(i in 2:d[1]){
    if(e[ind]<s[i]){boundaryMat[i,] <- c(s[i], e[i]);
                    ind <- i}
    ##if(e[i-1]>s[i] && e[i-1]>=e[i]) {boundaryMat[i,] <- c(0,0)}
    ##I don't need to code the previous case I believe because
    ##the matrix already has double 0's

    ##if we replace [ind,2], we are looking at the new end so
    ##we should bump ind because we are looking at the vector e
    if(e[ind]>=s[i] && e[ind]<e[i]){boundaryMat[ind,2] <- e[i];
                                  ind <- i}
  }
  toDrop <- which(boundaryMat[,1]==0)
  if(length(toDrop)>0){
    boundaryMat <- boundaryMat[-toDrop,,drop=FALSE]
  }
  boundaryMat
}

nonCodedBounds <- function(boundaryMat, chromeLength){
  ##once we have the exon boundaries, we can find the
  ##non-coding regions of the genomes
  d <- dim(boundaryMat)
  if(boundaryMat[1,1]!=1){
    ncBounds <- matrix(nrow=(nrow(boundaryMat)+1), ncol=2)
    ncBounds[1,] <- c(1,boundaryMat[1,1]-1)
    for(i in 2:nrow(boundaryMat)){
      ncBounds[i,] <- c(boundaryMat[i-1,2]+1, boundaryMat[i,1]-1)
    }
    ncBounds[nrow(ncBounds),] <- c(boundaryMat[nrow(boundaryMat),2]+1,
                                   chromeLength)

  }

  if(boundaryMat[1,1]==1){
    ncBounds <- matrix(nrow=(nrow(boundaryMat)), ncol=2)
    for(i in 2:nrow(boundaryMat)){
      ncBounds[i-1,] <- c(boundaryMat[i-1,2]+1, boundaryMat[i,1]-1)
    }
    ncBounds[nrow(ncBounds),] <- c(boundaryMat[nrow(boundaryMat),2]+1,
                                   chromeLength)
  }

  return(ncBounds)
}

getNonCodedBoundary <- function(gff, snpFile){
  ##second function to get the non-coding regions and also
  ##to see which snps belong to non-coding parts
  gff <- sortGffByStartPos(gff)
  d <- dim(gff)
  s <- gff[,'start']
  e <- gff[,'end']
  boundaryMat <- matrix(0,d[1]+1,2)
  boundaryMat[1,] <- c(1,s[1]-1)
  ind <- 1
  for(i in 2:d[1]){


    if(e[i-1]+1<s[i]){boundaryMat[i,] <- c(e[i-1]+1,s[i]-1)}
    ##the above is the only case where there exist a gap
    ##between exons. other cases will need book-keeping
    if(e[i-1]>s[i] && e[i-1]>=e[i]){e[i] <- e[i-1]}
    #if(e[i-1]+1>=s[i] && e[i-1]<e[i]){}
    ##the above case is where an exon is exactly one space
    ##apart so there is no space in-between.
  }
  boundaryMat[d[1]+1,] <- c(e[length(e)]+1, nrow(snpFile))
  toDrop <- which(boundaryMat[,1]==0)
  if(length(toDrop)>0){
    boundaryMat <- boundaryMat[-toDrop,,drop=FALSE]
  }
  boundaryMat
}

##need to test
calcTXProp <- function(gff, snpFile){
  ##compute the proportion of snps in transcripts
  exonBoundsMat <- mergeOverlapEx(gff)
  exonLength <- exonBoundMat[,2] - exonBoundMat[,1]
  codingLength <- sum(exonLength)
  chromeLength <- nrow(snpFile)
  #nonCodingLength <- chromeLength-codingLength
  coding2Tot <- c(codingLength, chromeLength)
  coding2Tot
}

getSNPRate <- function(snp, byChrome=FALSE){
  ##getting the snp rates from a genome
  ntNumber <- nrow(snp)
  snpNumber <- sum(snp[,3])
  snpRateGlobal <- snpNumber/ntNumber


  if(byChrome){
    snpByChrome <- parseSNPByChrome(snp)
    ntNumber <- sapply(snpByChrome, nrow)
    snpNumber <- sapply(snpByChrome, function(x) {sum(x[,3])})
    snpRateByChrome <- snpNumber/ntNumber

    rate <- vector(mode='list', length=2)
    rate[[1]] <- snpRateGlobal
    rate[[2]] <- snpRateByChrome
    names(rate[[2]]) <- names(snpRateByChrome)

    return(rate)
  }

  else{return(snpRateGlobal)}
}

getChromeLength <- function(snp){
  ##getting the chromosome length as seen from the snp file
  snpByChrome <- parseSNPByChrome(snp)
  l <- sapply(snpByChrome, nrow)
  return(l)
}


desertLength <- function(snp, byChrome=FALSE){
  ##computing a desert length by using intersnp regions as deserts
  snp.col <- snp[,3]
  snp.pos <- which(snp.col == 1)
  desert.length <- mapply(function(x,y) y-x, snp.pos[1:length(snp.pos)-1],
                          snp.pos[2:length(snp.pos)])

  desert.length <- c(snp.pos[1], desert.length)
  desert.length <- desert.length - 1
  desert.length

  if(byChrome){
   snp.ch <- parseSNPByChrome(snp)
   snp.col.ch <- lapply(snp.ch, function(x) x[,3])
   snp.pos.ch <- lapply(snp.col.ch, function(x){which(x==1)})
   desert.len.ch <- lapply(snp.pos.ch, function(x){
                           x[2:length(x)] - x[1:length(x)-1]
                           })
   desert.len.ch <- mapply(function(x,y){c(y[1],x)}, desert.len.ch, snp.pos.ch)
   desert.len.ch <- lapply(desert.len.ch, function(x) x-1)

   des.len <- vector(mode="list", length=2)
   des.len[[1]] <- desert.length
   des.len[[2]] <- desert.len.ch
  }

  else{
    des.len <- desert.length
  }

return(des.len)

}


trimSNPTable <- function(snp){
  #tmp <- read.table(snp)
  ##getting rid of bottom drawer snps...needs to be
  ##cleaned up so we can exclude things on input
  loc <- unique(snp[,1])[1:27]
  ind <- which(snp[,1]%in%loc)
  newTab <- snp[ind,]

}

snpPositions <- function(snp, byChrome=TRUE){
  ##actually returns the positions along the
  ##chromosomes where we find snps
  if(byChrome){
    snp1 <- trimSNPTable(snp)
    parsed.snp <- parseSNPByChrome(snp1)
    ##The ChEnd will give the last base pair of the chromosome regardless
    ##of the prescence of a snp...this is done for downstream tests
    ChEnd <- lapply(parsed.snp, nrow)
    snpIndex <- lapply(parsed.snp, function(x)
                       {which(x[,3]==1)})
    snpIndex <- mapply(function(x,y)c(x,y), snpIndex, ChEnd)
  }



  else{
    snpIndex <- which(snp[,3]==1)
    snpIndex
  }

  snpIndex
}

snpModel <- function(snpIndices, p, n, lTail=TRUE, log.p=FALSE){
  #options(error=recover)
  ##This is the main function of the package. It takes a vector
  ##of the snp locations (coordinates in the genome or chrome)

  origN <- n
  testTab <- pvalTable(1:350000, n=n, prob=p)
  if(lTail){testTab1 <- testTab[,"lower.tail"]}
  else{testTab1 <- testTab[,"upper.tail"]; print("got here")
     }

  j <- 1
  chromLen <- snpIndices[length(snpIndices)]
  pVals <- vector(length=chromLen)
  nSnps <- vector(length=chromLen)

  for(i in 1:chromLen){
    if(n>0){
      len <- snpIndices[j+n]-i
      if(n == origN){
        pVals[i] <- testTab1[len-n]
      }
      else{
        pVals[i] <- pnbinom(q=len-n, size=n, prob=p, lower.tail=lTail,
                            log.p=log.p)
      }

      ##sanity check!!!
      #if(pVals[i] != testTab[len-n]){print("not working"); print(c(n,i,len));
      #                               print(c(pVals[i],testTab[len-n]));break}

      nSnps[i] <- snpIndices[j+n]
    }
    else{

      pVals[i] <- (1-p)^(chromLen-i)
      nSnps[i] <- -1
    }

    if(i == snpIndices[j]){j <- j+1}

    if(j+n > length(snpIndices)){n <- n-1} ##need to mark this somehow...
  }

  pv <- cbind(pVals, nSnps)
  return(pv)

}


bounds <- function(pval, thresh=0.0001, desert=TRUE){
  pvalVect <- pval[,1]
  if(desert){ind <- which(pvalVect<thresh)}
  else {ind <- which(pvalVect>(1-thresh))}
  if(length(ind)>0){
    dif <- ind[2:length(ind)] - ind[1:(length(ind)-1)]
    start.bounds <- c(1,(which(dif>1)+1))
    end.bounds <- c(which(dif>1), length(ind))
    bounds <- cbind(ind[start.bounds], ind[end.bounds])
    s <- vector(mode='list', length=nrow(bounds))
    s1 <- vector(length=nrow(bounds))
    for(i in 1:nrow(bounds)){
      s[[i]] <- unique(pval[bounds[i,1]:bounds[i,2],2])
      s1[[i]] <- max(s[[i]])
    }
    bounds <- cbind(bounds, s1)
  }
  else{bounds <- -1}
  bounds <- bounds[-nrow(bounds),,drop=FALSE]
  bounds
}



hsBounds <- function(pval, thresh=0.0001, desert=FALSE){

  b <- bounds(pval=pval, thresh=thresh, desert=desert)
  b
}

threePBound <- function(hsBds){
  z <- hsBds[,3] + hsBds[,2] - hsBds[,1]
  z1 <- cbind(hsBds, z)
  colnames(z1) <- c('start','end pval','end nSNP','end')
  z1
}

desertBounds <- function(pval, thresh=0.0001){
  options(error=recover)
  b <- bounds(pval=pval, thresh=thresh)
  b
}

enumHS <- function(hsBd){

  tot <- vector(mode='list', length=2)
  tot[[1]] <- lapply(hsBd, function(x){sapply(x, function(y){nrow(y)-1})})
  names(tot[[1]]) <- names(hsBd)

  xx <- NULL
  for(i in 1:length(tot[[1]])){
    xx <- cbind(xx, tot[[1]][[i]])

  }
  colnames(xx) <- names(hsBd)
  tot[[2]] <- xx
  tot
}

#snpModel2 <- function(snpIndices, p, n, lTail=TRUE, log.p=FALSE){

#  j <- 1
#  chromLen <- snpIndices[length(snpIndices)]
#  pVals <- vector(length=chromLen)

# for(i in 1:chromLen){
#    if(n>0){

#      if(i %in% snpIndices){n <- n-1}##snp at position i
#      if(j<=n){boundSnpVec <- snpIndices[1:(j+n-1)]}
      ##boundary condition if there are fewer than
      ## n snps prior to the j^th snp
#      if(j>n) {boundSnpVec <- snpIndices[(j-n):(j+n-1)]}

#      lenVec <- vector(length=length(boundSnpVect)-n+1)

      #topHalf <- boundSnpVec[length(boundSnpVec):]




#      len <- snpIndices[j+n]-i
#      pVals[i] <- pnbinom(q=len-n, size=n, prob=p, lower.tail=lTail,
#                          log.p=log.p)

#      if(i %in% snpIndices) {n <- n+1}
#    }


#  }

#}

snpOverlap <- function(snp1, snp2, table=TRUE, jaccard=TRUE){
  if(table){snp1 <- snp1[,3]; snp2 <- snp2[,3]}

  snpI <- snp1 * snp2
  snpU <- snp1 + snp2
  mode(snpU) <- "logical"
  mode(snpU) <- "numeric"
  jac <- snpI/snpU


  if(jaccard){xx <- vector(mode="list", length=3);
            xx[[1]] <- snpI; xx[[2]] <- snpU; xx[[3]] <- jac;
            return(xx)}
  else(return(snpI))


}

interSNPLen <- function(snpPosVect){
  snpPosVect <- c(0,snpPosVect)
  snpInterval <- snpPosVect[2:length(snpPosVect)]-
    snpPosVect[1:(length(snpPosVect)-1)]-1
  snpInterval
}

snpIntervalPerm <- function(snpPosVect){

  snpInterval <- interSNPLen(snpPosVect)

  shuffle <- sample(1:length(snpInterval), length(snpInterval))
  reorder <- snpInterval[shuffle]
  newSnpPos <- vector(length=length(reorder))
  for(i in 1:length(newSnpPos)){
    newSnpPos[i] <- sum(reorder[1:i])+i
  }

  return(newSnpPos)

}

qq4Snps <- function(snp1, snp2, ...){
  if(snp1[1]!=1){snp1 <- c(1,snp1)}
  if(snp2[1]!=1){snp2 <- c(1,snp2)}

  snp1Int <- snp1[2:length(snp1)]-snp1[1:(length(snp1)-1)]-1
  snp2Int <- snp2[2:length(snp2)]-snp2[1:(length(snp2)-1)]-1

  qqplot(snp1Int, snp2Int,)

}

interSNP <- function(snpPosV){
  #calculates distance between snps in a chrome
  iSnp <- snpPosV[2:length(snpPosV)]-snpPosV[1:(length(snpPosV)-1)]-1
  iSnp
}

mergeOverlapHS <- function(dataMat){
  options(error=recover)
  r <- nrow(dataMat)
  if(r==0){return(NULL)}




  newMat <- matrix(0, nrow=r, ncol=2)
  i <- 1
  while(i <= r){
    ending <- max(which(dataMat[i,4]>=dataMat[,1]))
    if(ending > i){dataMat[i,4] <- dataMat[ending,4]
                   dataMat <- dataMat[-((i+1):ending),,drop=FALSE]
                   r <- nrow(dataMat)}
    else{
      newMat[i,1] <- dataMat[i,1]
      newMat[i,2] <- dataMat[ending,4]
                                        #if(end == i){i <- i+1}
                                        #else{i <- ending+1}
      i <- i+1}
  }
  ind <- which(newMat[,1]==0)
  if(length(ind)>0){
    newMat <- newMat[-ind,,drop=FALSE]
  }

  colnames(newMat) <- c('hs origin','hs terminate')
  return(newMat)

  ##old code...
  #if(r==1){newMat <- dataMat[-1,,drop=FALSE]; return(newMat)}
  #for(i in 1:(r-1)){
  #  if(dataMat[i,4] >= dataMat[i+1,1]){
  #    newMat[i,1]=dataMat[i,1]
  #    newMat[i,2]=dataMat[i+1,4]
  #  }
  #  else{
  #    newMat[i,] <- dataMat[i, c(1,4)]
  #  }
  #}


}

intervalLen <- function(dataMat){
  Length <- dataMat[,2]-dataMat[,1]-1
  dataMat1 <- cbind(dataMat, Length)
  dataMat1
}

mergeDeserts <- function(dataMat, mergeThresh=100,
                         secThresh=100){

  dummy <- FALSE
  r <- nrow(dataMat)
  if(r==0){return(NULL)}
#  if(r==1){newMat <- dataMat[,c(1,3),drop=FALSE];
#           colnames(newMat) <- c('desert origin','desert terminate');
#           return(newMat)}
  newMat <- matrix(0, nrow=r, ncol=2)

  i <- 1

  if(mergeThresh==Inf){
    dummy <- TRUE
    mergeThresh <- secThresh
    #print(mergeThresh)
    #print(paste("This is before:", nrow(dataMat), sep=" "))
    #print(dataMat[,3]-dataMat[,1])
  }



  while(i <=  r){
    options(error=recover)
    ending <- max(which((dataMat[i,3]+mergeThresh-1)>=dataMat[,1]))
    if(ending > i){dataMat[i,3] <- dataMat[ending,3]
                   dataMat <- dataMat[-((i+1):ending),,drop=FALSE]
                   r <- nrow(dataMat)}

    else{
    newMat[i,1] <- dataMat[i,1]
    newMat[i,2] <- dataMat[ending,3]
    i <- i+1}


    #if(end == i){i <- i+1}
    #else{i <- end+1}
  }
  ind <- which(newMat[,1]==0)
  if(length(ind)>0){
    newMat <- newMat[-ind,,drop=FALSE]
  }

  if(dummy){
    #print(paste("After", nrow(newMat), sep=" "))
    #print(newMat[,2]-newMat[,1])
    dummy <- FALSE
  }

  colnames(newMat) <- c('desert origin','desert terminate')
  return(newMat)


}


pvalTable <- function(r, n, prob, log.p=FALSE){

  l <- pnbinom(r, size=n, prob=prob, lower.tail=TRUE, log.p=log.p)
  u <- pnbinom(r, size=n, prob=prob, lower.tail=FALSE, log.p=log.p)

  tab <- cbind(l,u)
  colnames(tab) <- c("lower.tail", "upper.tail")
  return(tab)
}

overLapCheck <- function(boundaryTab){
  if(nrow(boundaryTab)<2){return(Inf)}
  dis <- (boundaryTab[(2:nrow(boundaryTab)),1] - boundaryTab[(1:nrow(boundaryTab)-1),2])
  dis
}

##indel functions in this group:
indelRegions<- function(inserts, thresh, ins = TRUE){

  if(ins){ins <- which(inserts <= thresh)
          return(ins)
  }
  else{dels <- which(inserts>=thresh)
       return(dels)
  }

}

indelBounds <- function(mp.stats, thresh, ins=TRUE){
  avg <- mp.stats$mp_ins_mean
  inserts <- mp.stats$per_nt_mp_ins
  regions <- indelRegions(inserts, thresh, ins=ins)
  if(length(regions)>0){
    dif <- regions[2:length(regions)] - regions[1:(length(regions)-1)]
    st.bds <- c(1,(which(dif>1)+1))
    end.bds <- c(which(dif>1), length(regions))
    bounds <- cbind(regions[st.bds], regions[end.bds])
    len <- bounds[,2]-bounds[,1]+1
    bounds <- cbind(bounds, len)

    max.indels.ind <- apply(bounds, 1, function(x){xyz <- which(inserts[x[1]:x[2]] ==
                                                                max(inserts[x[1]:x[2]]));
                                                 (x[1]:x[2])[xyz]})
    ##the next apply fails if your bounds matrix only has 1 row! the previous apply will
    ##return a vector and not a list....so a hack is in order
    if(!(is.list(max.indels.ind))){x <- vector(mode="list", length=1)
                                 x[[1]] <- max.indels.ind
                                 max.indels.ind <- x}


    max.indels.ind <- sapply(max.indels.ind, function(x){if(length(x)%%2==0){x[(length(x)/2)]}
                                                       else{x[((length(x)+1)/2)]}})
    max.indels <- inserts[max.indels.ind]
    bounds <- cbind(bounds, abs(avg-max.indels))
    bounds <- cbind(bounds, max.indels.ind)

    if(ins){colnames(bounds) <- c("Start","End","Start/End Distance",
                                  "Estimated Insertion Length", "Peak Position")}
    else{colnames(bounds) <- c("Start","End","Start/End Distance",
                                  "Estimated Deletion Length", "Peak Position")}

    return(bounds)
  }
  else(return(NULL))
}

estimateIndels <- function(json, chromes, thresh=500, ins=TRUE){
  mainChromes <- json$nodes[chromes]
  if(ins){
    Thresh <- sapply(mainChromes, function(x){x$mp_ins_mean - thresh})
  }
  else{
    Thresh <- sapply(mainChromes, function(x){x$mp_ins_mean + thresh})
  }

    indels <- mapply(function(x,y,z){indelBounds(x,y,ins=ins)},
                     mainChromes,Thresh,ins, SIMPLIFY=FALSE)
    indels
}

s1 <- function(x, sp){
  y <- strsplit(x, split=sp, fixed=TRUE)
  y
}

obtainNodeInfo <- function(node){
  x <- strsplit(node, "|", fixed=TRUE)
  n1 <- sapply(x, function(y){return(y[1])})
  n2 <- as.numeric(sapply(x, function(y){
    t <- s1(y[2], sp="_")[[1]][1];
    u <- s1(t, sp="$")[[1]][1];
    return(u)
    }))
  w <- cbind(n1,n2)
  w

}

singleChrGraph <- function(combNSC, chr){
  ##if this function does not work change combNSC back to
  ##combNodesSameChr and write a for loop
  #debug()
  perChr <- (combNSC[,1]%in%chr)
  cNSC <- combNSC[perChr,]
  cNSC <- cNSC[,c(3,4)]
  mode(cNSC) = 'numeric'
  deg <- cNSC[,2] - cNSC[,1]
  cNSC <- cbind(cNSC, deg)
  nodes = sort(union(cNSC[,1], cNSC[,2]))
  nodeDeg <- matrix(0, nrow=length(nodes), ncol=2)
  rownames(nodeDeg)=as.character(nodes)
  colnames(nodeDeg)=c('indegree', 'outdegree')
  nodeDeg[as.character(cNSC[,2]),1] <- cNSC[,3]
  nodeDeg[as.character(cNSC[,1]),2] <- cNSC[,3]
  return(nodeDeg)
}

contigGraph <- function(jsonFile, interChrome=TRUE){
  #options(error=recover)
  ##first we obtain the node information (i.e. which contig and which section)
  #n1 <- lapply(jsonFile, function(x){sapply(x$edges, function(y){return(y$n1)})})
  n1 <- sapply(jsonFile$edges, function(x){return(x$n1)})
  n2 <- sapply(jsonFile$edges, function(x){return(x$n2)})

  #n2 <- lapply(jsonFile, function(x){sapply(x$edges, function(y){return(y$n2)})})
  #node1 <- lapply(n1, obtainNodeInfo)
  #node2 <- lapply(n2, obtainNodeInfo)
  node1 <- obtainNodeInfo(n1)
  node2 <- obtainNodeInfo(n2)

  combineNodes <- cbind(node1,node2)
  combineNodes <- combineNodes[,c(1,3,2,4)]
  ##now we get rid of anything not belonging to the assembled Chromosomes
  chr <- unique(combineNodes[,1])
  chr1 <- chr[grep("Chr", chr)]
  index1 <- (combineNodes[,1] %in% chr1)
  index2 <- (combineNodes[,2] %in% chr1)
  index <- (index1 & index2)
  ##next we restrict to the nodes that are within the same Chromosomes
  combineNodesChr <- combineNodes[index,,drop=FALSE]
  sameChr <- (combineNodesChr[,1] == combineNodesChr[,2])
  combNodesSameChr <- combineNodesChr[sameChr,,drop=FALSE]
  colnames(combNodesSameChr) <- c('','','n1','n2')
  mode(combNodesSameChr[,c(3,4)])="numeric"

  #chrList <- vector(length=length(chr), mode='list')
  #names(chrList) <- chr
  ##code now breaks at this call...
  chrList <- lapply(chr1, function(x){singleChrGraph(combNSC = combNodesSameChr,
                                                    chr=x)})
  ##each entry of the list should correspond to a particular chromosome
  names(chrList) <- chr1

  if(interChrome){
    allList <- vector(length=2, mode='list')

    sameChrome <- (combineNodes[,1]==combineNodes[,2] & (combineNodes[,1]%in%chr1 & combineNodes[,2]%in%chr1))

    differentCh <- combineNodes[!sameChrome,,drop=FALSE]
    #differentCh <- differentCh[,c(1,3,2,4), drop=FALSE]
    colnames(differentCh) <- c('Node 1','Node 2','Region on n1',' Region on n2')
    allList[[1]] <- chrList
    allList[[2]] <- differentCh
    names(allList) <- c('Within One Chome','Across Chrome')
    return(allList)
  }

  else{
    return(chrList)
  }

}

uniDirectional <- function(graphMat){

  ##We want to look for double 0's in one column
  ##this signifies a change in direction. if we
  ##add the i^th and (i^th + 1) elements, we get
  ##a 0 in the i^th position if the i^th +1 is also 0
  trans1 <- graphMat[1:(length(graphMat[,1])-1),1] +
    graphMat[2:length(graphMat[,1]),1]
  trans2 <- graphMat[1:(length(graphMat[,1])-1),2] +
    graphMat[2:length(graphMat[,1]),2]
  ##now we find the start positions of the double 0
  ##for both the first and second columns
  z1 <- which(trans1==0)
  z2 <- which(trans2==0)
  z <- unique(sort(c(z1,z2)))
  ##decide whether the first block is in the
  ##negative or positive orientation; if the
  ##first 0 occurs in the first column before
  ##the second, we have a positive orientation, etc
  fz1 <- (which(graphMat[,1]==0))[1]
  fz2 <- (which(graphMat[,2]==0))[1]

  ##if the first 0 occurs in colunm 2 first,
  ##we want to switch the orientation of the
  ##first connected component of the graph
  if(fz1>fz2){z <- c(0,z)}

  ##we now create a new graph matrix where
  ##all connected components have the same
  ##(positive) orientation
  if(length(z)%%2 == 1){z <- c(z, nrow(graphMat))}
  graph1 <- graphMat
  i <- 1
  while(i <= length(z)){
    graph1[(z[i]+1):z[i+1],] <- (graphMat[(z[i]+1):z[i+1],c(2,1)]*-1)
    i <- i+2
  }
  return(graph1)
}


renameNodes <- function(node){
  v <- unlist(strsplit(node, '|', fixed=TRUE))
  w <- paste(v[1],v[2],sep='.')
  return(w)
}

contigGraphNEL <- function(json, directed = FALSE){
  n1 <- sapply(json$edges, function(x) x$n1)
  n2 <- sapply(json$edges, function(x) x$n2)
  n1.1 <- sapply(n1, renameNodes)
  n2.1 <- sapply(n2, renameNodes)
  edgeMat <- cbind(n1.1, n2.1)
  if(directed){
    g <- ftM2graphNEL(edgeMat, edgemode='directed')
  }
  else{
    g <- ftM2graphNEL(edgeMat, edgemode='undirected')
  }
  return(g)

}

chrome.subG <- function(diCC, diG){
  ##function that rfeturns the contig subgraphs
  ##our purpose is directed and linear
  if(!is.list(diCC)){
    v <- vector(length=1, mode="list")
    v[[1]] <- diCC
  }

  sg <- lapply(diCC, function(x) subGraph(x, diG))
  sg
}

chunk.path <- function(sg){
  ##function to find the source node and
  ##then order the nodes in a linear way
  d <- degree(sg)
  n <- names(which(d$inDegree==0))
  if(length(n)>1){stop("Graph has more than one start node")}
  p <- dag.sp(sg, n)
  c.p <- names(sort(p$distance))
  return(c.p)
}

est.chrome <- function(path){
  ##function to estimate which conting (here Chrome)
  ##is dominant. i.e. you might have Chr4 for all
  ## but a few chunks so the dominant is Chr4
  ch0 <- strsplit(path, '.', fixed=TRUE)
  ch1 <- sapply(ch0, function(x) x[1])
  tab <- table(ch1)
  n <- names(which(tab == max(tab)))
  if(length(n)>1){
    chr <- grep('Chr', n)
    if(length(chr)==0){
      chr <- grep('chloroplast',n)
    }
    if(length(chr)==0){
      chr <- grep('BD',n)
    }
    n <- n[chr[1]]
  }

  ind <- which(ch1 == n)

  ch2 <- sapply(ch0, function(x) x[length(x)])
  ch3 <- sapply(sapply(ch2, function(x)
                       {strsplit(x, "$", fixed=TRUE)}),
                function(y) y[1])
  ch4 <- as.numeric(unlist(sapply(ch3, function(x)
                                  {strsplit(x, "_Dup_.")})))


  if(ch4[length(ind)]<ch4[ind[1]]){
    path1 <- path[(length(path):1)]
    names(path1) <- rep(n, length(path1))
    return(path1)
  }

  else{
    names(path) <- rep(n, length(path))
    return(path)
  }

}

order.contigs <- function(chromeList){
  first <- sapply(chromeList, function(x) x[1])
  first.num <- as.numeric(sapply(strsplit(first, '.', fixed=TRUE),
                                 function(x) x[length(x)]))
  o <- order(first.num)
  o
}


chrome.restrict <- function(chromeList, chr='Chr1$'){
  ##silly wrapper function for grep in the way that
  ##i need to use it
  n <- grep(chr, names(chromeList))
  n
}

gene.intersect <- function(geneTable, featureTable, chr){
  sub <- which(geneTable[,"Chrome"] == chr)
  gTable <- geneTable[sub,]
  print(nrow(featureTable))
  geneFeatureList <- vector(length=nrow(featureTable), mode='list')
  for (i in 1:nrow(featureTable)){
    gene.int <- which((gTable[,2] >= featureTable[i,1] &
                       gTable[,2] <= featureTable[i,2]) |
                      (gTable[,3] <= featureTable[i,2] &
                       gTable[,3]>= featureTable[i,1]))
    geneFeatureList[[i]] <- gene.int
  }
  return(geneFeatureList)
}

goodness.fit <- function(k=50, n,  ref="global", rate, ftd, verbose=T, idx=0, sample=0){

  err <- rowSums(ftd[,8:12])
  #print(ref)
  if(ref=="global"){
    pick <- (ftd$Cov == k)
  }
  else{
    pick <- (ftd$Cov == k & ftd$Ref2 == ref)
  }

  #print(sum(pick))
  if(sum(pick)==0){
    return('There is no such combination of k and reference')
  }

  if(idx>0){
    # hack to allow smaller subsample
    pick <- pick & (1:length(cov) <= idx)
  }
  if(sample>0){
    # better subsample hack
    unpicked <- sample(which(pick), sum(pick)-sample)
    pick[unpicked] <- F
  }

  #n = sum(pick)
  #cat("n=",n,"erate=",rate,"\n")

  expected <- c(dbinom(0:3, k, rate), pbinom(3, k, rate,lower.tail=F))*n
  observed <- c(sum(err[pick]==0),sum(err[pick]==1),sum(err[pick]==2),
                sum(err[pick]==3),sum(err[pick]>3))

  x1 <- (observed-expected)/sqrt(expected)
  x2 <- (x1^2)

  print(rbind(expected,observed,x1,x2))
  print(sum(x2))
  ex <- c(expected[1:2],sum(expected[3:5]))
  ob <- c(observed[1:2],sum(observed[3:5]))

  xx1 <- (ob-ex)/sqrt(ex)
  xx2 <- (xx1^2)

  print(rbind(ex,ob,xx1,xx2))
  print(sum(xx2))

  cs <- chisq.test(ob,p=ex/n)
  return(cs)

}

snpCounter <- function(fullTable, regions, chr){
    chrInd <- (fullTable[,1] == chr)
    fullTable.chr <- fullTable[chrInd,]
    counts <- vector('numeric', length=nrow(regions))
    for(i in 1:nrow(regions)){
        counts[i] <- sum(fullTable.chr[regions[i,1]:regions[i,2],"snp"])
    }
    counts.regions <- cbind(counts,regions[,3])
    colnames(counts.regions) <- c('Number of SNPs','Length of region')
    return(counts.regions)
}


threeAlleles <- function(fullTables, counts=5, refCt=5){
    a <- fullTables[,'a'] > counts
    g <- fullTables[,'g'] > counts
    c <- fullTables[,'c'] > counts
    t <- fullTables[,'t'] > counts

    m <- fullTables[, '.match'] > refCt


    nt <- list(a=a, g=g, c=c, t=t)
    tfTab <- do.call(cbind, nt)
    tfTab1 <- tfTab[m,]
    rs <- rowSums(tfTab1)
    return(rs)

}


chrome2genome <- function(chr, tobeMapped, reverse=F){
    chrStarts <- c(1,  3042586,  5749781,  8189833, 10592156, 12898128, 14969608, 16962042,
                   18229240, 19420300, 20525968, 21332110, 21414953, 22543335, 23595531, 24594174,
                   25525442, 25985218, 26154594, 26814518, 27641571, 28248810, 28400487, 28691681,
                   29491915, 30549480, 31004434, 31301783, 31345610, 31474424, 31500794, 31522319,
                   31536713, 31548410, 31567957, 31647613, 31685246, 31736624, 31752873, 31778283,
                   31795661, 31809131, 31909423, 31940157, 31961523, 31976765, 31988566, 31993083,
                   32059743, 32143133, 32146782, 32169968, 32308518, 32329250, 32335654, 32399524,
                   32404243, 32415269, 32425824, 32433914, 32439069, 32509920, 32553378, 32555660,
                   32577073, 32596174)


    names(chrStarts) <- c("Chr1", "Chr2", "Chr3", "Chr4",
                          "Chr5", "Chr6", "Chr7", "Chr8",
                          "Chr9", "Chr10","Chr11a","Chr11b",
                          "Chr12", "Chr13","Chr14","Chr15",
                          "Chr16a","Chr16b","Chr17","Chr18",
                          "Chr19a_19","Chr19b_31","Chr19c_29","Chr20",
                          "Chr22","Chr23","Chr24", "mitochondria.fasta",
                          "chloroplast.fasta","BD1_7","BD2_100","BD3_105",
                          "BD4_130","BD5_160","BD6_46","BD7_47",
                          "BD8_51","BD9_52","BD10_65","BD11_74",
                          "BD12_85","BD13_1","BD14_19","BD15_19",
                          "BD16_19", "BD17_19","BD18_19","BD19_4",
                          "BD20_5","BD21_5","BD22_7","BD23_33",
                          "BD24_33","BD25_33","BD26_41","BD27_41",
                          "BD28_107","BD29_110","BD30_155","BD31_227",
                          "BD32_49","BD33_54","BD34_588","BD35_67",
                          "BD36_69","BD37_91")

    #chrEnds <- c(chrStarts[2:length(chrStarts)]-1, 32610006)
    #names(chrEnds) <- names(chrStarts)

    if(reverse){
      newCoor <- tobeMapped - (chrStarts[chr] - 1)
    #  newCoor <- chrEnds[chr] - (tobeMapped -1)
    }

    else{
        newCoor <- tobeMapped + (chrStarts[chr] - 1)
    }

    return(newCoor)
}

comp.dna <- function(str){
  s <- toupper(str)
  comp <- paste(chartr('ATGC','TACG',
               s ,sep='',collapse=''))
  return(comp)
}


erosion <- function(show=1, chunk = 2000, oversample = 20, mul=FALSE,
                    col="red", title=st.loc(show), exon=T){
  #find desert ends for all followed by >chunk of non-dessert

  z = vector(mode='list', length=7)
  y = vector(mode='list', length=7)
  ##this is new; calculates the length from end of i_th desert to the
  ##start of i_th +1
  inter.des.len <- vector(mode='list', length=7)

  y1 = vector(mode='list', length=7)
  for(i in 1:7){
    x = des[[i]][[1]][(2:nrow(des[[i]][[1]])),1] -
        des[[i]][[1]][(1:(nrow(des[[i]][[1]])-1)),2]
    y[[i]] = x>(2*chunk)
    y1[[i]] <- which(y[[i]])
    ## z will give all the desert ends that abutt a 2*chunk size of
    ##non-desert at the 3' direction
    z[[i]] = des[[i]][[1]][y[[i]],2]
    inter.des.len[[i]] <- (des[[i]][[1]][y1[[i]]+1, 1] -
                           des[[i]][[1]][y1[[i]], 2])
  }

  ##don't want to re-sample any of the 2K chunk post deserts
  ##so

  #y1 <- lapply(y, function(x){which(x)})
  z1 <- lapply(z, function(x){x+chunk})
  sample.scale <- lapply(inter.des.len, function(x){x-2000})
  #non.des.region <- mapply(function(x,y) {unlist(mapply(function(u,v){x:y},x,y))},
  #z1, inter.des.len)

  qq = vector(mode='list', length=7)
  for(j in 1:7){

    v = vector(length=length(z[[j]]))

    for(i in 1:length(v)){
        if(exon){
            v[i] <- sum(full.tables.Chr1[[j]][z[[j]][i]:(z1[[j]][i]), 'snp'],na.rm=T)
        }
        else{
            new.tab <- full.tables.Chr1[[j]][z[[j]][i]:(z1[[j]][i]),]
            v[i] <- sum(new.tab[!new.tab[,'exon'], 'snp'], na.rm=T)
        }
    }

    w <- vector(length=oversample*length(v))

    for(i in 1:length(w)){
        if(F){
            s <- sample(chunk, 1) ####randomly start the comparison chunk between 1B-2KB away
            w[i] <- sum(full.tables.Chr1[[1]][(z[[j]][i]+chunk+s):(z[[j]][i]+chunk+s), 'snp'],
                        na.rm=T)
        }

      else {
        # sample from anywhere in Chr1, not just the same non-desert
        #st <- sample(y1[[j]], 1)
        #interval <- sample(((des[[j]][[1]][st,2]+chunk):(des[[j]][[1]][st+1,1]-chunk)), 1)
          want2sample <- unlist(mapply(function(x,y){rep(x,length(y))}, y1[[j]],
                                       sample.scale[[j]]))
          st <- sample(want2sample, 1)
          interval <- sample(((des[[j]][[1]][st,2]+chunk):(des[[j]][[1]][st+1,1]-chunk)),
                                 1)
          if(exon){
              w[i] <- sum(full.tables.Chr1[[j]][interval:(interval+chunk), 'snp'])
          }

          else{
              new.tab <- full.tables.Chr1[[j]][interval:(interval+chunk),]
              w[i] <- sum(new.tab[!new.tab[,'exon'],'snp'],na.rm=T)
          }
        #s <- sample(nrow(full.tables.Chr1[[1]])-chunk,1)
        #w[i] <- sum(full.tables.Chr1[[1]][(s):(s+chunk), 'snp'])
      }
    }

    u <- apply(matrix(sort(w),nrow=oversample),2,mean) #average the samples in groups of 10

    qq[[j]] <- cbind(sort(v),u)
  }

  # qqplot with dithered y:
  if(!mul){
    chunky <- chunk/1000
    plot((qq[[show]][,1]/5000), (qq[[show]][,2] +
         rnorm(nrow(qq[[show]]), 0,.1))/5000,col='blue',
         xlab=paste('b. Non-exonic SNP density ', chunky, 'k post desert',sep=''),
         #xlab='',
         ylab=paste('Non-exonic SNP density in random ', chunky, 'k region',sep=''),
         main=title)
    abline(0,1)
  }
  else{
    chunky <- chunk/1000
    points((qq[[show]][,1]/5000), (qq[[show]][,2]/5000) +
           rnorm(nrow(qq[[show]]),0,.1),col=col,
          xlab=paste('b. SNPs ', chunky, 'k after desert',sep=''),
           #xlab='',
          ylab=paste('SNPs, random ', chunky, 'k',sep=''))
    abline(0,1)
  }
}

parseQualTxt <- function(qualFile){
    qualIntoR <- readLines(qualFile)
    qual <- sapply(qualIntoR, function(x){strsplit(x, split= " ", fixed=TRUE)})
    names(qual) = NULL
    return(qual)
}

getQual <- function(qualStats){

    nt <- c(substr(qualStats[2], 1, 1), substr(qualStats[3],1,1))
                                        #sapply(qualStats, function(x)
                                        #{return(c(substr(x[2], 1, 1), substr(x[3],1,1)))})
    ql <- c(substr(qualStats[2], 2, 1000000), substr(qualStats[3],2,1000000))
                                        #sapply(qualStats, function(x)
                                        #{return(c(substr(x[2], 2, 1000000),
                                        #substr(x[3],2,1000000)))})
    t <- strsplit(ql, ",", fixed=T)
    print(t)
    t.1 <- strsplit(t[[1]], ":", fixed=T)
    t.2 <- strsplit(t[[2]], ":", fixed=T)
    firstQ.1 <- as.numeric(sapply(t.1, function(x) {x[1]}))
    secQ.1 <- as.numeric(sapply(t.1, function(x) {x[2]}))
    first.Q1.pos <- as.numeric(sapply(t.1, function(x) {x[3]}))

    sec.Q1.pos <- as.numeric(sapply(t.1, function(x) {x[4]}))
    firstQ.2 <- as.numeric(sapply(t.2, function(x) {x[1]}))
    secQ.2 <- as.numeric(sapply(t.2, function(x) {x[2]}))
    first.Q2.pos <- as.numeric(sapply(t.2, function(x) {x[3]}))
    sec.Q2.pos <- as.numeric(sapply(t.2, function(x) {x[4]}))

    qual <- vector(length=3, mode="list")
    qual[[1]] <- nt
    qual[[2]] <- cbind(firstQ.1, secQ.1, first.Q1.pos, sec.Q1.pos)
    qual[[3]] <- cbind(firstQ.2, secQ.2, first.Q2.pos, sec.Q2.pos)

    qual

}

nonrefRatio <- function(tab, nrMax=TRUE){
  if(nrMax){
    mx <- apply(tab[ , c('a','g','c','t')], 1, max)
    r <- (mx / (mx + tab[,'.match']))
  }

  else{
    nr <- rowSums(tab[,c('a','g','c','t')])
    r <- (nr / (nr + tab[,'.match']))
  }

  return(r)
}

diQual <- function(qualMat, qualThresh=5, qualThresh2=NULL, returnNum=TRUE, SW=TRUE){

  if(is.null(qualThresh2)){
    qualThresh2 <- qualThresh
  }

  if(SW){

      if(returnNum){
          bad.pos <- sum(apply(qualMat, 1, function(x)
                               {x[1] < qualThresh && x[2]< qualThresh2}))
      }
      else{
          bad.pos <- apply(qualMat, 1, function(x)
                           {x[1] < qualThresh && x[2]< qualThresh2})
      }

        return(bad.pos)
  }

  else{
      if(returnNum){
          good.pos <- sum(apply(qualMat, 1, function(x)
                                {x[1] > qualThresh && x[2] > qualThresh2}))
      }
      else{
          good.pos <- apply(qualMat, 1, function(x)
                            {x[1] > qualThresh && x[2] > qualThresh2})
      }

      return(good.pos)
  }



}

qual.filt.cov <- function(covList, ref.base){

    ##is there trivial coverage

    na.tot <- sum(is.na(covList[[1]]))
    ind <- match(ref.base, covList[[1]])
    #print(ind)
    if(na.tot > 1){
        ref.cov <- 0
        alt.cov <- 0
    }

    if(na.tot == 1){

        if(is.na(ind)){
            alt.cov <- covList[[2]]
            ref.cov <- 0
        }


        else{
            ref.cov <- covList[[2]]
            alt.cov <- 0
        }

    }

    if(na.tot == 0){
        #print('do i get here')
        if(is.na(ind)){
            ref.cov <- 0
            alt.cov <- covList[[2]]+covList[[3]]
        }
        else{
            if(ind == 1){
                ref.cov <- covList[[2]]
                alt.cov <- covList[[3]]
            }
            if(ind == 2){
                ref.cov <- covList[[3]]
                alt.cov <- covList[[2]]
            }
        }
    }

    cov <- c(ref.cov, alt.cov)
    names(cov) <- c('ref','nr')
    return(cov)
}
