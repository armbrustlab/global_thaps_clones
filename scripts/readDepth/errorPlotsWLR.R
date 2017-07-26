# look at error rates in 320k desert
# for data Tony sent 2013-08-15

# look up strain location:
strains <- c(1335, 3369, 1007, 1012, 1013, 1014, 1015)
strain.locations <- c('NY', 'Italy', 'Virginia', 'W. Australia', 'Wales', 'N. Pacific Gyre', 'Puget Sound')
st.loc <- function(index){
	x <- names(results)[index]
	for(i in 1:length(strains)){
		if(x == paste('tp',strains[i],sep='')){
			return(paste(x, ' (', strain.locations[i], ')', sep=''))
		}
	}
	return(paste(x, '(?)'))
}


# skip if already loaded (so I can re-source the file)
if(!exists('full.tables.des.plus')){
   	load('ftdp.rda')
}

# loading file defines one var "full.tables.des.plus", which contains a list of 7 data 
# frames, one per strain, containing:
# > str(full.tables.des.plus[[1]]) 
# 'data.frame':	419995 obs. of  12 variables:
#  $ V1/chr    : Factor w/ 66 levels "BD10_65","BD11_74",..: 39 39 39 39 39 39 39  ...
#  $ V2/pos    : int  1376223 1376224 1376225 1376226 1376227 1376228 1376229 ...
#  $ V3/snp    : int  0 0 0 0 0 0 0  ...
#  $ Chr   : chr  "Chr1" "Chr1" "Chr1" "Chr1" ...
#  $ Pos   : int  1376223 1376224 1376225 1376226 1376227 1376228 1376229  ...
#  $ Ref/Ref2   : chr  "T" "C" "C" "C" ...
#  $ ---/Cov   : num  34 33 34 32 36 40 38 37 37 37 ...
#  $ a     : num  0 0 0 0 0 0 0 0 0 0 ...
#  $ g     : num  0 0 0 0 0 0 0 1 1 0 ...
#  $ c     : num  0 0 0 0 0 0 1 0 0 0 ...
#  $ t     : num  0 0 0 0 0 0 0 0 0 0 ...
#  $ n     : num  0 0 0 0 0 0 0 0 0 0 ...
#  $ .match: num  15 26 34 32 36 40 37 36 36 37 ...
# > 
#
# [[names before slashes are as in older full.tables.des; kept in case I need to decypher
#   old code, but hopefully replaced everywhere but comments below...]]
# V1/V2 essentially duplicate Chr/Pos, but former is defined everywhere, while
# later (and Ref) are sometimes NA (positions with 0 positions, I think)
# V3 is 1 if Chris's pipeline called a SNP
# .match is number of reads matching reference in a given position
# agctn are number of reads NOT matching ref in a given position, but instead called
# as agctn, resp. If ref is G, say, then g is necessarily zero.  Apparently, n is always 0.
# V3 and the 6 counts are zero if Chr/Pos/Ref are NA

# data in all strains should start at same place, and have same length, 
# (as follows), namely loc/length of big desert in 1335 plus 100k
desert.start <- 1376223
desert.length <- 319995
nondesert.length <- 100000

# desert.mask is TRUE in desert positions, else FALSE
desert.mask <- rep(c(T,F),c(desert.length, nondesert.length))


# some simple checks: any N's in read data?
if(!exists('check.Ns')){
	if(sum(unlist(lapply(full.tables.des.plus,function(x){sum(x$n)}))) == 0){
		cat("As expected, no N's read.\n") 
	} else {
		cat("*** Unexpected N's! ***\n")
	}
	checkNs <- T
}

for(i in 1:7){
	if(		
	  all(full.tables.des.plus[[i]]$chr == "Chr1") &&
	  all(full.tables.des.plus[[i]]$pos == (desert.start-1+(1:(desert.length+nondesert.length)))) &&
	  all(full.tables.des.plus[[i]]$chr == full.tables.des.plus[[i]]$Chr, na.rm=T) &&
	  all(full.tables.des.plus[[i]]$pos == full.tables.des.plus[[i]]$Pos, na.rm=T) &&
	  all((full.tables.des.plus[[i]]$snp == 0) | (full.tables.des.plus[[i]]$snp == 1)) &&
	  identical(is.na(full.tables.des.plus[[i]]$Chr), is.na(full.tables.des.plus[[i]]$Pos)) &&
	  identical(is.na(full.tables.des.plus[[i]]$Chr), is.na(full.tables.des.plus[[i]]$Ref2))
	  ){
	  	cat("Strain",i, "(", names(full.tables.des.plus)[i], ")", "passes basic sanity checks.\n")
	  } else {
	  	cat("Strain",i,"fails basic sanity checks.\n")
	  }
}

# Another sanity check: same ref in all strains?? 
for(i in 1:(length(full.tables.des.plus)-1)){
	if( ! identical(full.tables.des.plus[[i]]$Ref2, full.tables.des.plus[[i+1]]$Ref2)){
		if(all(full.tables.des.plus[[i]]$Ref2 == full.tables.des.plus[[i+1]]$Ref2, na.rm=T)){
			cat("*** Refs of (", i, i+1, ") identical, except for NAs ***\n")
		} else {
			cat("*** Unexpected non-NA ref difference! (", i, i+1, ") ***\n")
		}
	}
}

# where do we have zero positions?
cat("number of positions with zero positions:\n")
for(i in 1:7) print(sum(is.na(full.tables.des.plus[[i]]$Ref2)))
cat("But many are shared; total distinct positions across all 7 is:\n")
zcov <- is.na(full.tables.des.plus[[1]]$Ref2)
for(i in 2:7){
	zcov <- zcov | is.na(full.tables.des.plus[[i]]$Ref2)
}
print(sum(zcov))

# brute force truncation of all tables to exclude non-desert positions past end
prune <- function(len=319995){
	for(i in 1:length(full.tables.des.plus)){
		full.tables.des.plus[[i]] <<- full.tables.des.plus[[i]][1:len,]
	}
}
#prune()

# Summaries:

# stats in desert:
nt.stats.des <- vector(mode='list', length=7)
nt.rates.des <- vector(mode='list', length=7)
# stats non-desert:
nt.stats.nd <- vector(mode='list', length=7)
nt.rates.nd <- vector(mode='list', length=7)
snps     <- vector(mode='list', length=7)
e.rate   <- vector(mode='numeric', length=7)
gcread   <- vector(mode='numeric', length=7)
gcref    <- vector(mode='numeric', length=7)

# X.counts is # of reads for a reference X that was called AGCTN in
# positions defined by mask (e.g., desert or nondesert)
# TODO: fold in .match ???
make.nt.stats <- function(mask){
	a.counts <- colSums(full.tables.des.plus[[i]][a & mask,c('a','g','c','t','n','.match')],na.rm=T)
	g.counts <- colSums(full.tables.des.plus[[i]][g & mask,c('a','g','c','t','n','.match')],na.rm=T)
	c.counts <- colSums(full.tables.des.plus[[i]][c & mask,c('a','g','c','t','n','.match')],na.rm=T)
	t.counts <- colSums(full.tables.des.plus[[i]][t & mask,c('a','g','c','t','n','.match')],na.rm=T)
	n.counts <- colSums(full.tables.des.plus[[i]][n & mask,c('a','g','c','t','n','.match')],na.rm=T)

	# .counts is a 4x4 table of counts; .rates is same, but fractions of reads, not counts
	nt.counts <- rbind(a.counts, g.counts, c.counts, t.counts)
	nt.rates <- matrix(as.vector(nt.counts)/rep(apply(nt.counts,1,sum),6),4)
	
	# return both counts and rates
	return(list(nt.counts,nt.rates))
}



# per strain summaries
for(i in 1:length(full.tables.des.plus)){
  # mask for reference A's etc.
  a <- (full.tables.des.plus[[i]]$Ref2=='A')
  g <- (full.tables.des.plus[[i]]$Ref2=='G')
  c <- (full.tables.des.plus[[i]]$Ref2=='C')
  t <- (full.tables.des.plus[[i]]$Ref2=='T')
  n <- (full.tables.des.plus[[i]]$Ref2=='N')
  a[is.na(a)] <- F
  g[is.na(g)] <- F
  c[is.na(c)] <- F
  t[is.na(t)] <- F
  n[is.na(n)] <- F
 

  des  <- make.nt.stats(desert.mask)
  ndes <- make.nt.stats(!desert.mask)
  nt.stats.des[[i]] <- des[[1]]
  nt.rates.des[[i]] <- des[[2]]
  
  nt.stats.nd[[i]]  <- ndes[[1]]
  nt.rates.nd[[i]]  <- ndes[[2]]
  
  reads <- sum(full.tables.des.plus[[i]][desert.mask,c('a','g','c','t','n','.match')],na.rm=T)
  errs  <- sum(full.tables.des.plus[[i]][desert.mask,c('a','g','c','t','n')],na.rm=T)
  e.rate[i] <- errs/reads
  gcread[i] <- sum(nt.stats.des[[i]][2:3,6])/sum(nt.stats.des[[i]][1:4,6])  ##suspicious
  gcref[i]  <- (sum(g)+sum(c))/(sum(g)+sum(c)+sum(a)+sum(t))
  
  snps[[i]] <- which(full.tables.des.plus[[i]]$snp==1)
}

names(nt.stats.des) <- names(full.tables.des.plus)
names(nt.rates.des) <- names(full.tables.des.plus)
names(nt.stats.nd) <- names(full.tables.des.plus)
names(nt.rates.nd) <- names(full.tables.des.plus)
names(gcread) <-names(full.tables.des.plus)
names(gcref)  <-names(full.tables.des.plus)
names(e.rate) <-names(full.tables.des.plus)
names(snps)   <-names(full.tables.des.plus)


## ONLY ONE STRAIN:
st <- 7
erra1 <- full.tables.des.plus[[st]]$a[desert.mask]
errc1 <- full.tables.des.plus[[st]]$c[desert.mask]
errg1 <- full.tables.des.plus[[st]]$g[desert.mask]
errt1 <- full.tables.des.plus[[st]]$t[desert.mask]
err1  <- erra1+errc1+errg1+errt1
cvr1  <- err1+full.tables.des.plus[[st]]$.match[desert.mask]

e10<-which(err1>10)

enz1 <- err1>0

maxnonref <- apply(cbind(erra1,errc1,errg1,errt1),1,max)


if(F){########## start ################
### is err rate coverage-dependent?
bynuc <- F
maxcvr <- max(cov,na.rm=T)
err.rate <- vector('numeric',maxcvr)
err.n    <- vector('numeric',maxcvr)
if(bynuc){
	erra.rate <- vector('numeric',maxcvr)
	errg.rate <- vector('numeric',maxcvr)
	errc.rate <- vector('numeric',maxcvr)
	errt.rate <- vector('numeric',maxcvr)
}
for(i in 1:maxcvr){
	err.n[i]    <- i*sum(cvr1==i)                     # how many reads in all coverage i positions
	err.rate[i] <- sum(err[cvr1==i],na.rm=T)/err.n[i] # fraction thereof in error
	if(bynuc){
		erra.rate[i] <- sum(erra[cvr1==i],na.rm=T)/(i*sum(cvr1==i & full.tables.des.plus[[st]]$Ref2[desert.mask]=='A'))
		errg.rate[i] <- sum(errg[cvr1==i],na.rm=T)/(i*sum(cvr1==i & full.tables.des.plus[[st]]$Ref2[desert.mask]=='G'))
		errc.rate[i] <- sum(errc[cvr1==i],na.rm=T)/(i*sum(cvr1==i & full.tables.des.plus[[st]]$Ref2[desert.mask]=='C'))
		errt.rate[i] <- sum(errt[cvr1==i],na.rm=T)/(i*sum(cvr1==i & full.tables.des.plus[[st]]$Ref2[desert.mask]=='T'))
	}
}
err.sig = sqrt(err.rate*(1-err.rate)/err.n)
library(Hmisc)
gz <- err.rate>0
errbar(x=((1:maxcvr)[gz]), y=err.rate[gz], yplus=(err.rate+err.sig)[gz], yminus=(err.rate-err.sig)[gz], ylim=c(0,.01),  errbar.col='gray', cap=.003, lwd=.5, cex=.5, xlab='', ylab='')
title(paste('Error Rate versus Coverage',st.loc(st)), xlab="Coverage", ylab='Error Rate')
abline(h=e.rate[st], col='blue', lwd=.5)
} ############################# end #########################

# visualization: plot total reads (dot), total nonref reads (plus), and 
#   total presumed SNP reads (max of nonref at called SNPs; circle) for a range 
#   of positions (default width 1000) starting at "x" relative to desert start.  
#   Perhaps confusingly, total positions is by default sqrt-transformed; use
#   "transform='identity'" to override
plotachunk <- function(x, width=1000, transform='sqrt', ymax=NA){
	# which transform of positions?  Label accordingly
	xform <- switch(transform, sqrt=sqrt, identity=identity)
	ylab  <- switch(transform, sqrt='sqrt(total)/nonref/SNP reads',
	                           identity='total/nonref/SNP reads')

	# range of positions to plot
	range <- x+(0:(width-1))
	# default ymax is max of 3 data series being plotted
	if(is.na(ymax)){ymax<-max(maxnonref[range], err1[range],xform(cvr1[range]))}

	# plot (transformed) positions
	plot(range, xform(cvr1[range]), pch='.', ylim=c(0,ymax), ylab=ylab)

	# plot lower error counts in lighter grays (gray(0)=black, 1=white)
	mxerr <- max(err1[range])
	points(range,err1[range], pch='+', col=gray(0.99*((1-(err1[range])/mxerr)^2)))

	# this (sloppily) plots all SNPs; graphics (usually) clips out-of-range ones,
	# tho a few may show at the margins
	points(snps[[1]],maxnonref[snps[[1]]],col="blue")
}

#see snps.  all vs desert(-ish); note clumps
plot(snps[[1]],1:length(snps[[1]]))
plot(snps[[1]][1:50],1:50)

#interesting : called SNP with ZERO mismatches
plotachunk(11001)
full.tables.des.plus[[1]][11519 ,]

rates.des<-function(i=1){
	matrix(as.vector(nt.stats.des[[i]])/rep(apply(nt.stats.des[[i]],1,sum),6),4, 
		dimnames=list(c('A', 'G', 'C', 'T'), c('A', 'G', 'C', 'T', 'N', 'MATCH')))
}

gof <- function(k=33, ref='', strain=1, idx=0, subsample=0, verbose=T){
	if(ref == ''){
		pick <- (cvr1 == k)
		rate <- e.rate[strain]
	} else {
		pick <- ((cvr1 == k) & (full.tables.des.plus[[strain]]$Ref2[desert.mask] == ref))
		rate <- 1-(rates.des(strain)[ref,'MATCH'])
	}
	if(idx>0){
		# hack to allow smaller subsample
		pick <- pick & (1:length(cvr1) <= idx) 
		}
	if(subsample>0){
		# better subsample hack
		unpicked <- sample(which(pick), sum(pick)-subsample)
		pick[unpicked] <- F
	}
	
	n <- sum(pick)
	
	expected <- c(dbinom(0:3, k, rate), pbinom(3, k, rate,lower.tail=F))*n
	observed <- c(sum(err1[pick]==0),sum(err1[pick]==1),sum(err1[pick]==2),sum(err1[pick]==3),sum(err1[pick]>3))

	x1 <- (observed-expected)/sqrt(expected)
	x2 <- (x1^2)

	ex <- c(expected[1:2],sum(expected[3:5]))
	ob <- c(observed[1:2],sum(observed[3:5]))

	xx1 <- (ob-ex)/sqrt(ex)
	xx2 <- (xx1^2)

	if(verbose){
		cat("n=",n,"erate=",rate,"\n")
		print(rbind(expected,observed,x1,x2))
		print(sum(x2))
		print(rbind(ex,ob,xx1,xx2))
		print(sum(xx2))
	}

	chisq.test(ob,p=ex/n)
}

gof(33,'A') # has 3510 positions
if(F){
	pv <- unlist(lapply(1:100,function(x){gof(33,subsample=3510,verbose=F)$p.value}))
	summary(pv)
}

plotcov <- function(log=F){
	count <- unlist(lapply(0:max(cvr1),function(x){sum(cvr1==x)}))
	if(log){
		plot(0:(length(count)-1), log10(count), xlab="positions")
	} else {
		plot(0:(length(count)-1), count, xlab="positions")
	}
}

#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+

# results  of chi-square gof tests for independent error model in desert

# data is a list with 7 entries (1 per strain, named by strain)
# each entry is itself a list of several hundred entries, each of which 
# is the return value of a chisq.test for all positions in the desert
# with one specific positions level, and is named by that positions
# level (a char string, not an int)

# load it:
#load("/Users/ruzzo/Documents/u/proj/thaps/2013-08-28/gof.results.rda")
#load("/Users/ruzzo/Documents/u/proj/thaps/2013-09-05/gof.ref.results.rda")

desnp <- T  # TRUE to remove SNP positions from err rate calcs

# make a per-strain list of numeric vectors of several quantities:
ntests     <- list(7) # number of tests (inner list length)
coverage   <- list(7) # how many reads cover position for a given test
maxcvr     <- list(7)
positions  <- list(7) # how many positions with that coverge
exmin      <- list(7) # min of "expected" bins for that test
p.val      <- list(7) # test p.value
stdres0    <- list(7) # stdres[1] (0 errors)
stdres1    <- list(7) # stdres[2] (1 error)
stdres2    <- list(7) # stdres[3] (2 errors)
cvr        <- list(7)
erra       <- list(7) # count mis-reads showing A, etc.
errg       <- list(7)
errc       <- list(7)
errt       <- list(7)
err        <- list(7)
err.count  <- list(7)
err.n      <- list(7)
err.rate   <- list(7)
err.sig    <- list(7)
for(i in 1:7) ntests[[i]] <- length(results[[i]])
for(st in 1:7){
	# per strain, per test
	coverage[[st]]  <- as.numeric(names(results[[st]]))
	positions[[st]] <- vector('numeric', ntests[[st]])
	exmin[[st]]     <- vector('numeric', ntests[[st]]) 
	p.val[[st]]     <- vector('numeric', ntests[[st]]) 
	stdres0[[st]]   <- vector('numeric', ntests[[st]]) 
	stdres1[[st]]   <- vector('numeric', ntests[[st]]) 
	stdres2[[st]]   <- vector('numeric', ntests[[st]])
	for(i in 1:ntests[[st]]){
		positions[[st]][i] <- sum(results[[st]][[i]]$observed) 
		exmin[[st]][i]     <- min(results[[st]][[i]]$expected)
		p.val[[st]][i]     <-     results[[st]][[i]]$p.value
		stdres0[[st]][i]   <-     results[[st]][[i]]$stdres[1]
		stdres1[[st]][i]   <-     results[[st]][[i]]$stdres[2]
		stdres2[[st]][i]   <-     results[[st]][[i]]$stdres[3]
	}

	# per strain per position
	if(desnp){
		nonsnp.mask <- full.tables.des.plus[[st]]$snp == 0
	} else {
		nonsnp.mask <- desert.mask
	}
	erra[[st]] <- full.tables.des.plus[[st]]$a[desert.mask & nonsnp.mask]
	errc[[st]] <- full.tables.des.plus[[st]]$c[desert.mask & nonsnp.mask]
	errg[[st]] <- full.tables.des.plus[[st]]$g[desert.mask & nonsnp.mask]
	errt[[st]] <- full.tables.des.plus[[st]]$t[desert.mask & nonsnp.mask]
	err[[st]]  <- erra[[st]] + errc[[st]] + errg[[st]] + errt[[st]]
	cvr[[st]]  <- err[[st]]  + full.tables.des.plus[[st]]$.match[desert.mask & nonsnp.mask]
	
	# per strain
	maxcvr[[st]] <- max(cvr[[st]],na.rm=T)

	# per coverage level
	err.count[[st]] <- vector('numeric', maxcvr[[st]])
	err.n[[st]]     <- vector('numeric', maxcvr[[st]])
	err.rate[[st]]  <- vector('numeric', maxcvr[[st]])
	err.sig[[st]]   <- vector('numeric', maxcvr[[st]])
	
	for(i in 1:desert.length){
		err.n[[st]][cvr[[st]][i]] <- err.n[[st]][cvr[[st]][i]] + cvr[[st]][i]   # sum reads in coverage i positions
		err.count[[st]][cvr[[st]][i]] <- err.count[[st]][cvr[[st]][i]] + err[[st]][i] # count ones in error
	}
	err.rate[[st]] <- err.count[[st]]/err.n[[st]] # fraction in error
	err.sig[[st]] <- sqrt(err.rate[[st]]*(1-err.rate[[st]])/err.n[[st]])
}

library(Hmisc)  # for errbar

# Simple plot to eyeball test results:
# for a given strain, make a plot with 5 panels.  x-axis is coverage in all.
# panel 1: plots "positions" vs coverage, i.e. how many positions were seen with a given coverage
# panel 2: plots p.value vs coverage -- 0 to 1; 
# panel 3: is same on log scale -- 0 to minus whatever.
# panel 4: "standardized residuals" from chisq.test vs coverage.  This shows three lines (R,G,B) for
#    the 0-, 1-, and 2+-error bins, in rainbow order (R=0, G=1, B=2).  E.g., if the red line is highest at
#    a given coverage, it means that there is an excess of error-free positions at that coverage,
#    compared to expectation under our indp binomial error model. Scale is z-score: # of sigma 
#    above/below expectation.
# panel 5: error rate vs coverage, with std error bars
#
#  * overlaid on panel 1 is (approximate) expected coverage under a simple binomial with same
#    mean as observed data.  (yellow curve)
#  * overlaid on panel 2 is exmin/5 (gray pluses), clipped at 1; anywhere that this point is
#    visible, the p.value is suspect, because we have fewer than 5 expected counts in some bin,
#    so chi-sq is a dicey test.  
#  * Vertical gray lines overlaid in all panels mark approximate border of the suspicious
#    region and p.values where exmin<5 are plotted with red dots.
#
# ylim3 passed to panel 3; allows zooming in on a log-p.value range.
#
look <- function(st, ylim3=NULL, exmincol='gray'){
	opar <- par(no.readonly=T); on.exit(par(opar))
	# fiddle plot sizes:
	nf <- layout(matrix(1:5,5,1), widths=rep(1,5), heights=c(2,1.5,1.5,1.5,2.5)) #layout.show(nf)
	# mark min/max coverage with exmin>5 and plot individual p.values in red/black accordingly:
	chilim <- range(coverage[[st]][exmin[[st]]>5])
	colorflag <- ifelse(exmin[[st]]>5,'black','red') 
	
	# plot position counts vs coverage
	par(mai=c(0.05,01.0,0.65,0.1),omi=c(.1,0,.05,0.2),cex=.8)
	plot(coverage[[st]], log2(positions[[st]]), xaxt='n',  xlab='', ylab = "log2(count)", main=st.loc(st),pch=20)
	abline(v=chilim,col=exmincol)
	# attempt at showing expected coverage under a simple binomial model with same mean:
	read.length <- 24
	total.reads <- sum(coverage[[st]]*positions[[st]])/read.length
	average.coverage <- total.reads*read.length/desert.length
	scale<-1
	lines(log2(dbinom(1:200, round(total.reads/scale), scale*read.length/desert.length)*total.reads),col='yellow')
	
	# plot p.vals & exmin vs coverage
	par(mai=c(.05,01.0,0.05,0.1))
	plot(coverage[[st]], p.val[[st]],xaxt='n', xlab='', ylab="p.value", pch=20, col=colorflag, ylim=c(0,1.09))
	points(coverage[[st]], exmin[[st]]/5, pch='+', col=exmincol)
	abline(v=chilim,col=exmincol)
	
	# plot log(p.val) vs coverage
	par(mai=c(.05,01.0,0.05,0.1))
	plot(coverage[[st]], log10(p.val[[st]]),xaxt='n', xlab= '', ylab="log10(p.value)",pch=20, col=colorflag, ylim=ylim3)
	abline(v=chilim,col=exmincol)
	
	# now plot residuals; omit 1st few bins - they are often outliers
	par(mai=c(.05,01.0,0.05,0.1))
	res.range <-range(stdres0[[st]][8:ntests[[st]]], stdres1[[st]][8:ntests[[st]]], stdres2[[st]][8:ntests[[st]]], na.rm=T)
	plot (coverage[[st]], stdres0[[st]], ylim=res.range, col='red', type='l', xaxt='n', xlab= '', ylab='Std Residuals')
	lines(coverage[[st]], stdres1[[st]], ylim=res.range, col='green')
	lines(coverage[[st]], stdres2[[st]], ylim=res.range, col='blue')
	abline(h=0,lwd=.5)
	abline(v=chilim,col=exmincol)

	# finally, error rate vs coverage; is err rate coverage-dependent?
	par(mai=c(.8,01.0,0.05,0.1))
	gz <- rep(T,maxcvr[[st]])  ##err.rate[[st]]>0
	errbar(x=((1:maxcvr[[st]])[gz]), 
		y      =  err.rate[[st]][gz], 
		yplus  = (err.rate[[st]] + err.sig[[st]])[gz], 
		yminus = (err.rate[[st]] - err.sig[[st]])[gz], 
		ylim=c(0,.01),  errbar.col='gray', cap=.003, lwd=.5, cex=.5, xlab='', ylab='')
	title(#paste('Error Rate versus Coverage',st.loc(st)), 	
		xlab="Coverage", ylab='Error Rate')
	text(400,.006,paste("de-SNP",desnp))
	abline(h=e.rate[st], col='blue', lwd=.5)
	abline(v=chilim,col=exmincol)
}
look(7)

look.all <-function(sts=1:7){
	opar <- par(no.readonly=T, ask=T); on.exit(par(opar))
	for(st in sts) {look(st)}
}

# quick dump of the i-th test result from strain st.
showtest <- function(st, i){
	cat(st.loc(st), "\nCoverage =",names(results[[st]])[i], "\n")
	df <- data.frame(
	  observed   =results[[st]][[i]]$observed,
	  expected   =results[[st]][[i]]$expected,
	  stdres     =results[[st]][[i]]$stdres,
	  residuals  =results[[st]][[i]]$residuals,
	  res.squared=results[[st]][[i]]$residuals^2
	  )
	# kludge in summary statistic (sum(res.sq)) as last row under res.sq:
	df <- rbind(df,c(sum(results[[st]][[i]]$observed),sum(results[[st]][[i]]$expected),NA,NA,results[[st]][[i]]$statistic))
	rownames(df) <- c(0,1,2,'tot:')
	print(df)
	#cat("stat:                                       ", results[[st]][[i]]$statistic, '\n')
	cat("(df:", results[[st]][[i]]$parameter, ')')
	cat(", P.VAL:", results[[st]][[i]]$p.value)
	cat("\n")
}
showtest(2,38)
