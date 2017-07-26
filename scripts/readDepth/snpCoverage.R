
#source('../../../R/allFunctions.R')
#load('../../../data/des.rda')
load('full.tables.Chr1.rda')

#non.des.chr1 <- vector(mode='list', length=7)
#for(i in 1:length(non.des.chr1)){
#    x <- des[[i]][[1]]
#    ind <- rep(T, 3042585)
#    for(j in 1:nrow(x)){
#        ind[x[j,1]:x[j,2]] <- F
#    }
#    non.des.chr1[[i]] <- full.tables.Chr1[[i]][ind,]
#}
strains <- c(1335, 3369, 1007, 1012, 1013, 1014, 1015)

strain.locations <- c('NY', 'Italy', 'Virginia', 'W. Australia', 'Wales',
                      'N. Pacific Gyre', 'Puget Sound')
st.loc <- function(index){
        x <- names(full.tables.Chr1)[index]
        for(i in 1:length(strains)){
                if(x == paste('tp',strains[i],sep='')){
                        return(paste(x, ' (', strain.locations[i], ')', sep=''))
                }
        }
        return(paste(x, '(?)'))
}

dipwin <- 100
dipc <- vector(mode='list', length=7)
dipm <- vector(mode='list', length=7)
dipn <- vector(mode='numeric', length=7)
for(st in 1:7){
        dipc[[st]] <- vector(mode='numeric',length=2*dipwin+1)
        dipm[[st]] <- vector(mode='numeric',length=2*dipwin+1)
        dipn[st] <- 0
        for(i in (1+dipwin+1):(3042585-dipwin-1)){
                if(full.tables.Chr1[[st]]$snp[i] == 1){
                        dipc[[st]] <- dipc[[st]] + full.tables.Chr1[[st]]$Cov[i+(-dipwin:dipwin)]
                        dipm[[st]] <- dipm[[st]] + full.tables.Chr1[[st]]$.match[i+(-dipwin:dipwin)]
                        dipn[st]  <- dipn[st] + 1
                }
        }
}

pdf('SNPcoverage.pdf',onefile=T)
for(st in 1:7){
        dipma <- dipm[[st]]/dipn[st]
        dipma[dipwin+1] <- 2 * dipma[dipwin+1]
        plot((-dipwin:dipwin),dipc[[st]]/dipn[st], ylim=range(c(dipc[[st]]/dipn[st],dipma)),
          main=paste("Coverage around SNPs",st.loc(st)),xlab="Position wrt SNP",ylab="Coverage")
        points((-dipwin:dipwin),dipma, pch=c(rep('+',dipwin),'*',rep('+',dipwin)))
        legend(ifelse(st==1 || st==4,'topleft','bottomleft'),
          legend=c(paste("=",dipn[st]),'Coverage','Matches','2*Matches @ SNP'),bty='n',pch='No+*')
}
dev.off()


