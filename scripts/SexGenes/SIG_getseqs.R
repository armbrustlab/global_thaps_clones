rm(list=ls(all=TRUE)) # clears workspace of ALL variables etc, use with caution

library(seqinr)

load("/Users/koesterj/Dropbox/julie/Tps7_2_08_2013/SexGenes/full.table.sig3.9724.rda")

## list of 7 dataframes with order 1007, 1012, 1013, 1014, 1015, 3367, 1335
t<- full.table.sig3.9724

## get the reference seq
ref <- paste(subset(t[[1]][,'Ref'], t[[1]][,'exon']==TRUE), collapse = '')
ref

## which strains have how many SNPs
sapply(t, function(x){sum(x[,'snp'])})


## which strains have how many SNPs in exons
sapply(t, function(x){sum(x[,'snp'] & x[,'exon']==TRUE)})

#what are the gene coordinates for SNPs in exons
lapply(t, function(x){which(x[,'snp']==1 & x[,'exon']==TRUE)})


## how long is the gene region
length(t[[1]][,'Ref'])

## how long is coding seq
sub <- subset(t[[1]][,'Ref'],t[[1]][,'exon']==TRUE)
length(sub)

## how many aa are there
aa <- length(sub)/3
aa

## what is the average coverage for each strain
sapply(t, function(x) {sum(x[,'Cov'])/(length(sub))})

## copy alternate base into refseq and place in new list of dataframes
t2 <- lapply(t, function(x) {
    within(x, Ref <- ifelse(snp==1 & exon==TRUE & a/Cov > 0.1 & a > 0, 'a',
    ifelse(snp==1 & exon==TRUE & g/Cov > 0.1 & g > 0, 'g',
    ifelse(snp==1 & exon==TRUE & c/Cov > 0.1 & c > 0, 'c',
    ifelse(snp==1 & exon==TRUE & t/Cov > 0.1 & t > 0, 't', Ref)))))
})


## double check that I changed the correct bases
lapply(t2, function(x){
    grep('[agct]', x[,'Ref'])
})

## list of actual sequences and make fasta
seqs <- lapply(t2, function(x) {
    paste(subset(x[,'Ref'], x[,'exon']==TRUE), collapse = '')
})
seqs

write.fasta(seqs, names = c("1007", "1012", "1013", "1014", "1015", "3367", "1335"), nbchar = 60, file.out = "Tps7_SIG3.9724.fasta", open = "w")
write.fasta(ref, names = "ref", nbchar = 60, file.out = "Tps7_SIG3.9724.fasta", open = "a")
