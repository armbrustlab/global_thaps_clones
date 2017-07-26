
#source('../../../R/allFunctions.R')
vcfdir <- '/share/projects/Thaps-strain-genomes/polymorphism/vcf_20111027_noBAQ_qualfilt/'
indel <- c('Tp1007.indels.vcf','Tp1012.indels.vcf','Tp1013.indels.vcf','Tp1014.indels.vcf',
	'Tp1015.indels.vcf','IT.indels.vcf','Tp1335.indels.vcf')
idfiles <- paste(vcfdir, indel, sep='')
#gff <- '/share/projects/Thaps-strain-genomes/gff/thaps3.models.extended.gff'

#####Getting global indices for indels and exons

##Indels first

id <- lapply(idfiles, read.table)
id.global.pos <- lapply(id, function(x){
  mapply(function(y,z){chrome2genome(y,z)},x[,1],x[,2])
})
save(id.global.pos, file='id.global.pos.rda')
