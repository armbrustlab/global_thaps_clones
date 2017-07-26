source('../../../../R/allFunctions.R')
source('../../../../R/wlr.R')

tabs <- c('Tp1007-Q50.tab','Tp1012-Q50.tab','Tp1013-Q50.tab',
          'Tp1014-Q50.tab','Tp1015-Q50.tab','Tp3369-Q50.tab',
          'Tp1335-Q50.tab')
fnames <- c('Tp1007-','Tp1012-','Tp1013-',
          'Tp1014-','Tp1015-','Tp3369-',
          'Tp1335-')

for(i in 1:7){
    tab <- read.table(tabs[i], header=T)
    png(paste(fnames[i], 'full.png', sep=''), 960, 960)
    show.allele.scatter2(cov.tab=tab, show.main.ttl=F,
                         hist.bins=45, scatter=F, mask=T,
                         strain=i, show.ylab=F, ratlab='',
                         one.grey=T)
    dev.off()
}
