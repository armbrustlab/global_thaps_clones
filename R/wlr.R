# -*- fill-column: 120; -*-

###############################################################################################
svn.stamp.wlr.r <- function(){
  #
  # svn version stamp
  #
	return('SVN Id, I miss you.  $Id: wlr.R  2017-07-21 or later $')
}

who.where <- function(){
  #
  # who am I? where am I?
  #
  return(paste('Running as:', system2('whoami',stdout=T),'@', system2('hostname',stdout=T)))
}
cat(who.where(), svn.stamp.wlr.r(), sep='; ')  # report both whenever this file is sourced

###############################################################################################
#
# my common knitr/.rnw/boilerplate setup
#

setup.my.knitr <- function(figpath='figs-knitr/'){
  #
  # Set global knitr/rnw options for compiling one of larrys .rnw's.
  #
  if(exists('opts_knit')){
    # If knitr is loaded, set some of its options.  
    # (Skip if not loaded, e.g., when I am playing in Rstudio.)
    opts_chunk$set(comment='#',size='footnotesize')
    opts_knit$set(comment='#', width=85)
    if(!is.na(figpath)){
      opts_chunk$set(fig.path=figpath)
    }
  }
  # width above seems to have little or no effect, but width below affects print(), etc.
  options(width=98)
  getOption('scipen')
  options(scipen=3)  ## bias towards integer vs scientific notation for snp counts etc.
}

generic.setup <- function(figdir='figs', figs=T, DEBUG=F){
  #
  # do some setup common in my rnw files, e.g. create figs dir,...
  # (poorly named function; perhaps rename it or merge with setup.my.*)
  #
  if(DEBUG){cat('generic.setup:', figdir, figs, file.exists(figdir), getwd(), '\n')}
  if(figs && !file.exists(figdir)){
    status <- dir.create(figdir, mode='0700')
    cat('Created dir', figdir, ' with status',status, '\n')
  }
}

setup.my.wd <- function(this.dir    = basename(getwd()),
                        path.in.git = 'Thaps_7_strains/scripts/larrys',
                        path.to.git = NA)
{ 
  # Set the current working directory for compiling one of larrys .rnw's.
  # 
  # Used in most of my .rnw files, all located in dirs within 'path.in.git'.  When .rnw is 
  # compiled from Makefile, wd will have been set (correctly) to the dir containing the .rnw, in 
  # which case default params above are fine.  However, when debugging in Rstudio, wd can be 
  # somewhat random, so calling this fn with 'this.dir' set to the name of the dir containing the 
  # .rnw ensures that related files, e.g., knitr cache, figs and summaries, and can be read 
  # from/written to the correct location.  E.g., in 
  #
  #   'Thaps_7_strains/scripts/larrys/paperfigs/ed3.rnw'
  #
  # set 'this.dir' to 'paperfigs', i.e. call 'setup.my.wd("paperfigs")'.
  # 
  # 'path.to.git' should be the abs path to the dir *containing* the root of the git working dir. 
  # This is system-dependant; if set to NA (the default), code below tries to guess it, but can
  # be explicitly given to override this.
  
  if(is.na(path.to.git)){
    if(system('whoami',intern=T) == 'ruzzo'){
      # Larry works here, both on laptop and CSE servers
      path.to.git <- paste(Sys.getenv('HOME'), '/Documents/g/projects/thaps', sep='')
    } else if(system('whoami',intern=T) == 'chiang'){
      cat('*** ERROR: Tony, update wlr.r::setup.my.wd to locate your git working dir. ***\n')
      # Tony works here
      path.to.git <- '/where/is/Tony.s_git/??/'                     
    } else {
      cat('*** ERROR: update wlr.r::setup.my.wd to locate your git working dir. ***\n')
      path.to.git <- '/where/is/your_git_wd/??/'                     
    }
  }
  mywd <- paste( path.to.git, path.in.git, this.dir, sep='/' )
  if(getwd() != mywd){
    cat('*****\nResetting working dir:\nsetwd(\'', mywd, '\')\n*****\n')
    setwd(mywd)
  }
}

pick.params <- function(mac=NULL,
                        linux=NULL,
                        default=list(load.tb=c(F,F,F,F), pri=1:4, clear.cache=F, nboot=0, trunc.tables=F)){
  # 
  # Pick params for this run, based on command line and where we are running.
  # 
  # See, e.g., shared-snps.rnw for param interpretation.
  # 
  # For convenience in Makefile, params provided as command line args override args to this 
  # function. E.g.  'R --no-save ... --args "c(F,F,F,F),1:4,F,0"' is equiv to do-nothing default 
  # above.
  # 
  # Otherwise, mac or linux arg is chosen based on home test.  (This works for me; Tony may need to 
  # tweak it.)
  # 
  # Params load.tb, pri, clear.cache, nboot, trunc need not be named, but are assumed to be in that
  # order; additional named params may be added after those 5.
  # 
  if(length(commandArgs(TRUE)) > 0){
    params <- eval(parse(text=paste('list(', commandArgs(TRUE)[1], ')')))
  } else {
    params <- default
    home <- substr(system('echo ~',intern=T),2,6) # '/Users/...' on my mac; '/homes/...' on linux
    if(home=='Users' && ! is.null(mac)){
      params <- mac
    } else {
      if(home=='homes' && ! is.null(linux)){
        params <- linux
      }
    }
  }
  # if there are unnamed params, name them (order matters)
  if(is.null(names(params))){
    names(params) <- rep('', length(params))
  }
  # fill omitted names  (order matters)
  for(i in 1:length(params)){
    if(names(params)[i]==''){
      names(params)[i] <- c('load.tb', 'pri', 'clear.cache', 'nboot', 'trunc.tables')[i]
    }
  }
  # for clarity, name the table-loading Bools
  names(params$load.tb) <- c('full.unf', 'chr1.unf', 'full.qf',  'chr1.qf')
  return(params)
}

decache <- function(clear.knitr.cache=FALSE){
  #
  # knitr cache: optionally clear (actually, rename) it, for compiling one of larrys .rnw's.
  #
  if(clear.knitr.cache){
    if(!file.exists('cache')){
      cat('No cache to remove.\n')
    } else {
      newname <- paste('cache', system('echo $$',intern=TRUE),sep='')
      rc <- file.rename('cache', newname)
      cat('Rename of \'cache\' to', paste('\'', newname, '\'',sep=''), 'returned', rc, '.\n')
      # perhaps I should also remove stuff in 00common/mycache?
    }
  } else {
    if(!file.exists('cache')){
      cat('No cache.\n')
    } else {
      cat('Cache exists, and was left alone.\n')
    }
  }
}

tset.picker <- function(priority=1:4, table.set=tset){
  # if multiple tables are loaded, pick first non-NULL in specified priority order;
  # default 1:4 prioritizes  unq- over qfiltered, and full over chr1, if there's a choice. 
  for(i in priority){
    if(!is.null(table.set[[i]])){
      return(table.set[[i]])
    }
  }
  return(NULL)
}

genome.length.constants <- function(){
  #
  # Some genome length parameters that we use moderately often.
  #
  # These are usually easy to infer from the tables, but convenient to centrally define them.
  #
  return(list(genome.length.full  = 32610006,   # full length of the reference genome
              genome.length.trunc = 31301783-1, # after truncation at mito: no mito, plastid, BD_
              chr1.length         =  3042585    # length of Chr1 only
  ))
}

which.snp.tables <- function(tables=snp.tables, string.val=TRUE){
  # return summary of which tables, either as a char string (default), e.g. "Chr1-qfiltered",
  # or as vector of 2 strings, e.g. c("full","unfiltered").
  if(is.null(tables)){
    return('NULL')
  } else {
    g.consts <- genome.length.constants()
    wst <- 'UNKNOWN'
    if(nrow(tables[[1]]) == g.consts$chr1.length        ){wst <- 'Chr1'}
    if(nrow(tables[[1]]) == g.consts$genome.length.full ){wst <- 'full'}
    if(nrow(tables[[1]]) == g.consts$genome.length.trunc){wst <- 'trunc'}
    if('rawCov' %in% names(tables[[1]])){
      # by a quirk of table construction, this name is defined only in qfiltered tables.
      filt <- 'qfiltered'
    } else {
      filt <- 'unfiltered'
    }
    if(string.val){
      return(paste(wst, filt, sep='-'))
    } else {
      return(c(wst,filt))
    }
  }
}

#
# end of knitr/rnw/boilerplate stuff
#
###############################################################################################
  
###############################################################################################
# load & return either full snp tables, or just chr1 subset
# by default:
#   - this should be run in a dir directly inside .../7_strains/trunk/code/snpNB/scripts/larrys/
#   - full tables are found in .../7_strains/trunk/code/snpNB/data
#   - Larry often just wants to prototype on chr 1; extract that and cache it by
#     default, else return full tables
# assumes 'data.name'.rda, when loaded, creates a var of the same name
#
# QFILTERED CODE added circa 9/2015; little changed since then.
#
load.snp.tables <- function(use.chr1.tables = (system('whoami',intern=T) == 'ruzzo') ,
                            write.cache = TRUE,
                            cache.path  = '../00common/mycache/',
                            data.path   = '../../../data/ungit-data/',
                            data.name   = 'full.tables.01.26.14',
                            DEBUG = FALSE
) {
  qfiltered <- (data.name == 'full.tables.02.25.15')
  if(qfiltered){
    cache.chr1 <- paste(cache.path, 'snp.tables.chr1.qfiltered.rda', sep='')
  } else {
    cache.chr1 <- paste(cache.path, 'snp.tables.chr1.unqfiltered.rda', sep='')
  }
  # load full tables if we want them, or if chr1 cache needs to be rebuilt
  if(!use.chr1.tables || !file.exists(cache.chr1)){
    big.path <- paste(data.path, data.name, '.rda', sep='')
    cat('Loading full tables from', big.path, '...')
    if(DEBUG){
      # block unnecessary load (takes 5 minutes) until debugged
      cat('\nDEBUG: NOT loading big tables.\n',
          use.chr1.tables, write.cache, cache.path, data.path, data.name,
          cache.chr1, file.exists(cache.chr1), big.path, chrs.only,'\n')
    } else {
      load(big.path)
      cat('Loaded.\n')
      the.big.table <- get(data.name)
      if(is.null(names(the.big.table))){
        # make sure we've attached strain IDs
        names(the.big.table) <- c('1007','1012','1013','1014','1015','3367','1335')
      }
      # build/cache chr 1 subset as needed
      if(write.cache || use.chr1.tables){
        if(FALSE){
          # code in this branch seems more general, but IT BREAKS in qfiltered tables
          # since Chr is sometimes NA, which seems like a bad idea, but ...
          if('chr' %in% names(the.big.table[[1]])){
            # early (pre-qfiltered) tables have a char field named chr and a factor Chr; use char
            snp.tables.chr1 <- lapply(the.big.table, function(x){x[x$chr=='Chr1',]})
          } else {
            # post-qfiltered tables just have a char field named Chr; use it
            snp.tables.chr1 <- lapply(the.big.table, function(x){x[x$Chr=='Chr1',]})
          }
        } else {
          # Instead, HARD-CODED CHR1 LENGTH is used
          chr1.len <- 3042585
          snp.tables.chr1 <- lapply(the.big.table, function(x){x[1:chr1.len,]})
        }
        if(write.cache){
          if(!file.exists(cache.path)){dir.create(cache.path,mode='0700',recursive=T)} #make dir if needed
          # uncompressed is 10% faster, but 10x larger:
          save(snp.tables.chr1, file=cache.chr1, compress=TRUE)
          cat(cache.chr1, 'saved.\n')
        }
      }
    }
  } else {
    cat('Loading', cache.chr1, '...')
    load(cache.chr1)
    cat('Loaded.\n')
  }
  # return requested table set
  if(use.chr1.tables){
    if(qfiltered){
      return(qfilter.bandaid(snp.tables.chr1))
    } else {
      return(snp.tables.chr1)
    }
  }
  if(qfiltered){
    return(qfilter.bandaid(the.big.table))
  } else {
    return(the.big.table)
  }
}

#############################################################################################
#
# qfilter.bandaid
#
# quality-filtered tables do not look like unfiltered tables; 
# patch them so there is a hope that old code will still work.
#
# UNREPAIRED: 
#   1. order of columns in the data frames are changed,
#      so let's hope we aren't accessing them by index.
#   2. probably other things...
#
qfilter.bandaid <- function(snp.tables.qfiltered){
  cat('Bandaiding qfiltered tables...')
  for(i in 1:7){
    # qfilt only has $Chr; some old code expects $chr; Ditto $Pos/pos
    # ALSO UNREPAIRED: IIRC, one of chr/Chr is a factor, other a char, and perhaps one 
    # could be NA when the other wasn't, but I'm not duplicating that 
    snp.tables.qfiltered[[i]] <- cbind(snp.tables.qfiltered[[i]],
                                            chr=snp.tables.qfiltered[[i]]$Chr, 
                                            pos=snp.tables.qfiltered[[i]]$Pos)
  }
  # recalculate coverage; $Cov in the table is the unfiltered value
  for(i in 1:7){
    snp.tables.qfiltered[[i]]$rawCov <- snp.tables.qfiltered[[i]]$Cov
    snp.tables.qfiltered[[i]]$Cov <- 
      snp.tables.qfiltered[[i]]$a +
      snp.tables.qfiltered[[i]]$g +
      snp.tables.qfiltered[[i]]$c +
      snp.tables.qfiltered[[i]]$t +
      snp.tables.qfiltered[[i]]$.match
  }
  cat('\n')
  return(snp.tables.qfiltered)
}

###############################################################################################
#
# 'cachet' -- A quick and dirty way to cache results of slow computations.
#
# Usage: cachet('goo', bar(baz)+bad [, cache.path='../bee/bim/bop/'])
#
# If '../bee/bim/bop/goo.rda' :
#
#  - does NOT exist, 'goo <<- eval( bar(baz)+bad )', and save goo as 'bee/bim/bop/goo.rda'.
#
#  - DOES exist, load it, WITHOUT evaluating 'bar(baz)+bad,' (thanks to R's lazy eval semantics).
#
# In either case goo is reset, OVERRIDING any existing binding.
#
# Manually delete .rda files to force rebuild.
#
# 'goo' binding is in '.GlobalEnv', aka 'user's workspace'.  Unclear whether this is good or bad,
# but easy and seems adequate.
#
# Advantage vs knitr cache is that I control it all, and especially that results are available
# both in knitr batch and in R/Rstudio interactive.  Disadvantages include single-variable model
# and manual control;
#
# Note 1: quotes around 'foo' -- it's a char string naming a variable, not a bare variable name.
#
# Note 2: cache.path is relative to getwd(); default is for SNPdip, etc., in thaps git
#
# Note 3: varname must be UNIQUE in ../00common/mycache/
#
cachet <- function(varname, expr, cache.path = '../00common/mycache/'){
  filename <- paste(cache.path, '/', varname, '.rda', sep='')
  if(!file.exists(filename)){
    cat('Evaluating... ')
    val <- expr						# force evaluation
    assign(varname, val, envir = .GlobalEnv)		# bind to varname, globally
    cat('Saving', varname, '\n')
    if(!file.exists(cache.path)){dir.create(cache.path,mode='0700',recursive=T)} #make dir if needed
    save(list=varname, file=filename)			# and cache it
  } else {
    cat('Loading...', load(filename, .GlobalEnv))	# reload from cache
  }
}

###############################################################################################
###############################################################################################
# Make a dir with today's date to hold output files.
#   (dir.create fails with warning msg if dir exists, so really no problem doing this
#    repeatedly, but setting/checking the global var avoids unnecessary sys calls & chatter.)
todaydir <- function(){
	if(!exists('TODAYOPATH')){
		# Sys.time() => '2013-11-03 16:26:32 PST', e.g.:
		TODAYOPATH <<- paste(OPATH, substr(Sys.time(),1,10), '/',sep='')
		dir.create(TODAYOPATH, mode='0700')
	}
	return(TODAYOPATH)
}
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
# return a mask with n random positions from strain st selected
# (since all strains have same coords, st shouldn't matter, but included in case we change)
rmask <- function(n,snp.tables=full.tables.01.26.14,strain=1){
	len <- nrow(snp.tables[[strain]])
	msk <- vector('logical', len)
	msk[sample(len,n)] <- T
	return(msk)
}
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
#
# look up strain ccmp name, geographical location, and/or age, based on index(1:7).
#
# Normally, only the index argument need be supplied.  namelist=names(full.tables), if
# supplied, defines the order of the strains in the table, hence the meaning of
# index.  If absent, the 'usual' order is assumed (numerical except for Italy).
# Parallel vectors 'strains,' 'strain.locations' and 'strain.dates' give the
# correspondence between strain numbers, places and date the CCMP culture was
# established; order of entries in these is irrelevant, other than that they are
# parallel.
# Return CCMP id, geographical loc and/or date, if id=T, loc=T and/or date=T;
# kind of a kludge to include here, but somewhat handy to keep strain info together.
#
# NOTE: an earlier version only returned date if date=T, so not backward compatible.
#
# 5/26/17: Italy changed to 2007 from 2006, per Julie email 3/7/17.
#
st.loc <- function(
  index,
  strains=         c(1335,          3367,       1007,           1012,    1013,              1014,          1015),
  strain.locations=c('New York', 'Italy', 'Virginia', 'W. Australia', 'Wales', 'N. Pacific Gyre', 'Puget Sound'),
  strain.locabbrv =c('NY',          'IT',       'VA',          'AUS',    'UK',             'NPG',          'WA'),
  strain.dates=    c(1958,          2007,       1964,           1965,    1973,              1971,          1985),
  namelist=NULL,
  id=T,
  loc=T,
  locabbrv=F,
  date=F
){
  if(is.null(namelist)){
    # if table is unnamed, assume the usual
    namelist <- c('CCMP1007','CCMP1012','CCMP1013','CCMP1014','CCMP1015','CCMP3367','CCMP1335')
  }
  x <- namelist[index]
  for(i in 1:length(strains)){
    if(x == paste('CCMP',strains[i],sep='')){
      paren <- id && (loc || locabbrv || date)
      out <- character(8)
      if(id)       { out[1] <- x                   }
      if(paren)    { out[2] <- ' ('                }
      if(loc)      { out[3] <- strain.locations[i] }
                   { out[4] <- ','                 }
      if(locabbrv) { out[5] <- strain.locabbrv[i]  }
                   { out[6] <- ','                 }
      if(date)     { out[7] <- strain.dates[i]     }
      if(paren)    { out[8] <- ')'                 }
      st.str <- paste(out,sep='',collapse='')
      # if some omitted, remove superfluous commas
      st.str <- gsub('^,,*', '',    st.str)
      st.str <- gsub(',,*$', '',    st.str)
      st.str <- gsub('\\(,,*', '(', st.str)
      st.str <- gsub(',,*\\)', ')', st.str)
      st.str <- gsub(',,*', ', ',   st.str)
      return(st.str)
    }
  }
  return(paste(x, '(?)'))
}

# wrapper for above to map vector of indices to vector of answers
st.locs <- function(indices,...){
  locs <- character(length(indices))
  for(i in indices){
    locs[i] <- st.loc(i,...)
  }
  return(locs)
}
###############################################################################################
###############################################################################################

# return vector y, same length as x, s.t. y[i] = sum(x[(i-win):(i+win)]) (out-of-bounds x's assumed 0)
# not carefully vetted with negative x's, but probably ok.
window.sum <- function(x, win, na.rm=T){
  if(any(is.na(x))){
    if(na.rm){
      x[is.na(x)] <- 0  ## call-by-value semantics; this is NOT changing argument
    } else {
      # replace by zero, cumsum, replace 2*win+1 by NA, or:
      cat('window.sum(...,na.rm=F) unimplemented.\n')
      return(NULL)
    }
  }
  n <- length(x)
  x.cs <- cumsum(x)
  # hi[i] = x.cs[i+win]; low[i] = x.cs[i-win-1]
  hi <- c(x.cs[-(1:win)], rep(x.cs[n], win))
  lo <- c(rep(0, win+1), x.cs[-((n-win):n)])
  #print(x);print(x.cs);print(hi);print(lo) # debug
  return(hi-lo)
}

# analogous to built-in ``table'' but simpler.  Count entries in an integer
# vector sharing values in a (smallish) range.  Result is a 2-column matrix with
# the shared values in col 1 and count of occurrences of that value in col 2.
# Out-of-range values cause subscript error.
mytable <- function(vec, therange=range(vec,na.rm=T)){
  counts <- matrix(0,nrow=therange[2]-therange[1]+1,ncol=2,dimnames=list(NULL,c('val','count')))
  counts[1:nrow(counts),1] <- therange[1]:therange[2]
  for(i in 1:length(vec)){
    if(!is.na(vec[i])){
      counts[vec[i]-therange[1]+1,2] <- counts[vec[i]-therange[1]+1,2] + 1
    }
  }
  return(counts)
}

###############################################################################################
###############################################################################################
#
# exploration of positions that are (nearly) homozygous for a non-reference nucleotide vs heterozygous positions
#
homnr.v.het <- function(coverage.threshold=20, match.threshold=5, ratio.threshold=0.4, mask=T){
	hvh <- matrix(NA,match.threshold+5,7)
	colnames(hvh) <- names(full.tables)
	rownames(hvh) <- rep(NA,nrow(hvh))
	for(i in 0:match.threshold){
		rownames(hvh)[i+1] <- paste('nearly-homozygous nonref (<=', i, 'ref reads)')
	}
	rownames(hvh)[match.threshold+2] <- paste('nearly-homozygous ref (<=', match.threshold, 'nonref reads)')
	rownames(hvh)[match.threshold+3] <- paste('low-ref-ratio (match <=', ratio.threshold, 'of coverage)')
	rownames(hvh)[match.threshold+4] <- paste('hi-cov-called-SNPs')
	rownames(hvh)[match.threshold+5] <- paste('hi-coverage-positions (cov >=', coverage.threshold, 'reads)')
	for(j in 1:7){
		mcov <- mask & full.tables[[j]]$Cov >= coverage.threshold
		for(i in 0:match.threshold){
			hvh[i+1,j] <- sum(mcov & (full.tables[[j]]$.match <= i))
		}
		hvh[match.threshold+2,j] <- sum(mcov & (full.tables[[j]]$Cov -
		                                        full.tables[[j]]$.match <= match.threshold))
		hvh[match.threshold+3,j] <- sum(mcov & (full.tables[[j]]$.match <=
		                                        full.tables[[j]]$Cov) * ratio.threshold )
		hvh[match.threshold+4,j] <- sum(mcov & (full.tables[[j]]$snp == 1))
		hvh[match.threshold+5,j] <- sum(mcov)
	}
	return(hvh)
}
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
#
# returns vector of 0:4 indicating which nonref nuc has max read count (order: 0agct);
# 0 means all nonref counts == 0
#
# does NO thresholding, e.g. to exclude cases where max nonref count is 1,2, ...;
# since its intended use is to look at SNP positions, there are presumably few where this
# is an issue, but maybe we should do something about that.
# It DOES flag cases where max nonref == 0.
#
nref.nuc <- function(strain=1, mask=T, snp.tables=full.tables.01.26.14){
	# get read count for max nonref nuc
	nref <- apply(snp.tables[[strain]][mask, c('a', 'g', 'c', 't')], 1, max)
	# where does nref count match a (g,c,t, resp) count
	as <- ifelse(nref == snp.tables[[strain]][mask,'a'],1,0)
	gs <- ifelse(nref == snp.tables[[strain]][mask,'g'],2,0)
	cs <- ifelse(nref == snp.tables[[strain]][mask,'c'],3,0)
	ts <- ifelse(nref == snp.tables[[strain]][mask,'t'],4,0)
	# most positions will show 3 zeros and one of 1:4, so max identifies max nonref count;
	# ties broken arbitrarily
	m1 <- pmax(as,gs,cs,ts)
	# but if max nonref count is zero, return 0
	m1[nref==0] <- 0
	return(m1)
}
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
#
# overlaps among called SNP positions
#

# Notes from 12.1.2013 email:
#
# For the 5 strains, the crux of our argument about lack of hardy-weinberg is that a large
# fraction of SNPs are called in the same positions in any pair of strains.  So far as I recall,
# we have not looked at whether the non-reference nucleotides are concordant as well.  It would
# be weird if they weren't, but just to be safe, I looked.  code is in the function snp.olap,
# checked in to SVN in:
#
#   tonys-svn/7_strains/trunk/code/snpNB/scripts/larrys/scatter-plus/scatter-plus.R
#
# Bottom line is that overlap is even stronger when looked at this way.  results, on the 100k
# after desert on chr1 are below.
#
#   - $overlap.counts[i,j] = # of SNP positions (as per Chris' SNP calls) in common between
#     strains i and j; this matrix is necessarily symmetrical.
#
#   - $overlap.percents[i,j] = count[i,j]/count[i,i]; i.e. what fraction of i's SNPs are also
#     seen in j; not symmetrical
#
#   - $alt.counts[i,j]: define the 'alt' nucleotide at any position k of strain i as the
#     non-reference nucleotide with the highest read count (ties broken arbitrarily; ties should
#     in general be quite rare at SNPs, but 0/0 and 1/1 ties at nonSNPs are common).  Among
#     postions that are SNPs in i, count the number where the alt nuc in i == the alt nuc in j.
#     Not symmetric.
#
#   - $alt.percents[i,j] = alt[i,j]/alt[i,i]; at what fraction of i's SNPs is there agreement
#     with j's alt nuc. Not symmetric.
#
# [NOTE: alt.counts include cases where pos in j was not called a SNP, perhaps because its alt
# read count was slighty below threshold.  This certainly alters the error characteristics of the
# data, but given that SNP positions are <1% of positions overall, a more tolerant criterion is
# probably OK, tho maybe we should filter out cases where the 'alt' nucleotide at j is only
# supported by one or two reads.]
#
# Result: alt counts and percents are universally better than olap counts/percents, with all
# pairs from 4 strains showing >= 99% agreement, gyre > 93% with the 4, and agreement between all
# pairs > 49%.
#
# I don't know whether we want to include these numbers in the paper; if so, we should run it on
# a larger sample and probably need to think about additional filtering and/or controls as
# suggested in the NOTE above.  But at least it is reassuring that there is not a surprise
# waiting here.
#
# As to H-W in Italy/Wales, it is encouraging that the overlaps are now > 50%, mostly, but I
# think this data are still inconclusive, given that each 'SNP' is called based on the union of
# several cells.

snp.olap <- function(mask=T){
	alt  <<- vector(mode='list',7)
	olap  <- matrix(NA,7,7)
	olapp <- matrix(NA,7,7)
	alap  <- matrix(NA,7,7)
	alapp <- matrix(NA,7,7)
	rownames(olap)  <- names(full.tables)
	colnames(olap)  <- names(full.tables)
	rownames(olapp) <- names(full.tables)
	colnames(olapp) <- names(full.tables)
	rownames(alap)  <- names(full.tables)
	colnames(alap)  <- names(full.tables)
	rownames(alapp) <- names(full.tables)
	colnames(alapp) <- names(full.tables)
	for(i in 1:7){
		olap[i,i] <- sum(mask & full.tables[[i]]$snp==1)
		alt[[i]]  <<- nref.nuc(i, mask)
	}
	for(i in 1:6){
		for(j in (i+1):7){
			olap[i,j] <- sum(mask & full.tables[[i]]$snp==1 &
				     	        full.tables[[j]]$snp==1)
			olap[j,i] <- olap[i,j]
		}
	}
	for(i in 1:7){
		isnps <- full.tables[[i]][mask,'snp']==1
		alap[i,i] <- sum(isnps)
		for(j in 1:7){
			olapp[i,j] <- round(olap[i,j]/olap[i,i] * 100, 1)
			alap[i,j]  <- sum(alt[[i]][isnps] == alt[[j]][isnps])
			alapp[i,j] <- round(alap[i,j]/alap[i,i] * 100, 1)
		}
	}
	return(list(overlap.counts=olap, overlap.percents=olapp, alt.counts=alap, alt.percents=alapp))
}
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
#
# snp calls shared among multiple strains
#
shared.snp.calls <- function(strains, nc=FALSE, snp.tables=full.tables.01.26.14, NEW=FALSE){
  shared <- snp.tables[[strains[1]]]$snp==1
    if(length(strains)>1){
      for(i in 2:length(strains)){
        shared <- shared & (snp.tables[[strains[i]]]$snp==1)
      }
    }
  ##tc changed so that we can look at non-coding
  ##WLR changed to optionally also exclude undefined reference (NEW=T, circa 3/10/16)
  ## old form used in snp.rates.o, but neither is used by currrent snp.rates
  if(nc){
    if(!NEW){
      shared <- shared & !snp.tables[[strains[1]]]$exon
    } else {
      shared <- shared & !snp.tables[[strains[1]]]$exon & !is.na(snp.tables[[1]]$Ref)
    }
  }
  return(shared)
}
shared.snp.calls.old <- function(strains, nc=FALSE, snp.tables=full.tables.01.26.14){
  shared <- snp.tables[[strains[1]]]$snp==1
    if(length(strains)>1){
      for(i in 2:length(strains)){
        shared <- shared & (snp.tables[[strains[i]]]$snp==1)
      }
    }
  ##tc changed so that we can look at non-coding
  if(nc){
    shared <- shared & !snp.tables[[strains[1]]]$exon
  }
  return(shared)
}
###############################################################################################
###############################################################################################

###############################################################################################
###############################################################################################
#
# Look at distribution of inter-SNP distances:
#   print summary, % of distances < 25, and histogram of distr
#
intersnp.old <- function(strain=1,mask=T,max=200,bins=ceiling(max/5)){
	whichs <- which(full.tables[[strain]]$snp[mask]==1)
	dels <- whichs[-1] - whichs[1:(length(whichs)-1)]
	close <- sum(dels<25)
	close.pct <- close/length(dels)*100
	sumy <- summary(dels)
	sum2 <- c(as.vector(sumy),close.pct)
	names(sum2) <- c(names(sumy),'% < 25')
	print(sum2, digits=3)
	hist(dels[dels<=max],breaks=bins+1)
}
###############################################################################################
###############################################################################################

###############################################################################################
###############################################################################################
#
# Look at distribution of inter-SNP distances:
#   print summary, % of distances < 25
#   plot histogram of distr,
#   overlaid with geometric with same average rate
#   and histo of low-coverage distances (do low-cov cases dominate close pairs?)
#
# Looks at distances between each SNP and the delta-th subsequent SNP.  Default delta=1 =>
# consecutive SNPs & compare to binomial; delta > 1, compare to negative binomial
#
# Some parameters:
#   - delta: above
#   - strain: a specific 1:7, or NULL => 4-way-shared SNPs
#   - mask: look only at selected subset of tables; 'T' => all
#   - scat: if not NULL, some extra plots (concerning...?)
#   - showlow=T/low.thresh: overlay histo with portion attributable to low coverage positions
#   - snp.rate.est: for geometric/NegBinom, use this rate if not NA, else estimate from data
#   - snp.tables: which tables to use
#
intersnp <- function(strain=1, mask=T, max=1000, binwidth=10, bins=ceiling(max/binwidth), low.thresh=25,
	    	     scat=c(20,64,256), delta=1, logy=T, showlow=F, snp.rate.est=NA, debug=FALSE,
		     snp.tables=full.tables.01.26.14){
	if(delta==1){
		xl <- 'inter-SNP distance'
	} else {
		xl <- paste(delta,'-th inter-SNP distance',sep='')
	}

	# get intersnp distances and coverage
	if(is.null(strain)){
		snps4 <- shared.snp.calls(c(1,2,5,7), snp.tables=snp.tables)
		whichs <- which(snps4)
		strain <- 1  ### arbitrary hack; not really interested in coverage but code wants it
		covs <- rep(low.thresh,length(whichs)-delta)
		histo.title <- paste('4-strain shared',xl)
	} else {
		whichs <- which(snp.tables[[strain]]$snp[mask]==1)
		covs <- snp.tables[[strain]]$Cov[mask][snp.tables[[strain]]$snp[mask]==1]
		covs <- (covs[-(1:delta)] + covs[-(length(whichs)+1-(1:delta))])/2  # ave cov @ adj pairs (or try min)
		histo.title <- paste(st.loc(strain),xl)
	}
	####<<<<<<--- NOTE: lines marked like this were once '<<-' global assignments, but now globbed up as return value
	dels <- whichs[-(1:delta)] - whichs[-(length(whichs)+1-(1:delta))]	####<<<<<<---
	if(!is.null(scat)){
		# does cov vary with interSNP dist?
		av <- mean  (snp.tables[[strain]]$Cov[mask])
		md <- median(snp.tables[[strain]]$Cov[mask])
		plot(log2(dels), covs, pch=18, col='blue', ylim=range(covs,av),
			main=paste('Coverage vs', xl ,st.loc(strain)), xlab=xl, ylab='Coverage')
		abline(h=av,col='blue')
		abline(h=md,col='blue',lty=2)
		nseg <- length(scat)+1
		avg <- numeric(nseg)
		med <- numeric(nseg)
		avg[1] <- mean  (covs[dels <= scat[1]])
		med[1] <- median(covs[dels <= scat[1]])
		lines(c(0,log2(scat[1])),c(avg[1],avg[1]))
		lines(c(0,log2(scat[1])),c(med[1],med[1]),lty=2)
		scat <- c(scat,Inf)
		for(i in 1:(nseg-1)){
			avg[i+1] <- mean  (covs[scat[i] < dels & dels <= scat[i+1]])
			med[i+1] <- median(covs[scat[i] < dels & dels <= scat[i+1]])
			lines(c(log2(scat[i]),log2(min(scat[i+1],max(dels)))),c(avg[i+1],avg[i+1]),col=rainbow(nseg)[i])
			lines(c(log2(scat[i]),log2(min(scat[i+1],max(dels)))),c(med[i+1],med[i+1]),col=rainbow(nseg)[i],lty=2)
		}
		legend('topright', bty='n',
			legend=c(paste('GAv',round(av,1),' GMedian',md), paste('Avg',round(avg,1),' Median ',med)))
		hh <- NULL
		counts <- NULL
		breaks <- NULL
	} else {
		#summarize close fraction
		close <- sum(dels<25)
		close.pct <- close/length(dels)*100
		#sumy <- summary(dels)
		#sum2 <- c(as.vector(sumy),close.pct)
		#names(sum2) <- c(names(sumy),'% < 25')
		#print(sum2, digits=3)

		# histo of distances
		hh <- hist(dels[dels<=max],breaks=seq(0,max,binwidth),plot=F)	####<<<<<<---
		if(logy){
			counts <- log2(hh$counts+1)
		} else {
			counts <- hh$counts
		}
		yl <- 'Frequency'
		#hh <- hist(counts, breaks=seq(0,max,binwidth), plot=F)
		breaks <- hh$breaks

		#print(summary(counts))
		#print(summary(breaks))
		print(cbind(rbind(
			intersnp.gaps=summary(dels),
			counts=summary(counts),
			breaks=summary(breaks)),
			'% < 25' = c(close.pct,NA,NA))
			,digits=2)
		plot(hh$breaks,c(0,counts),xlab=xl,ylab=yl,main=histo.title,type='S',yaxt=ifelse(logy,'n','s'),ylim=c(0,max(counts)+2))
		points(hh$breaks,c(counts[1],counts),type='h')
		if(logy){
			ticks <- c(0,1,2,4,6,2^(3:ceiling(log2(max(hh$counts)))))
			axis(2,log2(1+ticks),ticks,las=1)
		}

		if(showlow){# histo of low-coverage distances
			ii <- hist(dels[dels<=max & covs>low.thresh],breaks=seq(0,max,binwidth),add=T,ylim=c(0,max(counts)+2),col='red')
		}

		# overlay geometric with same mean
		if(length(mask)==1 && mask){
			region.length <- nrow(snp.tables[[strain]])
		} else {
			region.length <- sum(mask)
		}
		snp.count <- length(whichs)
		snp.rate <- snp.count/region.length # slight underest? perhaps correct for flanks before 1st/after last in region?
		if(is.na(snp.rate.est)){snp.rate.est <- snp.rate} # estimate from data if not provided as arg
		if(logy){
			lines(hh$mids,log2(1+dnbinom(round(hh$mids),delta,snp.rate.est)*snp.count*binwidth),col='blue')
		} else {
			lines(hh$mids,       dnbinom(round(hh$mids),delta,snp.rate.est)*snp.count*binwidth, col='blue')
		}
		#lines(hh$mids,dgeom(round(hh$mids),snp.rate)*snp.count*binwidth,col='green',lty=2)
		legtxt <- c(paste('N =',snp.count, 'SNPs'),
		            paste('L =',region.length,'nt'),
		            paste('< 25: ',round(close.pct,1),'%',sep=''),
			    paste(ifelse(delta==1, 'Geom', 'NegBinom'), '@ rate'),
		            paste('rate =',round(1/snp.rate.est,1), 'nt/SNP'))
		if(snp.rate.est != snp.rate){
			legtxt <- c(legtxt, paste('(observed rate =',round(1/snp.rate,1), 'nt/SNP)'))
			}
		if(showlow){
		  legtxt <- c(legtxt,paste('White part of hist: coverage <=',low.thresh))
		}
		legend('topright',bty='n',lwd=c(rep(NA,3),1),col='blue', legend=legtxt)
	}
	if(debug){return( list(intersnp.dels=dels, intersnp.hh=hh) )}
}
###############################################################################################

###############################################################################################
# look at average inter-4-way-snp distances in big sliding windows
win.snp4 <- function(win=50e3,step=win/5,horiz=T){
  opar<-par(mfrow=c(2,1),no.readonly=T);on.exit(par(opar))
  snps4 <- shared.snp.calls(c(1,2,5,7))
  l <- length(snps4)
  n <- sum(snps4)
  avg4 <- n/l*1000
  k <- ceiling((l-win)/step)
  a <- numeric(k)
  for(i in 1:k){
    a[i] <- sum(snps4[(i-1)*step+(1:win)])/win*1000
  }
  ttl <- paste('Avg SNPs per Kb (', win/1000, 'Kb windows; steps of ', step/1000, 'Kb)',sep='')
  pos <- 'Chr 1 position'
  rate <- 'SNPs per Kb'
  if(horiz){
    plot((1:k)*step, a, type='o', pch=20, cex=.6, main=ttl, xlab=pos, ylab=rate)
    abline(h=avg4,   col='blue', lty=2)
    abline(h=avg4/2, col='red',  lty=2)
  } else {
    plot(a, (1:k)*step, type='o', pch=20, cex=.6, main=ttl, ylab=pos, xlab=rate, xlim=c(0,15))
    abline(v=avg4,   col='blue', lty=2)
    abline(v=avg4/2, col='red',  lty=2)
  }
  hist(a,breaks=0:15,main='Histogram of SNP rates')
  abline(v=avg4,   col='blue', lty=2)
  abline(v=avg4/2, col='red',  lty=2)
  legend('topright', legend=c('Chr1 average','Half average'), lty=c(2,2), col=c('blue','red'), bty='n')
}
###############################################################################################

# OLD OLD OLD ##############################################################################################
# plot snp rates in deserts and intervening regions (blue/black)
# and after merging deserts within thresh of each other (red)
# 'intervening regions' may include deserts shorter than the selection threshold.
# deserts: 1335;  SNPs: by default, 1335, but can do intersections of samtools SNP calls
#
# parameters:
#  * snp.tables [default full.tables.01.26.14] - where to get SNP and other data; may be a subset
#    of the full data, but should include at least all of Chr1.  (TO CHECK: if it includes more,
#    this may cause shared.snp.calls to return unwanted data, maybe crashing the rest of this...?)
#
#  * nc [default FALSE] - only count SNP rates within NonCoding DNA, as defined by the $exon
#    flag in snp.tables
#
#  * length.thresh [default 5000] - only take deserts this long, based on number of nonexonic
#    positions if nc&&ncmin==T, else based on total length.
#
#  * merge.thresh - if non-null, an int specifying that the plot should include an overlay
#    reflecting merger of deserts within this distance of each other.  (nc/ncmin are irrelevant.)
#
#  * ncmin [default T] - if nc=T, then length.thresh is min number of noncoding positions in desert
#
#  * snpCalls [default NULL, equivalent to c(7)] - if non-NULL, a subset of 1..7.  SNPs counted
#    will be the *intersection* of samtools SNP calls in these strains.  E.g. snpCalls=c(1,2,5,7)
#    will count SNPs shared by the 4 coastal non-Eurpoean isolates.
#
snp.rates.o <- function(length.thresh=5000, merge.thresh=NULL, snpCalls=NULL, nc=FALSE, ncmin=TRUE,
                        snp.tables=full.tables.01.26.14){
  DEBUG <- TRUE
  ##tc changed 7/30/2014 so that we can determine which snps we use
  ##   NOTE: default used to be 4-way SNPSs; new default is 1335 SNPs; param order has changed
  if(!is.null(snpCalls)){
    # Get intersect of samtools SNP calls for selected strains, optionally restricted to nonexonic
    snps4 <- shared.snp.calls(snpCalls, nc, snp.tables, NEW=FALSE)
  } else {
    snps4 <- shared.snp.calls(c(7), nc, snp.tables, NEW=FALSE)
  }
  if(DEBUG){cat('total snps:', sum(snps4),'\n')}

  n.alldes <- nrow(des[[7]][[1]])
  des.nclen <- integer(n.alldes)
  if(nc){
    # count just non-exonic positions in deserts
    for(i in 1:n.alldes){
      des.nclen[i] <- sum(!snp.tables[[1]][des[[7]][[1]][i,1]:des[[7]][[1]][i,2], 'exon'])
    }
  }
  if(nc && ncmin){
    # select deserts exceeding length threshold, either total length or nonexonic length
    big.des <- (des.nclen > length.thresh)
  } else {
    big.des <- (des[[7]][[1]][,3] > length.thresh)
  }
  # list of big deserts, plus nonexonic
  new.tab <- cbind(des[[7]][[1]][big.des,], nonexonic=des.nclen[big.des])

  ndes  <- nrow(new.tab)
  sn    <- integer(ndes)  # number of SNPS in ith desert
  snlen <- integer(ndes)  #   and its length
  snr   <- numeric(ndes)  # SNP rate in ith desert
  snsig <- numeric(ndes)  # std. dev. of rate
  for(i in 1:ndes){
    sn[i] <- sum(snps4[new.tab[i,1]:new.tab[i,2]])
    ##tc changed to account for non-coding
    if(nc){
      snlen[i] <- new.tab[i,4]
    } else {
      snlen[i] <- new.tab[i,2] - new.tab[i,1] + 1
    }
    snr[i] <- sn[i]/snlen[i]
    snsig[i] <- sqrt(snr[i]*(1-snr[i])/snlen[i])
  }

  # analogous stats for flanking non-deserts
  usn    <- integer(ndes+1)
  usnlen <- integer(ndes+1)
  usnr   <- numeric(ndes+1)
  usnsig <- numeric(ndes+1)

  # special case to the left of 1st desert
  usn[1] <- sum(snps4[1:(new.tab[1,1]-1)])
  ##tc changed - looking at only non-exon in flanking
  if(nc){
      usnlen[1] <- sum(!snp.tables[[1]][1:new.tab[1,1], 'exon'])
  } else {
      usnlen[1] <- new.tab[1,1] - 1
  }
  # general case: undesert left of i-th desert
  for(i in 2:ndes){
    usn[i] <- sum( snps4[ (new.tab[i-1,2]+1):(new.tab[i,1]-1) ] )
    ##tc changed...only looking for non-exon in flanking
    if(nc){
        usnlen[i] <- sum(!snp.tables[[1]][new.tab[i-1,2]:new.tab[i,1], 'exon'])
    } else {
        usnlen[i] <- new.tab[i,1] - new.tab[i-1,2] - 1
    }
  }

  # special case to the right of last desert
  # find end of Chr1
  if(all(snp.tables[[1]]$chr=='Chr1')){
    last.chr1 <- nrow(snp.tables[[1]])
    #cat('ch1', last.chr1)
  } else {
    last.chr1 <- which.max(snp.tables[[1]]$chr!='Chr1')-1
    #cat('all', last.chr1)
  }
  #cat(' should be: 3042585 \n')
  usn[ndes+1] <- sum(snps4[(new.tab[ndes,2]+1):last.chr1])
  usnlen[ndes+1] <- last.chr1 - new.tab[ndes,2]

  # rate, sigma for each:
  for(i in 1:(ndes+1)){
    usnr[i] <- usn[i]/usnlen[i]
    usnsig[i] <- sqrt(usnr[i]*(1-usnr[i])/usnlen[i])
  }

  # aggregate stats:
  desert.stats <- cbind(new.tab[,1:2],snlen,sn,snr,snsig,new.tab[,3:4])
  nondesert.stats <- cbind(usnlen,usn,usnr,usnsig)

  des.col   <- rep('blue',ndes)
  undes.col <- rep('black',ndes+1)

  # some adjacent deserts are separated by very short non-deserts, resulting
  # in highly variable rate estimates.  To smooth these, if requested, we merge
  # deserts within merge.thresh of each other.
  # '*.all' stats below are for full extent of the merged region;
  # '*.nc' will be the same if nc=F, else just for the nonexonic subset.
  df <- NULL
  if(!is.null(merge.thresh)){
    i <- 1
    while(i < ndes){
      j <- i
      while(j < ndes && new.tab[j,2]+merge.thresh > new.tab[j+1,1]){j <- j+1}
      if(i < j){
        des.col[i:j] <- 'lightblue'
        undes.col[(i+1):j] <- 'grey'
        snx <- sum(snps4[new.tab[i,1]:new.tab[j,2]])	# raw SNP count in whole interval
        snxlen <- new.tab[j,2] - new.tab[i,1] + 1	# raw undesert length
        snxd <- sum(sn[i:j])   	 	      		# tot SNPs counted in those deserts (wrt nc)
        snxu <- sum(usn[(i+1):j])			# tot SNPs counted in those undeserts (wrt nc)
        snxdlen <- sum(snlen[i:j])			# tot len of deserts
        snxulen <- sum(usnlen[(i+1):j])			# tot len of undeserts
        snxr.all <- snx/snxlen				# SNP rate and sigma, whole interval
        snxsig.all <- sqrt(snxr.all*(1-snxr.all)/snxlen)
        snxr.nc <- (snxd+snxu)/(snxdlen+snxulen)	# ditto, wrt nc
        snxsig.nc <- sqrt(snxr.nc*(1-snxr.nc)/(snxdlen+snxulen))
        df <- rbind(df,data.frame(i=i, j=j, deslen=snxdlen, snxd=snxd, undeslen=snxulen, snxu=snxu, len=snxlen, snx=snx,
                  snxr.all=snxr.all, snxsig.all=snxsig.all, snxr.nc=snxr.nc, snxsig.nc=snxsig.nc))
      }
      i <- j+1
    }
    rownames(df) <- NULL
  }

   if(nc){
    ylab <- 'Non-Exonic SNP rate (SNPs per base-pair)'
  } else {
    ylab <- 'SNP rate (SNPs per base-pair)'
  }
  if(nc && ncmin){
    xlab <- paste('a. Chromosome 1: Deserts with >', length.thresh/1000, 'K Non-Exonic Positions')
  } else {
    xlab <- paste('a. Chromosome 1: Deserts of Size >', length.thresh/1000, 'K')
  }
  if(is.null(snpCalls)){
    main <- 'SNP Rates in CCMP1335 Chromosome 1 Deserts/non-Deserts'
  } else {
    main <- paste(paste(snpCalls,collapse='-'),'Shared SNP Rates in CCMP1335 Chromosome 1 Deserts/non-Deserts')
  }
  plot(0:(ndes+1),ylim=c(0,max(snr+2*snsig,usnr+0.1*usnsig,na.rm=T)),type='n',
       xlab="", ylab=ylab, main=main)

  if(is.null(merge.thresh)){
    legend('topleft',bty='n',
         legend=c('Intervening','Desert'),
         lty=c(1,1),
         col=c('black','blue'))
  } else {
    legend('topleft',bty='n',
         legend=c('Intervening','Desert',paste(merge.thresh/1000, 'Kb merged deserts')),
         lty=c(1,1,1),
         col=c('black','blue','red'))
  }

  points(1:ndes,snr,pch=18,col=des.col)
  for(i in 1:ndes){lines(c(i,i),snr[i]+c(-2,2)*snsig[i],col=des.col[i])}

  points((1:(ndes+1))-0.5, usnr, pch=18,col=undes.col)
  for(i in 1:(ndes+1)){lines(c(i,i)-0.5,usnr[i]+c(-2,2)*usnsig[i],col=undes.col[i])}

  if(!is.null(df)){
    del <- 0.05
    for(k in 1:nrow(df)){
      i <- df$i[k]
      j <- df$j[k]
      snxr   <- df$snxr.nc[k]
      snxsig <- df$snxsig.nc[k]
      lines(c(i+del,j-del),rep(snxr,2),col='red')
      lines(rep((i+j)/2,2),snxr+c(-2,2)*snxsig,col='red')
    }
  }
  return(list(desert.stats=desert.stats, nondesert.stats=nondesert.stats, merged.desert.stats=df))
}
# END OLD END OLD ##############################################################################################

# NEW NEW NEW ##############################################################################################
# plot snp rates in deserts and intervening regions (blue/black)
# and after merging deserts within thresh of each other (red)
# 'intervening regions' may include deserts shorter than the selection threshold.
# SNPs/deserts: by default, SAMTools 1335 calls & deserts, but can do any strain
#
# Initially, code only examined deserts in Chr1. Reworking to look beyond Chr1 using des.to.df(des)
# data frames in place of des list-of-lists.
#
# parameters:
#  * snp.tables [default full.tables.01.26.14] - where to get SNP and other data; may be a subset
#    of the full data, but should include at least all of Chr1.
#
#  * nc [default FALSE] - If T, only count SNP rates within NonCoding DNA, or more accurately,
#    non-exonic DNA, as defined by the $exon flag in snp.tables
#
#  * length.thresh [default 5000] - only take deserts this long (see next).  
#
#  * length.thresh.eff [default F] - if T, length.thresh is based on "effective desert length,"
#    i.e., number of desert positions that are not NA in genome, not in CNVnator deletion calls
#    if cnv.dels parameter supplied, and not exonic if nc==T, else based on total length.
#
#  * cnv.dels [defualt NULL] - if supplied, table of CNVnator calls for effective length calc.
#
#  * merge.thresh - if non-null, an int specifying that the plot should include an overlay
#    reflecting merger of deserts within this distance of each other (absolute, not effective 
#    distance)
#
#  * strain [default 7] - which snps/deserts to use
#
#  * xCoordsReal [default F] - in plot, should markers be plotted at real chromosomal coords (T),
#    or at desert index (F)?
#
#  * xlab, ylab, main, legend,... [NULL] - plot axis labels, legend and title; if NULL, they are
#    calculated below; non-NULL values override the default calculation
#
#  * ... extra params assumed to be graphic params to main plot, e.g. cex.lab
#
#
# A NOTE ABOUT NA:
# 
# NAs in 'Chr' (and other table fields), are responsible for a gap of ~10k and some other sporadic
# positions in Chr1, perhaps 225k elsewhere.
# 
# Of the various fields in the big tables, Chr, Pos & Ref (but neither chr nor pos) have many NA's,
# I think marking gaps in the reference seq.  Others, in particular 'exon', have none.  In general,
# I think these NA regions are nearly SNPless, (and presumably erroneous if not) and long stretches
# thereof are probably being called as "deserts", and ALL of the NA positions are labeled non-exon.
# See newer discussion in scripts/larrys/paperfigs/Fig2A-desert-distribution.rnw for more on this.
#
# > names(snp.tables.full[[1]])
# [1] "chr"    "pos"    "snp"    "Chr"    "Pos"    "Ref"    "Cov"    "a"      "g"      "c"     
# [11] "t"      "n"      ".match" "exon"   "indel" 
# > unlist(lapply(names(snp.tables.full[[1]]), function(x){sum(is.na(snp.tables.full[[1]][,x]))}))
# [1]      0      0      0 228819 228819 228819      0      0      0      0      
# [11]     0      0      0      0      0
# > sum(snp.tables.full[[1]]$exon && is.na(snp.tables.full[[1]]$Ref))
# [1] 0
#
# There are also a few hundred 'N' nucleotides in the ref seq, but none happens to be a SNP:
#
# > sum(snp.tables.full[[1]]$Ref=='N',na.rm=T)
# [1] 745
# > sum(snp.tables.full[[1]]$Ref=='N' & snp.tables.full[[7]]$snp==1,na.rm=T) 
# [1] 0
#
# The distinction between NA and N is unclear to me. Perhaps I should treat them like NAs, but in
# the interest of simplicity, and since they are few, I'm ignoring N's for now.
#
###
#
# Even NEWER: as of 2017-07-19, separating calc from plot; calc returns a blob for plot to use.
#   plain snp.rates retained for backward compatability.
#
snp.rates <- function(strain=7,                         # which strain to process
                      length.thresh=5000,               # skip shorter deserts
                      length.thresh.eff=FALSE,          # thresh by effective length?
                      nc=FALSE,                         # only do noncoding rates
                      merge.thresh=NULL,                # visual merge in plot if closer
                      snp.tables=full.tables.01.26.14, 
                      des.tables=des,
                      cnv.dels=NULL,                    # if non-null, mask these CNVnator dels
                      DEBUG=FALSE,
                                                    # params below are for plotting
                      des.col='blue',                   # color for desert points, err bars
                      undes.col='black',                # ditto for non-deserts
                      yclip=NULL,                       # if non-NULL, clip y axis here 
                      legend=NULL,                      # non-NULL => overrides default; '' => omit 
                      xlab=NULL, ylab=NULL, main=NULL,  # if-non-NULL, override defaults
                      ylab.sub=NULL,                    # if non-NULL, subtext
                      yticks=NULL,                      # if non-NULL, override y-axis ticks
                      xCoordsReal=FALSE,                # alt plot based on real coords
                      ...){
  
  snp.rates.blob <- snp.rates.calc(strain, length.thresh, length.thresh.eff, nc, merge.thresh, 
                                   snp.tables, des.tables, cnv.dels, DEBUG)
  if(DEBUG){print(str(snp.rates.blob))}
  snp.rates.plot(snp.rates.blob, des.col, undes.col, yclip, legend, xlab, ylab, main, ylab.sub, 
                 yticks, xCoordsReal, snp.tables, des.tables, ...)
}

snp.rates.calc <- function(strain, length.thresh, length.thresh.eff, nc, merge.thresh,
                           snp.tables, des.tables, cnv.dels, DEBUG=FALSE) {
  if (length(strain) != 1) {
    cat('*** snp.rates requires length(strain)==1. ***\n')
  }
  
  # snps4 (meaningless historical name) is the full set of SNPs we could count
  snps4 <- snp.tables[[strain]]$snp==1
  snp.count.raw <- sum(snps4)
  snp.count.df <- data.frame(Type='total snps:', 
                             SNP.count=snp.count.raw,
                             Total.Positions=length(snps4))
  
  # positions is the full set of genomic positions over which we count.  Initially this is the full
  # genome but will be reduced by (1) NA's in the refseq, (2) CNVnator deletion calls, if requested,
  # (3) exonic positions, if requested, and (4) if I get around to it, possibly by short desert
  # calls in the intervals between long deserts. (1) & (2) are removed since these regions 
  # shouldn't contain SNPs (and mostly don't) but cover a lot of real estate.  Removing (3) focuses
  # on more neutral positions.  Des to nondes comparison is conservative if we don't do (4), but
  # more apples-to-apples if we do.
  positions <- !is.na(snp.tables[[1]]$Ref) 
  snp.count.noNA <- sum(snps4 & positions)
  snp.count.df <- rbind(snp.count.df,
                        data.frame(Type='after removing NAs in ref:', 
                                   SNP.count=snp.count.noNA,
                                   Total.Positions=sum(positions)))
  if(!is.null(cnv.dels)){
    positions <- positions & ! cnv.dels[[strain]]
    snp.count.noCNV <- sum(snps4 & positions)
    snp.count.df <- rbind(snp.count.df,
                          data.frame(Type='after (also) removing CNVnator dels:', 
                                     SNP.count=snp.count.noCNV,
                                     Total.Positions=sum(positions)))
  }
  if(nc){
    positions <- positions & ! snp.tables[[strain]]$exon
    snp.count.nc <- sum(snps4 & positions)
    snp.count.df <- rbind(snp.count.df,
                          data.frame(Type='after (also) removing exons:', 
                                     SNP.count=snp.count.nc,
                                     Total.Positions=sum(positions)))
  }
  cat('snp.rates:\n')
  snp.count.df <- cbind(snp.count.df, SNP.Rate=snp.count.df$SNP.count/snp.count.df$Total.Positions)
  print(snp.count.df)
  cat('\n')   
  
  # generalizing to full genome;  at least temporarily, I will use old code ("des" tables)
  # if just chr1, else use des.to.df to convert to more convenient data.frame format.
  chr1.only <- (nrow(snp.tables[[1]]) == genome.length.constants()$chr1.length)
  if(chr1.only){
    if(DEBUG){cat('chr1\n')}
    # n.alldes is the count of deserts we look at
    n.alldes <- nrow(des.tables[[strain]][[1]])
    des.len <- des.tables[[strain]][[1]][,'Length']
  } else {
    if(DEBUG){cat('not chr1\n')}
    des.df <- des.to.df(des.tables)
    n.alldes <- nrow(des.df[[strain]])
    des.len <- des.df[[strain]]$Length
  }
  
  # des.len[i] is length of the i-th desert
  # des.eff.len[i]  will be the (effective) length of the i-th desert, after  
  # masking 'positions' as above
  des.eff.len <- integer(n.alldes)
  for(i in 1:n.alldes){
    if(chr1.only){
      des.span <- des.tables[[strain]][[1]][i,1] : des.tables[[strain]][[1]][i,2]
    } else {
      des.span <- des.df[[strain]]$iStart[i] : des.df[[strain]]$iEnd[i]
    }
    # count (masked) positions in desert
    des.eff.len[i] <- sum(positions[des.span])
  }

  # select deserts exceeding length threshold, either based on total length or on effective length
  if(length.thresh.eff){
      big.des <- (des.eff.len > length.thresh)
  } else {
      big.des <- (des.len     > length.thresh)
  }
  if(DEBUG){cat('number of big.deserts:',sum(big.des),'\n')}
  # list of big deserts, with effective lengths
  if(chr1.only){
    big.df <- data.frame(Chr='Chr1',
                          Start  =des.tables[[strain]][[1]][big.des,1], 
                          End    =des.tables[[strain]][[1]][big.des,2],
                          iStart =des.tables[[strain]][[1]][big.des,1], 
                          iEnd   =des.tables[[strain]][[1]][big.des,2], 
                          Length =des.tables[[strain]][[1]][big.des,3],
                          Len.eff=des.eff.len[big.des])
  } else {
    big.df <- data.frame(des.df[[strain]][big.des,c(1:3,5,6,4)],
                          Len.eff=des.eff.len[big.des])
  }
  rownames(big.df) <- NULL
  if(DEBUG){cat('big.df:\n');print(big.df)}
  
  ndes  <- nrow(big.df)
  sn    <- integer(ndes)  # number of SNPS in ith desert
  snlen <- integer(ndes)  #   and its EFFECTIVE length
  snr   <- numeric(ndes)  # SNP rate in ith desert
  snsig <- numeric(ndes)  # std. dev. of rate
  for(i in 1:ndes){
    des.irange <- big.df$iStart[i] : big.df$iEnd[i]
    sn[i] <- sum(positions[des.irange] & 
                     snps4[des.irange])
  }
  snlen <- big.df$Len.eff
  snr   <- sn/snlen
  snsig <- sqrt(snr*(1-snr)/snlen)
  
  # analogous stats for flanking non-deserts
  
  usn    <- integer(ndes+1)
  usnlen <- integer(ndes+1)
  usnr   <- numeric(ndes+1)
  usnsig <- numeric(ndes+1)

  # Create a data frame analogous to big.df for the intervals between the big deserts. Chr will 
  # appear twice, since start/end may be on different chrs.
  if(chr1.only){
    last.index <- genome.length.constants()$chr1.length
  } else {
    last.index <- genome.length.constants()$genome.length.trunc
  }
  last.pos   <- snp.tables[[strain]][last.index,'pos']
  last.chr   <- as.character(snp.tables[[strain]][last.index,'chr'])
  interbig.df <- data.frame(sChr   = c('Chr1',as.character(big.df$Chr)),
                            Start  = c(1, big.df$End+1),
                            eChr   = c(as.character(big.df$Chr), last.chr),
                            End    = c(big.df$Start-1, last.pos),
                            iStart = c(1, big.df$iEnd+1),
                            iEnd   = c(big.df$iStart-1, last.index),
                            Length = 0,
                            Len.eff= 0
  )
  interbig.df$Length <- interbig.df$iEnd-interbig.df$iStart+1
  rownames(interbig.df) <- NULL
  
  for(i in 1:nrow(interbig.df)){
    undes.span <- interbig.df$iStart[i] : interbig.df$iEnd[i]
    # count (masked) positions in undesert
    interbig.df$Len.eff[i] <- sum(positions[undes.span])
    usn[i] <- sum(positions[undes.span] & snps4[undes.span])
  }
  usnlen <- interbig.df$Len.eff
  
  if(DEBUG){cat('interbig.df:\n');print(interbig.df)}

  # rate, sigma for each:
  usnr <- usn/usnlen
  usnsig <- sqrt(usnr*(1-usnr)/usnlen)

  # aggregate stats:
  desert.stats <- data.frame(Chr=as.character(big.df$Chr),
                             big.df[,c('Start','End','iStart','iEnd')],
                             sn,snr,snsig,big.df[,c('Length','Len.eff')],
                             stringsAsFactors = FALSE)
  # add a summary line
  desert.stats[ndes+1,'Chr']       <- 'Overall'
  desert.stats[ndes+1,'Length']    <- sum(desert.stats[1:ndes,'Length'])
  desert.stats[ndes+1,'Len.eff']   <- sum(desert.stats[1:ndes,'Len.eff'])
  desert.stats[ndes+1,'sn']        <- sum(desert.stats[1:ndes,'sn'])
  desert.stats[ndes+1,'snr']       <- desert.stats[ndes+1,'sn']/desert.stats[ndes+1,'Len.eff']

  nondesert.stats <- data.frame(usnlen,usn,usnr,usnsig)
  # summary line
  nondesert.stats[ndes+2,'usnlen'] <- sum(nondesert.stats[1:(ndes+1),'usnlen'])
  nondesert.stats[ndes+2,'usn']    <- sum(nondesert.stats[1:(ndes+1),'usn'])
  nondesert.stats[ndes+2,'usnr']   <- nondesert.stats[ndes+2,'usn']/nondesert.stats[ndes+2,'usnlen']

  # some adjacent deserts are separated by very short non-deserts, resulting in highly variable rate
  # estimates.  To smooth these, if requested, we merge deserts within merge.thresh of each other.
  # '*.all' stats below are for full extent of the merged region;
  # '*.nc' will be the same if nc=F, else just for the nonexonic subset.
  
  # When merge is requested, the original [un]deserts being merged are still plotted, but in lighter
  # color, overlayed by merged version is another color.  Splitting between calc & merge complicates
  # this.  My hack to fix is to export Bool vectors flagging who's been absorbed in a merge and let
  # .plot decide colors.  A nuisance ... 
  
  des.merged.over   <- rep(FALSE, ndes)
  undes.merged.over <- rep(FALSE, ndes+1)
  if(DEBUG){print(des.merged.over)}
  
  merge.df <- NULL
  if(!is.null(merge.thresh)){
    cat('*** snp.rates: merge code not yet updated to get Len.eff/masked SNPs. ***\n')
    i <- 1
    while(i < ndes){
      j <- i
      while(j < ndes && big.df$iEnd[j]+merge.thresh > big.df$iStart[j+1]){j <- j+1}
      if(i < j){
        des.merged.over[i:j]       <- TRUE # 'lightblue'
        undes.merged.over[(i+1):j] <- TRUE # 'grey'
        snx <- sum(snps4[big.df$iStart[i]:big.df$iEnd[j]])  # raw SNP count in merged interval
        snxlen <- big.df$iEnd[j] - big.df$iStart[i] + 1     # raw merged length 
        snxd <- sum(sn[i:j])                  # tot SNPs counted in those deserts (wrt nc)
        snxu <- sum(usn[(i+1):j])			        # tot SNPs counted in those undeserts (wrt nc)
        snxdlen <- sum(snlen[i:j])			      # tot len of deserts
        snxulen <- sum(usnlen[(i+1):j])			  # tot len of undeserts
        snxr.all <- snx/snxlen			      	  # SNP rate and sigma, whole interval
        snxsig.all <- sqrt(snxr.all*(1-snxr.all)/snxlen)
        snxr.nc <- (snxd+snxu)/(snxdlen+snxulen)	      # ditto, wrt nc
        snxsig.nc <- sqrt(snxr.nc*(1-snxr.nc)/(snxdlen+snxulen))
        merge.df <- rbind(merge.df,data.frame(i=i, j=j, deslen=snxdlen, snxd=snxd, undeslen=snxulen, snxu=snxu,
                                  len=snxlen, snx=snx, snxr.all=snxr.all, snxsig.all=snxsig.all, 
                                  snxr.nc=snxr.nc, snxsig.nc=snxsig.nc))
      }
      i <- j+1
    }
    rownames(merge.df) <- NULL
  }
  
  # return a blob of data containing 
  #  1) All parameters to .calc call EXCEPT the large tables (snp.tables, des.tables, cnv.dels)
  #  2) All of the derived summaries needed for plots
  #  3) Other useful summaries
  snp.rates.blob <- list(strain=strain, length.thresh=length.thresh, 
                         length.thresh.eff=length.thresh.eff, nc=nc, merge.thresh=merge.thresh,
                         DEBUG=DEBUG,
                         chr1.only=chr1.only,
                         desert.stats=desert.stats,
                         nondesert.stats=nondesert.stats,
                         last.index=last.index,
                         ndes=ndes,
                         snr=snr,
                         snsig=snsig,
                         usnr=usnr,
                         usnsig=usnsig,
                         merge.df=merge.df,
                         des.merged.over=des.merged.over,
                         undes.merged.over=undes.merged.over,
                         # following aren't needed in plot, but convenient to grab
                         snp.count.df=snp.count.df, des.len=des.len, des.eff.len=des.eff.len, 
                         big.df=big.df, last.index=last.index, last.pos=last.pos,
                         last.chr=last.chr, interbig.df=interbig.df
                         )
  return(snp.rates.blob)
}

snp.rates.plot <- function(srb,                 # blob from above; rest are for plotting
                           des.col,             # color for desert points, err bars
                           undes.col,           # ditto for non-deserts
                           yclip,               # if non-NULL, clip y axis here 
                           legend,              # non-NULL => overrides default; '' => omit 
                           xlab, ylab, main,    # if-non-NULL, override defaults
                           ylab.sub,            # if non-NULL, subtext
                           yticks=NULL,         # if non-NULL, override y-axis ticks
                           xCoordsReal=FALSE,   # alt plot based on real coords (probably buggy)
                           snp.tables=NULL,     # needed only if xCoordsReal
                           des.tables=NULL,     # ditto
                           ...){
  # pull calc params from blob (others explicitly ref-d as srb$...)
  strain            <- srb$strain
  length.thresh     <- srb$length.thresh
  length.thresh.eff <- srb$length.thresh.eff
  nc                <- srb$nc
  merge.thresh      <- srb$merge.thresh
  DEBUG             <- srb$DEBUG
  # retrofit [un]des.col.vec (color vectors); .calc sets [un]des.merged.over T/F
  des.col.vec   <- rep(  des.col, srb$ndes  )
  undes.col.vec <- rep(undes.col, srb$ndes+1)
  des.col.vec[srb$des.merged.over] <- 'lightblue'
  undes.col.vec[srb$undes.merged.over] <- 'grey'
  
  if(is.null(xlab)){
    xlab <- paste('Deserts >', length.thresh/1000, 'K',
                  ifelse(length.thresh.eff, 'Effective', ''),
                  ifelse(nc, '(Non-Exon)', ''))
    
  }

  if(is.null(ylab)){
    if(nc){
      ylab <- 'Non-Exonic SNP rate (SNPs per bp)'
    } else {
      ylab <- 'SNP rate (SNPs per bp)'
    }
  }
  
  if(is.null(main)){
    main <- paste('SNP Rates in', st.loc(strain,loc=F) ,'Deserts/non-Deserts')
    if(srb$chr1.only){
      main <- paste(main, '(Chr 1)')
    } else{
      main <- paste(main, '(All Chrs)')
    }
  }

  if(xCoordsReal){
    # NEEDS UPDATE, if we ever care to use it
    if(F && DEBUG){print(str(srb$usnr));print(str(srb$desert.stats))}
    xrange <- c(0, srb$last.index)
    des.x.coords   <- (    srb$desert.stats[1:srb$ndes,'iStart'] +  srb$desert.stats[1:srb$ndes,'iEnd'])/2
    undes.x.coords <- (c(0,srb$desert.stats[1:srb$ndes,'iEnd']) + c(srb$desert.stats[1:srb$ndes,'iStart'],srb$last.index))/2
  } else {
    xrange <- c(0, srb$ndes+1)
    des.x.coords   <- 1:srb$ndes
    undes.x.coords <- (1:(srb$ndes+1))-0.5
  }
  if(is.null(yclip)){
    yclip <- max(srb$snr+2*srb$snsig,srb$usnr+0.1*srb$usnsig,na.rm=T)
  }
  plot(NA,xlim=xrange,ylim=c(0,yclip),type='n', main=main, xaxt='n', yaxt='n', xlab='', ylab='', ...)
  axis(1, padj=-1)
  # Hack: override y-axis ticks for paperfig
  if(!is.null(yticks)){
    axis(2, padj=yticks$padj,at=yticks$at,labels=yticks$labels,cex.axis=yticks$cex) 
  } else {
    axis(2, padj=0.9) 
  }
  mtext(xlab,side=1,line=1.8,cex=1.0)
  mtext(ylab,side=2,line=1.8,cex=1.0)
  if(!is.null(ylab.sub)){
    mtext(ylab.sub$text,side=2,line=ylab.sub$line,cex=ylab.sub$cex)
  }
  if(is.null(legend)){
    legend <- c('Intervening', 'Desert')
  }
  if(paste(legend,collapse='') != ''){
    if(is.null(merge.thresh)){
      legend('topleft',bty='n',
             legend=legend,
             lty=c(1,1),
             pch=c(18,18),
             col=c(undes.col, des.col))
    } else {
      legend('topleft',bty='n',
             legend=c('Intervening','Desert',paste(merge.thresh/1000, 'Kb merged deserts')),
             lty=c(1,1,1),
             col=c(undes.col, des.col, 'red'))
    }
  }
  
  if(DEBUG){
    cat('Des pts:\n')
    print(des.x.coords)
    print(srb$snr)
    print(des.col.vec)
  }
  points(des.x.coords, srb$snr, pch=18, col=des.col.vec)
  for(i in 1:srb$ndes){
    lines(rep(des.x.coords[i],2),srb$snr[i]+c(-2,2)*srb$snsig[i],col=des.col.vec[i])
    if(xCoordsReal){
      lines(c(srb$desert.stats[i,'iStart'], srb$desert.stats[i,'iEnd']),rep(-.0002,2),col='red')
    }
  }
  
  points(undes.x.coords, srb$usnr, pch=18, col=undes.col.vec)
  for(i in 1:(srb$ndes+1)){
    lines(rep(undes.x.coords[i],2),srb$usnr[i]+c(-2,2)*srb$usnsig[i],col=undes.col.vec[i])
  }
  
  if(!is.null(srb$merge.df)){
    del <- 0.05
    for(k in 1:nrow(srb$merge.df)){
      i <- srb$merge.df$i[k]
      j <- srb$merge.df$j[k]
      snxr   <- srb$merge.df$snxr.nc[k]
      snxsig <- srb$merge.df$snxsig.nc[k]
      if(xCoordsReal){
        lines(c(srb$desert.stats$iStart[i], srb$desert.stats$iEnd[j]),rep(snxr,2),col='red')
        lines(rep(sum(srb$desert.stats$iStart[c(i,j)])/2,2),snxr+c(-2,2)*snxsig,col='red')
        if(!srb$chr1.only){
          # des.tables and snp.tables needed here ONLY
          # add ticks to mark chr boundaries
          des.df <- des.to.df(des.tables)
          for(chr in levels(des.df[[1]]$chr)){
            lines(rep(chr.to.index(chr,snp.tables = snp.tables),2), c(-.0003,-.0005),col='gray')
          }
          ###hmmm. should be 'last.pos' from .calc??? Or last.index??
          lines(rep(last.position,2), c(-.0003,-.0005),col='gray') 
        }
      } else {
        lines(c(i+del,j-del),rep(snxr,2),col='red')
        lines(rep((i+j)/2,2),snxr+c(-2,2)*snxsig,col='red')
      }
    }
  }

  return(list(desert.stats=srb$desert.stats, nondesert.stats=srb$nondesert.stats, 
              merged.desert.stats=srb$merge.df))
}

###############################################################################################

###
# is there spacial structure to variants?  Select positions with min coverage and min nonref fraction.
#
# indp of plotting:
#
#   find (and return) n x 7 matrix of 'nonref allele freqs' for the n positions in the UNION of
#   positions in strains in who at which nonref satisfies filtering criteria defined by min.cover,
#   min/max.nr.frac.  Who order is ignored; Columns not in who are left empty.
#
#   if allmin, restrict to positions with  min.cover<=coverage<=max.cover in *all* strains in who.
#   non-default julie.filters further restrict result.
#
# if plot=T:
#
#   UPPER PANEL: nonref fraction vs index of position (green line at right edge of desert)
#   LOWER PANEL: ditto, but with position randomly permuted.
#   BOTH: loess fit, using small span; in Italy & wales, shuffled version is notably flatter,
#   suggesting that this region is a mosaic of patches of 100 or so consecutive positions
#   (i.e. ~10Kb segments) that tend to be at similar non-ref allele frequency, either <.6 or > .6.
#   *IF* they are sexually reproducing, suggests recombination hotspots?  If *NOT*,
#   suggests perhaps gene conversion events dragging allele freq high or low?
#   There is doubtless a more quantitative test for this...
#
spacial.freq <- function(who=3, region=NULL, min.cover=20, max.cover=Inf, min.nr.frac=.1, max.nr.frac=1.0,
                         span=.05,pwr=1,permute=T,plot=T,snp.tables=full.tables.01.26.14, allmin=F,
                         julie.filter1=0, julie.filter2=F
			){
  mm <- region
  if(allmin){
    mmallc <- make.mask(who, mm, min.cover=min.cover, max.cover=max.cover, snp.tables=snp.tables)
    if(length(who)>1){
      mm <- mmallc[[who[1]]]
      for(i in 2:length(who)){
        mm <- mm & mmallc[[who[i]]]
      }
    } else {
      mm <- mmallc
    }
  }
  mmall <- make.mask(who, mm, min.cover=min.cover, max.cover=max.cover,
                     min.nr.frac=min.nr.frac, max.nr.frac=max.nr.frac, snp.tables=snp.tables)
  if(length(who)>1){
    mm <- mmall[[who[1]]]
    for(i in 2:length(who)){
      mm <- mm | mmall[[who[i]]]
    }
  } else {
    mm <- mmall
  }
  mm.na <- sum(is.na(mm))
  if(mm.na>0){
    cat("Spacial.freq: killing", mm.na, "NA's in mm mask.\n")
    mm[is.na(mm)] <- FALSE
  }
  n <- sum(mm)
  nrfall <- matrix(nrow=n,ncol=length(snp.tables))
  colnames(nrfall) <- names(snp.tables)
  rownames(nrfall) <- paste(snp.tables[[1]]$chr[mm],snp.tables[[1]]$pos[mm],sep=':')
  for(w in who){
    # cat('spacial.freq for',w)
    # pull selected rows from snp.tables
    snppet <- snp.tables[[w]][mm,]

    # extra filtering to address Julie's concern that discordant bases are included,
    # the bulk of which are bases with 1 read; subtract them from both nr and cover
    if(julie.filter1 > 0){
      # cat('Filter 1=',julie.filter1)
      for(nuc in c('a','g','c','t')){
        toothin <- (snppet[,nuc] <= julie.filter1)
        # tzap <- sum(snppet[toothin,nuc])
        snppet$Cov[toothin] <- snppet$Cov[toothin] - snppet[toothin,nuc]
        snppet[toothin,nuc] <- 0
        # cat(' zapped',sum(toothin),tzap,nuc,'\'s.')
      }
    }

    # as in scatterplus: max.nonref/(that+match)
    if(julie.filter2){
      # cat(';Filter2')
      max.nref <- apply(snppet[, c('a', 'g', 'c', 't')], 1, max)
      nrfall[,w] <- max.nref / ( max.nref + snppet$.match )
    } else {
      nrfall[,w] <- 1 - snppet$.match / snppet$Cov
    }
    # cat('\n')
  }
  if(plot){
    spacial.freq.plot(who,nrfall,mm,min.nr.frac,desert.start,desert.length,permute,span,pwr,des,snp.tables)
  }
  return(nrfall)
}

spacial.freq.plot <- function(who,nrfall,mm,min.nr.frac,desert.start,desert.length,permute,span,pwr,des,snp.tables){
  if(length(who)+permute>1){
      opar <- par(mfrow=c(length(who)+permute,1),no.readonly=T,mai=c(0.1,.35,0.05,.35)); on.exit(par(opar))
  }
  for(w in who){
      nrf2 <- nrfall[,w]
      ticks.y <- c(0,.5,1)
      ticks.x <- c(0, 3500, 7000)
      plot(nrf2,pch='.',col='blue',cex=3.33,ylim=c(-.12,1.03), xaxt = 'n', yaxt='n')
      if(w == 2){
        axis(4, at=ticks.y, labels=ticks.y, las=2)
      } else {
        axis(2, at=ticks.y, labels=ticks.y, las=2)
      }
      if(w == 3){
        axis(1, at=ticks.x, labels=ticks.x, las=2)
      }
      #axis(1, at=c(0,3500, 7000), labels=c(135000, 292500, 330000), las=1)
      #exonic <- which(snp.tables[[w]][mm,'exon'])
      #points(exonic,rep(1.01,length(exonic)),pch='.')
      indelic <- which(snp.tables[[w]][mm,'indel'])
      #points(indelic,rep(1.03,length(indelic)),pch='.',col='red')
      abline(h=min.nr.frac, lwd=.5)
      # Chr1 deserts in strain who & ny:
      des.who <- seg.mask(des[[w]][[1]][,1],len=des[[w]][[1]][,3])
      des.ny  <- seg.mask(des[[7]][[1]][,1],len=des[[7]][[1]][,3])
      des.who2 <- which(des.who[mm])
      des.ny2  <- which(des.ny[mm])
      points(des.who2,rep(-.03,length(des.who2)),pch='.',col='slateblue1')
      points(des.ny2, rep(-.09, length(des.ny2)),pch='.',col='steelblue4')
      # mark approx loc of desert ends:
      edge.l <- sum(mm & snp.tables[[w]]$chr=='Chr1' & snp.tables[[w]]$pos<(desert.start))
      edge.r <- sum(mm & snp.tables[[w]]$chr=='Chr1' & snp.tables[[w]]$pos<(desert.start+desert.length))
      #abline(v=edge.l, col='green')
      #abline(v=edge.r, col='green')
      ll<-loess(nrf~x,data.frame(nrf=nrf2^pwr,x=1:n),span=span)
      lines((predict(ll))^(1/pwr),col='red', lwd=2.5)
      legend(x=5200, y=1.08,legend=st.loc(w), bty='n')
  }
  if(plot && permute){
      shuf <- sample(nrf2,length(nrf2))
      plot(shuf,pch='.',col='blue',cex=.5,ylim=c(min.nr.frac,1), main=paste(st.loc(w),'(Shuffled)'))
      #abline(v=edge.l, col='green')
      #abline(v=edge.r, col='green')
      ll<-loess(nrf~x,data.frame(nrf=shuf^pwr,x=1:n),span=span)
      lines((predict(ll))^(1/pwr),col='red')
  }
}


###
# pairs plots of nonref 'allele frequencies', 7 strains
#
nrf.pairs <- function(who=c(4,1,2,5,7,3,6), diag.sas=FALSE, snp.tables=full.tables.01.26.14,
	              mask=seg.mask(2250000,2750000,snp.tables=snp.tables), pch=20,debug=F){
  # make a pairs plot of non-reference allele frequencies in a specified region

  nrfall <- spacial.freq(who,region=mask,permute=F,plot=F,snp.tables=snp.tables)
  colnames(nrfall) <- names(snp.tables)
  nrfall <- nrfall[,who]  # permute/drop columns, if who != 1:7
  # sas <- lapply(who, show.allele.scatter)  ##  ?? seemingly unused; not sure why it was here
  if(diag.sas){
    pairs(nrfall,pch=pch,cex=.2,xlim=0:1,ylim=0:1,col='blue'
          ,lower.panel=function(x,y,...){text(.50,.50,round(cor(x,y),2))}
          ,diag.panel = lapply(who, function(z) {show.allele.scatter(strain=z)})
         )
  } else {
    pairs(nrfall,pch=pch,cex=.2,xlim=0:1,ylim=0:1,col='blue'
          ,lower.panel=function(x,y,...){text(.50,.50,round(cor(x,y),2))}
         )
  }
  if(debug){return(nrfall)}
}

###
# like above, but (for paper) just 6 vs 1335 plus italy vs wales
# 'who' lists strains, *excluding 1335*, in desired plot order
#
nrf.6plus1 <- function(who=c(1,2,5,4,3,6), min.cover=21, max.cover=150,
                       snp.tables=full.tables.01.26.14,
                       #mask=seg.mask(2250000,2750000-1,snp.tables=snp.tables),
                       mask=NULL,
                       allmin=T, sample=10000, pch=20, cex=.2, ell=FALSE, dummy=F, export=F,
                       julie.filter1=0, julie.filter2=F){
    if(!require(compactr)){
      cat('***\n*ERROR: "compactr" not loaded; install.packages() needed?\n***')
    }
    if(dummy){
      # dummy data to speed plot format debugging
      nrfall <- matrix(runif(700),ncol=7)
      colnames(nrfall) <- names(snp.tables)
    } else {
      nrfall <- spacial.freq(c(who,7), region=mask, permute=F, plot=F,
                             min.cover=min.cover, max.cover=max.cover, min.nr.frac=.1, max.nr.frac=1.0,
                             snp.tables=snp.tables, allmin=allmin, 
                             julie.filter1=julie.filter1, julie.filter2=julie.filter2)
      if(is.null(mask)){
        ssize <- nrow(snp.tables[[1]])
        cat("null mask", ssize, "positions.\n")
      } else {
        ssize <- sum(mask)
        cat("non-null mask", ssize, "positions.\n")
      }
      cat('nrf.6plus1: From a region of length:', ssize, 'we identified all positions satisfying:',
          '\n', min.cover, '<= coverage <=', max.cover,
          'in *all*', length(unique(c(who,7))), 'isolates,',
          '\nand having 0.1 <= nr.frac <= 1.0 in *at least 1* of them.',
          '\nFrom these', nrow(nrfall), 'positions, we sampled', sample,
          'to plot.\n')
    }
    if(sample < nrow(nrfall)){
      samp <- sort(sample(1:nrow(nrfall),sample))
    } else {
      samp <- 1:nrow(nrfall)
    }
    opar <- par(mar=c(0.5,0,0,0),tck=-.02); on.exit(par(opar))
    if(ell){# 6 + 1 L-shaped layout?
      layout(matrix(c(1:6,rep(0,5),7),nrow=6,ncol=2,byrow=F),heights=c(rep(1,5),1.4)) # more general than mfrow
    } else {# 7 vertical, with gap
      # base graphics seems to carve margins out of heights/widths; compactr seems to to add margins to height/width,
      # so left/bot margin adjustments below become irrelevant, but leaving them in case.
      # '0' row in layout puts space above last plot
      marl <- 1.4 * 0
      marb <- 1.3 * 0
      xy <- 2.54*1.0
      layout(matrix(c(1:6,0,7),nrow=8,ncol=1,byrow=F),heights=lcm(c(rep(xy,5),xy+marb,1.5,xy+marb)),widths=lcm(xy+marl))
    }
    for(st in who){
        lastplot <- (st==who[length(who)])
        #if(lastplot){par(mar=c(4,4,0,1))4
        #plot(nrfall[samp,7], nrfall[samp,st], ylab=colnames(nrfall)[st],
        #     xaxt=ifelse(lastplot,'s','n'), xlab=ifelse(lastplot,'1335',''),
        #     pch=pch, cex=cex, xlim=0:1, ylim=0:1, col='blue')
        eplot(ylab=colnames(nrfall)[st], xlab='1335',
             xlim=0:1, xat=(0:4)/4, xticklab=c(0,'',.5,'',1),
             ylim=0:1, yat=(0:4)/4, yticklab=c(0,'',.5,'',1))
        points(nrfall[samp,7], nrfall[samp,st], pch=pch, cex=cex, col='blue')
    }
    if(ell){
      par(mar=c(4,0,0,1))
      #plot(nrfall[samp,3], nrfall[samp,6], xlab=colnames(nrfall)[3], yaxt='n',
      #   pch=pch, cex=cex, xlim=0:1, ylim=0:1, col='blue')
      eplot(xlab=colnames(nrfall)[3],
             xlim=0:1, xat=(0:4)/4, xticklab=c(0,'',.5,'',1),
             ylim=0:1, yat=(0:4)/4, yticklab=c(0,'',.5,'',1))
      points(nrfall[samp,3], nrfall[samp,6], pch=pch, cex=cex, col='blue')
    } else {
      addxaxis()
      #plot(nrfall[samp,3], nrfall[samp,6], xlab=colnames(nrfall)[3], ylab=colnames(nrfall)[6],
      #   pch=pch, cex=cex, xlim=0:1, ylim=0:1, col='blue')
      eplot(xlab=colnames(nrfall)[3], ylab=colnames(nrfall)[6],
             xlim=0:1, xat=(0:4)/4, xticklab=c(0,'',.5,'',1),
             ylim=0:1, yat=(0:4)/4, yticklab=c(0,'',.5,'',1))
      points(nrfall[samp,3], nrfall[samp,6], pch=pch, cex=cex, col='blue')
    }
    if(export){return(list(sample=samp,nrfall=nrfall))}
}

###
# like above, but write 7 separate .pdf's, using smoothScatter
#
nrf.6plus1smooth <- function(who=c(1,2,5,4,3,6), min.cover=21, max.cover=150,
                             snp.tables=full.tables.01.26.14,
                             #mask=seg.mask(2250000,2750000-1,snp.tables=snp.tables),
                             mask=NULL,
                             allmin=T, sample=10000, pch=20, cex=2, 
                             col='gray66', # color for points in smoothScatter
                             ell=FALSE, dummy=F, export=F,
                             julie.filter1=0, julie.filter2=F, 
                             xform=function(x){x^.25}, ## the default transform in smoothScatter
                             fig.path='', # path prefix for .pdfs
                             smooth=T){
  chatter <- '' # capture chatter ("cat(...)" in initial implementation)
  if(dummy){
    # dummy data to speed plot format debugging
    nrfall <- matrix(runif(700),ncol=7)
    colnames(nrfall) <- names(snp.tables)
  } else {
    nrfall <- spacial.freq(c(who,7), region=mask, permute=F, plot=F,
                           min.cover=min.cover, max.cover=max.cover, min.nr.frac=.1, max.nr.frac=1.0,
                           snp.tables=snp.tables, allmin=allmin, 
                           julie.filter1=julie.filter1, julie.filter2=julie.filter2)
    if(is.null(mask)){
      ssize <- nrow(snp.tables[[1]])
      chatter <- paste(chatter, "null mask ", ssize, " positions.\n", sep='')
    } else {
      ssize <- sum(mask)
      chatter <- paste(chatter, "non-null mask ", ssize, " positions.\n", sep='')
    }
    chatter <- paste(chatter, 
        'nrf.6plus1: From a region of length: ', ssize, ' we identified all positions satisfying:',
        '\n', min.cover, ' <= coverage <= ', max.cover,
        ' in *all* ', length(unique(c(who,7))), ' isolates,',
        '\nand having 0.1 <= nr.frac <= 1.0 in *at least 1* of them.',
        '\nIn these positions, counts <= ', julie.filter1, ' were forced to zero.',
        '\nNonref fraction includes ',
        ifelse(julie.filter2,
               'only the max nonref count.',
               'all nonref counts.'),
        '\nFrom these ', nrow(nrfall), ' positions, we sampled ', min(sample,nrow(nrfall)),
        ' to plot.\n', sep='')
  }
  if(sample < nrow(nrfall)){
    samp <- sort(sample(1:nrow(nrfall),sample))
  } else {
    samp <- 1:nrow(nrfall)
  }
  opar <- par(mar=c(0.5,0,0,0),tck=-.02); on.exit(par(opar))
  pltype <-ifelse(smooth,'smooth-', 'dots-')
  for(st in who){
    pdf(paste(fig.path, '/', pltype, "scatter",names(snp.tables)[st],
              '-1335-julie',julie.filter1,julie.filter2,'.pdf', sep=''),5,5)
    if(smooth){
      smoothScatter(nrfall[samp,7], nrfall[samp,st], 
                    pch=pch, col=col, cex=cex, nrpoints=200, transform=xform,
                    xlab='', xlim=0:1, xaxp=c(0,1,4),
                    ylab='', ylim=0:1, yaxp=c(0,1,4))
    } else {
      plot(nrfall[samp,7], nrfall[samp,st], pch=pch, cex=cex, col='blue',
           xlab='', xlim=0:1, xaxp=c(0,1,4),
           ylab='', ylim=0:1, yaxp=c(0,1,4))
    }
    dev.off()
  }
  if(3 %in% who && 6 %in% who){
    pdf(paste(fig.path, '/', pltype, "scatter", names(snp.tables)[6],'-',  names(snp.tables)[3], 
              '-julie',julie.filter1,julie.filter2,'.pdf',sep=''),5,5)
    if(smooth){
      smoothScatter(nrfall[samp,3], nrfall[samp,6], 
                    pch=pch, col=col, cex=cex, nrpoints=200, transform=xform,
                    xlab='', xlim=0:1, xaxp=c(0,1,4),
                    ylab='', ylim=0:1, yaxp=c(0,1,4))
    } else {
      plot(nrfall[samp,3], nrfall[samp,6], pch=pch, cex=cex, col='blue',
           xlab='', xlim=0:1, xaxp=c(0,1,4),
           ylab='', ylim=0:1, yaxp=c(0,1,4))
    }
    dev.off()
  }
  
  cat(chatter)
  if(export){return(list(sample=samp, nrfall=nrfall, chatter=chatter))}
}

###############################################################################################
#
# make masks for various criteria
#
make.bool.region <- function(start, end, full.length){
	bool <- rep(F, full.length)
	bool[start:end] <- T
	return(bool)
}

make.mask <- function(who=1:7, region=NULL, include.snps=T, 
		min.cover=0,   min.nr.count=0,   min.nr.frac=0.0,
		max.cover=Inf, max.nr.count=Inf, max.nr.frac=1.0,
		negate=F,
		chrs.only=F,
		debug=F,
		snp.tables=full.tables.01.26.14
	){
  # if who is scalar, return 1 Bool vector, else list of 7; order in who irrelevant.
  # region can be 1 Bool vector, or a list of 7 of them; NULL means all positions.
  # chrs.only is applied AFTER negate.
	if(debug){
	  cat('make.mask:',who,include.snps,negate,min.cover,min.nr.count,
			   min.nr.frac, max.cover, max.nr.count, max.nr.frac,chrs.only,'\n')
		print(str(region))
	}
  mask.l <- length(snp.tables[[1]]$Cov) # masks for all strains are this length
  if(chrs.only){
    # only want chromosomes; no mito, plastid, nor BD contigs.
    # I initially tried this:
    #   chrs.only.mask <- substr(snp.tables[[1]]$Chr,1,3)=='Chr'
    # but there are ~200k spots where Chr is NA, and varies strain-to-strain
    # (both N in reference and zero coverage cause this, I think).  Could 
    # patch it via:
    #   chrs.only[is.na(chrs.only)] <- F
    # but I don't like the fact that the result is strain dependant.
    # Instead, **ASSUME** all chrs precede mito in the tables, that mito
    # starts in the same place in all 7, and isn't NA at 1st occurrance.
    # As of 12/13/2015, this is true in both:
    #    ../../../data/full.tables.01.26.14.rda (full, unqfiltered tables)
    # and 
    #    ../../../data/full.tables.02.25.15.rda (full, qfiltered)
    # > rbind(
    #     unlist(lapply(tset[[1]], function(x){match("mitochondria.fasta",x$Chr)})),
    #     unlist(lapply(tset[[3]], function(x){match("mitochondria.fasta",x$Chr)})))
    #          1007     1012     1013     1014     1015     3367     1335
    # [1,] 31301783 31301783 31301783 31301783 31301783 31301783 31301783

    first.mito <- match("mitochondria.fasta", snp.tables[[7]]$Chr)
    if(is.na(first.mito)){
      # probably just have Chr1 tables
      chrs.only.mask <- rep(TRUE, mask.l)
    } else {
      if(first.mito != 31301783){
        cat('MAKE.MASK TABLE ERROR\n')
        return(NULL)
      }
      chrs.only.mask <- logical(mask.l)
      chrs.only.mask[1:(first.mito-1)] <- TRUE
    }
  }

	mask <- vector(mode='list',7)
	for(i in who){
		if(debug){cat('make.mask: i=',i,'\n')}
	  if(min.nr.count > 0 || is.finite(max.nr.count) || min.nr.frac > 0.0 || max.nr.frac < 1.0){
	    # nr needed only in these cases
	    nr  <- snp.tables[[i]]$Cov - snp.tables[[i]]$.match
	    if(min.nr.frac > 0.0 || max.nr.frac < 1.0){
	      # nrf needed only in these cases
	      nrf <- nr / snp.tables[[i]]$Cov
	    }
	  }
		if(  is.list(region)          ){msk <- region[[i]]}
		  else                         {msk <- region}
		if(  is.null(msk)             ){msk <- rep(T, mask.l)}
	    else 
	      if(length(msk) != mask.l) { 
	        cat('MAKE.MASK LENGTH ERROR\n')
	        return(NULL) 
	      }
		if( !include.snps             ){msk <- msk & snp.tables[[i]]$snp == 0}
		if(          min.cover > 0    ){msk <- msk & snp.tables[[i]]$Cov >= min.cover}
		if(is.finite(max.cover)       ){msk <- msk & snp.tables[[i]]$Cov <= max.cover}
		if(          min.nr.count > 0 ){msk <- msk & nr  >= min.nr.count}
		if(is.finite(max.nr.count)    ){msk <- msk & nr  <= max.nr.count}
		if(          min.nr.frac > 0.0){msk <- msk & nrf >= min.nr.frac}
		if(          max.nr.frac < 1.0){msk <- msk & nrf <= max.nr.frac}
		if( negate                    ){msk <- ! msk}
	  if( chrs.only                 ){msk <- msk & chrs.only.mask}
		mask[[i]] <- msk
	}
	if(length(who)==1){
		return(msk)
		}
	names(mask) <- names(snp.tables)[who]
	return(mask)
}
###############################################################################################
###############################################################################################

# Make bool mask for one or more segments defined either by start/stop or start/len pairs
seg.mask <- function(start,stop=NULL,len=NULL,snp.tables=full.tables.01.26.14){
  if(is.null(len)){len <- stop-start+1}
  mask <- rep(F,nrow(snp.tables[[1]]))
  for(i in 1:length(start)){
    for(j in start[i]:(start[i]+len[i]-1)){
      mask[j] <- T
    }
  }
  return(mask)
}


###############################################################################################
###############################################################################################
#
# histograms of non-ref reads at hicov, majority ref positions in desert; '...' for other
# make.mask params:
#
hist.nr <- function(show.counts=0, breaks.ub = 200, region=desert.mask, ...){
	mask <- make.mask(max.nr.frac=0.49999,region=region,...)
	hh.nr <- vector(mode='list',7)
	for(i in 1:7){
		hh.nr[[i]] <- hist(full.tables[[i]][mask[[i]],'Cov'] -
		                   full.tables[[i]][mask[[i]],'.match'], breaks=-1:breaks.ub,plot=F)
		if(show.counts>0) print(hh.nr[[i]]$counts[1:show.counts])
	}
	return(hh.nr)
}
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
#
# histograms of ref reads at hicov, minority ref positions:
#
hist.ref <- function(show.counts=0, breaks.ub = 200, region=desert.mask, ...){
	mask <- make.mask(min.cover=20, min.nr.frac=0.5, region=region, ...)
	hh.ref <- vector(mode='list',7)
	names(hh.ref) <- names(full.tables)
	for(i in 1:7){
		hh.ref[[i]] <- hist(full.tables[[i]][mask[[i]],'.match'], breaks=-1:breaks.ub,plot=F)
		if(show.counts>0) cat(names(hh.ref)[[i]], hh.ref[[i]]$counts[1:show.counts], '\n')
	}
	return(hh.ref)
}
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
show.ref.hist <- function(who=1:7, hrd=hh.ref.desert, hrn=hh.ref.non, both=T){
	opar <- par(mfrow=c(2,2),ask=(length(who)*(both+1)>4),no.readonly=T); on.exit(par(opar))
	xl <- '# ref reads'
	yl <- 'log2(positions+1)'
	for(i in who) {
		if(both){
		plot(0:99,log2(hrd[[i]]$counts[1:100]+1),type='h',xlab=xl,ylab=yl,
			main=paste(names(hrd)[[i]],'(desert)'))
		text(80,.9*log2(1+max(hrd[[i]]$counts[1:100])),
			paste('n=',sum(hrd[[i]]$counts[1:100])))
		}
		plot(0:99,log2(hrn[[i]]$counts[1:100]+1),type='h',xlab=xl,ylab=yl,
			main=paste(names(hrn)[[i]],'(100k post-desert)'))
		text(80,.9*log2(1+max(hrn[[i]]$counts[1:100])),
			paste('n=',sum(hrn[[i]]$counts[1:100])))
	}
}
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
#
# Simple estimate of err rates:
#
doc.simple.err <- '%
    %
    Over all high-coverage (default: $\\ge 20$) sites in a specified region (default:big desert), with at most a
    specified number (default: 5) of nonref reads, optionally excluding called SNP positions (default: exclude SNPs),
    calculate the fraction of nonref reads in each strain.%
    %
'
simple.err <- function(min.cover=20, max.nr.count=5, region=desert.mask, include.snps=F, snp.tables=full.tables.01.26.14){
	mask <- make.mask(min.cover=min.cover, max.nr.count=max.nr.count, region=region, include.snps=include.snps,
	     		  snp.tables=snp.tables)
	out <- numeric(7)
	for(i in 1:7){
		n <- sum(snp.tables[[i]]$Cov   [ mask[[i]] ])
		m <- sum(snp.tables[[i]]$.match[ mask[[i]] ])
		out[i] <- (n-m)/n
	}
	names(out) <- names(snp.tables)
	return(list(simple.err.rates=out))
}
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
#
# Histogram of nonref reads showing portion attributable to simple error model:
#   over all high-coverage (default: >= 20) sites
#   in a specified region (default: 100k AFTER big desert),
#   with at most a specified fraction (default: 0.4999) of nonref reads,
#   optionally excluding called SNPs (default: no SNPS),
# tabulate number of nonref reads in each strain up to a max (default: 5), + number predicted
# by simple error model
#
s.e.hist <- function(
				maxnr=5,
				min.cover=20,
				max.nr.frac=0.4999,
				region=desert.mask,
				include.snps=F,
				s.e.rates=simple.err()
		){
	cover.range <- vector(mode='list',7)
	nref.xtab   <- vector(mode='list',7)
	p           <- vector(mode='list',7)
	est         <- vector(mode='list',7)
	mask <- make.mask(region=region,min.cover=min.cover,max.nr.frac=max.nr.frac, include.snps=include.snps)
	for(i in 1:7){
		cover.range[[i]] <- range(full.tables[[i]]$Cov[ mask[[i]] ])
		nref.xtab[[i]]   <- table( data.frame(
			coverage = full.tables[[i]]$Cov[ mask[[i]] ],
			nonref   = full.tables[[i]]$Cov[ mask[[i]] ] - full.tables[[i]]$.match[ mask[[i]] ]
			))
		if(i==1){
			# globalize one
			xt <<- nref.xtab[[i]]
			# print(summary(nref.xtab[[i]]))
		}
		p[[i]]   <- matrix(NA,nrow(nref.xtab[[i]]), maxnr+1)
		est[[i]] <- numeric(maxnr+1)
		for(j in 0:maxnr){
			if(j %in% colnames(nref.xtab[[i]])){
				p  [[i]][,j+1] <- dbinom(j,as.integer(rownames(nref.xtab[[i]])), s.e.rates[[1]][i])
				est[[i]][j+1]  <- sum(nref.xtab[[i]][,j+1]*p[[i]][,j+1])
			} else {
				cat('Really?', i, j)
			}
		}
	}
	return(list(simple.err.rates=s.e.rates[[1]], cover.range=cover.range, nref.xtab=nref.xtab, p=p, est=est))
}
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
#
# render the histograms
#
show.histos <- function(who=1:7,xmax=50,both=F,max2=10,se.est=NULL,hist.nr=hh.nr){
	opar <- par(no.readonly=T); on.exit(par(opar))
	# layout plots to fill a landscape page, up to 2 x 4 per page
	n1 <- length(who)
	n2 <- n1 * (1+both)
	numcols <- min(ceiling(n2/2),4)
	numrows <- min(n2,2)
	if(both){
		par(mfcol=c(numrows,numcols))
	} else {
		par(mfrow=c(numrows,numcols))
	}
	if(n2>numrows*numcols){par(ask=T)}
	# axis labels
	xl <- 'Reads'
	yl <- 'log2(count+1)'
	xrange  <- 0:xmax
	for(i in who){
		plot(0,0,type='n',
			xlim=c(-0.5,xmax),
			ylim=c(0,log2(1+max(hist.nr[[i]]$counts[xrange+1]))),
			xlab=xl,ylab=yl,main=paste(st.loc(i),'non-ref @ ref maj'))
		if(is.null(se.est)){
			# estimate errors by simple straight line fit thru 1st 2 points on semi-log plot
			n <- hist.nr[[i]]$counts[1]
			p <- hist.nr[[i]]$counts[2]/n
			est <- n*p^(0:max2)
		} else {
			# use error estimates provided
			est <- se.est[[i]]
		}
		# range over which we bothered to estimate errors
		range.est <- 0:(length(est)-1)
		points(range.est,log2(1+est),col='blue',pch=18)
		# 'corrected' count, i.e. after subtracting predicted errors
		corr <- hist.nr[[i]]$counts[1+range.est] - est
		# overcorrected, if negative
		over <- -pmin(0,corr)
		if(max(over)>0){
			# if any overcorrected, show via red line, but stop at zero after last nonzero
			nzs <- min(length(est)-1,1+max(which(over>0)))
			lines(0:(nzs-1), log2(1+over[1:nzs]), col='red' ,type='s')
		}
		lines(range.est-0.49, log2( 1+pmax(0,corr) ), col='green',type='s')
		lines(xrange-0.5,log2(1+hist.nr[[i]]$counts[xrange+1]),type='s')
		if(both){
			plot(xrange,log2(1+hh.ref[[i]]$counts[xrange+1]),type='s',xlab=xl,ylab=yl,
				main=paste(st.loc(i),'ref @ non-ref maj'))
		}
		print(who)
		print(hist.nr[[i]]$counts[xrange+1])
		print(est)
		print(corr)
	}
}
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
#
# Five functions for analysis of coverage dip around SNPs.  Overall goal:
#
#  Plot average coverage in a window around SNPs selected in some region
#  Secondarily, do same plots, but split the SNPs into 4 categories:
#    * those with a *low*  frequency of non-reference bases compared to coverage
#    * those with a *high* frequency of non-reference bases
#      (threshhold set below, currently 0.6)
#    * those where left context is entirely exonic or
#    * entirely non-exonic
#
#  dip.summary: generates the summary stats;  Returns: big list with named elements
#  showdip: use that to gen one plot for one strain
#  hilodip1: 3 plots for one strain
#  hilodip7: 3 for each of 7
#  dip.plots: 1 + 3 plots per strain, optionally as .pdf
#
# 2014-08-17: adding tables as param, defaulting to full.  'Region' defaults are historical,
#   from my preliminary analysis based on 100k region past big desert; no longer relevant,
#   so changing default to 'all of snp.tables'.
# 2014-09-02: added dip.thesesnps, to allow filtering by union.snps
#
dip.summary <- function (
	dip.region.name,				# name the region, e.g., perhaps 'Chr1-all';
	dip.region.start   = 1,				# which region to process; default = all
	dip.region.end     = nrow(snp.tables[[1]]),
	dip.win		   = 100,                       # window of +/- this
	dip.hilo.threshold = .5,			# threshold for separating low- from hi-freq nonref bases
	dip.hop            = 1,                         # min step from one SNP to the next counted
	dip.snpless	   = FALSE,			# should that interval also be SNP-free?
	dip.thesesnps	   = NULL,			# if so, based on per-strain snps (NULL) or provided vector?
	dip.dice	   = 10,			# how finely should I dissect nonref counts?
	snp.tables         = full.tables.01.26.14       # default to full tables (tho that's probably overkill)
) {
	# initialize tables
        n.strains <- length(snp.tables)
	dipc  <- vector(mode='list',    length=n.strains) # per strain coverage in window
	dipcl <- vector(mode='list',    length=n.strains) # per strain coverage in window, low group
	dipch <- vector(mode='list',    length=n.strains) # per strain coverage in window, hi group
	dipcx <- vector(mode='list',    length=n.strains) # per strain coverage in window, exon group
	dipcy <- vector(mode='list',    length=n.strains) # per strain coverage in window, nonexon group
	dipm  <- vector(mode='list',    length=n.strains) # per strain matches in window
	dipml <- vector(mode='list',    length=n.strains) # per strain matches in window
	dipmh <- vector(mode='list',    length=n.strains) # per strain matches in window
	dipmx <- vector(mode='list',    length=n.strains) # per strain matches in window, exon group
	dipmy <- vector(mode='list',    length=n.strains) # per strain matches in window, nonexon group
	dipn  <- vector(mode='numeric', length=n.strains) # per strain SNP counts
	dipnl <- vector(mode='numeric', length=n.strains) # per strain SNP counts, low group
	dipnh <- vector(mode='numeric', length=n.strains) # per strain SNP counts, high group
	dipnx <- vector(mode='numeric', length=n.strains) # per strain SNP counts, exon group
	dipny <- vector(mode='numeric', length=n.strains) # per strain SNP counts, nonexon group
	dipe  <- vector(mode='numeric', length=n.strains) # per strain count of 'errors' (reads != match or max non-match) at SNP pos
	dipav <- vector(mode='numeric', length=n.strains) # per strain average coverage
	dipavm<- vector(mode='numeric', length=n.strains) # per strain average matches
	dipcxk<- vector(mode='list',    length=n.strains) # per strain coverage in window, exon group, by count
	dipcyk<- vector(mode='list',    length=n.strains) # per strain coverage in window, nonexon group, by count
	dipmxk<- vector(mode='list',    length=n.strains) # per strain matches in window, exon group, by count
	dipmyk<- vector(mode='list',    length=n.strains) # per strain matches in window, nonexon group, by count
	dipnxk<- vector(mode='list',    length=n.strains) # per strain nonref counts, exon group, by count and position
	dipnyk<- vector(mode='list',    length=n.strains) # per strain nonref counts, nonexon group, by count and position

	for(st in 1:n.strains){
		# initialize per strain tables
		dipc [[st]] <- vector(mode='numeric',length=2*dip.win+1)
		dipcl[[st]] <- vector(mode='numeric',length=2*dip.win+1)
		dipch[[st]] <- vector(mode='numeric',length=2*dip.win+1)
		dipcx[[st]] <- vector(mode='numeric',length=2*dip.win+1)
		dipcy[[st]] <- vector(mode='numeric',length=2*dip.win+1)
		dipm [[st]] <- vector(mode='numeric',length=2*dip.win+1)
		dipml[[st]] <- vector(mode='numeric',length=2*dip.win+1)
		dipmh[[st]] <- vector(mode='numeric',length=2*dip.win+1)
		dipmx[[st]] <- vector(mode='numeric',length=2*dip.win+1)
		dipmx[[st]] <- vector(mode='numeric',length=2*dip.win+1)
		dipmy[[st]] <- vector(mode='numeric',length=2*dip.win+1)
		dipmy[[st]] <- vector(mode='numeric',length=2*dip.win+1)
		dipcxk[[st]]<- matrix(0,dip.dice,2*dip.win+1)
		dipcyk[[st]]<- matrix(0,dip.dice,2*dip.win+1)
		dipmxk[[st]]<- matrix(0,dip.dice,2*dip.win+1)
		dipmyk[[st]]<- matrix(0,dip.dice,2*dip.win+1)
		dipnxk[[st]]<- matrix(0,dip.dice,2*dip.win+1)
		dipnyk[[st]]<- matrix(0,dip.dice,2*dip.win+1)
		dipn  [st]  <- 0
		dipnl [st]  <- 0
		dipnh [st]  <- 0
		dipnx [st]  <- 0
		dipny [st]  <- 0
		dipe  [st]  <- 0

		# start summarizing
		dipav [st]  <- mean(snp.tables[[st]]$Cov[(dip.region.start):(dip.region.end)])
		dipavm[st]  <- mean(snp.tables[[st]]$.match[(dip.region.start):(dip.region.end)])

		# which snp set do we use?  null param means SAMTools per strain snp calls; non-null
		# means use provided vector (e.g. union of snp calls over all 7)
		if(is.null(dip.thesesnps)){
			the.snps <- snp.tables[[st]]$snp
		} else {
		        the.snps <- dip.thesesnps
		}

		# iterate through SNPs in region of interest (at least dip.win from region ends)
		# where are they?
		snpsat <- which(the.snps[ (dip.region.start+dip.win):(dip.region.end-dip.win-1) ] == 1) +
		       	  		   dip.region.start+dip.win - 1
		# look at the j-th SNP in that interval, which is at position i
		j <- 1
		while(j <= length(snpsat)){
			i <- snpsat[j]
			if(the.snps[i] != 1){
			  cat('***SNPdip error, j=',j,'i=',i,'should be snp***\n')
			}
			cov.slice <- snp.tables[[st]]$Cov   [i+(-dip.win:dip.win)]
			mat.slice <- snp.tables[[st]]$.match[i+(-dip.win:dip.win)]
			nrf.slice <- cov.slice - mat.slice
			dipn[st]   <- dipn[st] + 1
			dipc[[st]] <- dipc[[st]] + cov.slice
			dipm[[st]] <- dipm[[st]] + mat.slice
			if(snp.tables[[st]]$.match[i] > (1-dip.hilo.threshold) * snp.tables[[st]]$Cov[i]){
			  # SNPs with a LOW frequency of non-reference bases
			  dipnl[st]   <- dipnl[st] + 1
			  dipcl[[st]] <- dipcl[[st]] + cov.slice
			  dipml[[st]] <- dipml[[st]] + mat.slice
			} else {
			  # SNPs with a HIGH frequency of non-reference bases
			  dipnh[st]   <- dipnh[st] + 1
			  dipch[[st]] <- dipch[[st]] + cov.slice
			  dipmh[[st]] <- dipmh[[st]] + mat.slice
			}
			if(all(snp.tables[[st]]$exon[(i-dip.win):(i-1)])){
			  # SNPs with dip.win EXONIC bases to left
			  dipnx[st]   <- dipnx[st] + 1
			  dipcx[[st]] <- dipcx[[st]] + cov.slice
			  dipmx[[st]] <- dipmx[[st]] + mat.slice
			  for(k in 1:(dip.dice-1)){
			    dipnxk[[st]][k,]             <- dipnxk[[st]][k,]             + (nrf.slice==k)
			    dipcxk[[st]][k,nrf.slice==k] <- dipcxk[[st]][k,nrf.slice==k] + cov.slice[nrf.slice==k]
			    dipmxk[[st]][k,nrf.slice==k] <- dipmxk[[st]][k,nrf.slice==k] + mat.slice[nrf.slice==k]
			  }
			  k <- dip.dice
			  dipnxk[[st]][k,]             <- dipnxk[[st]][k,]             + (nrf.slice>=k)
			  dipcxk[[st]][k,nrf.slice>=k] <- dipcxk[[st]][k,nrf.slice>=k] + cov.slice[nrf.slice>=k]
			  dipmxk[[st]][k,nrf.slice>=k] <- dipmxk[[st]][k,nrf.slice>=k] + mat.slice[nrf.slice>=k]
			} else {
			  if(all(!snp.tables[[st]]$exon[(i-dip.win):(i-1)])){
			    # SNPs with dip.win NON-exonic bases to left
			    dipny[st]   <- dipny[st] + 1
			    dipcy[[st]] <- dipcy[[st]] + cov.slice
			    dipmy[[st]] <- dipmy[[st]] + mat.slice
			    for(k in 1:(dip.dice-1)){
			      dipnyk[[st]][k,]             <- dipnyk[[st]][k,]             + (nrf.slice==k)
			      dipcyk[[st]][k,nrf.slice==k] <- dipcyk[[st]][k,nrf.slice==k] + cov.slice[nrf.slice==k]
			      dipmyk[[st]][k,nrf.slice==k] <- dipmyk[[st]][k,nrf.slice==k] + mat.slice[nrf.slice==k]
			    }
			    k <- dip.dice
			    dipnyk[[st]][k,]             <- dipnyk[[st]][k,]             + (nrf.slice>=k)
			    dipcyk[[st]][k,nrf.slice>=k] <- dipcyk[[st]][k,nrf.slice>=k] + cov.slice[nrf.slice>=k]
			    dipmyk[[st]][k,nrf.slice>=k] <- dipmyk[[st]][k,nrf.slice>=k] + mat.slice[nrf.slice>=k]
			  }
			}
			# Cov is total; .match is ref; max of ACGT is presumably the alt allele; rest are 'errors'
			dipe[st]   <- dipe[st] + snp.tables[[st]]$Cov[i] -
			                         snp.tables[[st]]$.match[i] -
			                     max(snp.tables[[st]][i,c('a','g','c','t')])

			# to control the influence of closely-spaced SNPs, the next SNP must be at least dip.hop
			# positions past the current one (at position i); e.g., with default dip.hop=1, they could be
			# adjacent; with dip.hop=3, the two immediately following positions (if either/both are SNPs)
			# cannot be the next 'i' for the above tabulations (but i+3 could be, if it is a SNP).
			# Additionally, if dip.snpless is true, there must be at least dip.hop-1 *SNP-free* positions
			# immediately left of the next SNP to be tabulated.  E.g., if dip.hop=3 && dip.snpless, we skip
			# to the next SNP having at least 2 non-SNPs to its left.

			j <- j+1
			while(j <= length(snpsat) && i + dip.hop > snpsat[j]){
			  # j-th SNP too close; skip it.
			  if(dip.snpless){
			    i <- snpsat[j]
			  }
			  j <- j+1
			}
		}
	}

	return(list(dip.region.name    = dip.region.name,
	            dip.region.start   = dip.region.start,
                    dip.region.end     = dip.region.end,
		    dip.win            = dip.win,
 		    dip.hilo.threshold = dip.hilo.threshold,
		    dip.hop            = dip.hop,
		    dip.snpless	       = dip.snpless,
		    dip.thesesnps      = dip.thesesnps,
		    dip.dice	       = dip.dice,
		    snp.tables.signature = length(snp.tables[[1]]$cov),	# imprecise, but e.g. discriminates chr1 vs full
		    dipc	       = dipc,
		    dipcl	       = dipcl,
		    dipch	       = dipch,
		    dipcx	       = dipcx,
		    dipcy	       = dipcy,
		    dipm	       = dipm,
		    dipml	       = dipml,
		    dipmh	       = dipmh,
		    dipmx	       = dipmx,
		    dipmy	       = dipmy,
		    dipcxk	       = dipcxk,
		    dipcyk	       = dipcyk,
		    dipmxk	       = dipmxk,
		    dipmyk	       = dipmyk,
		    dipn	       = dipn,
		    dipnl	       = dipnl,
		    dipnh	       = dipnh,
		    dipnx	       = dipnx,
		    dipny	       = dipny,
		    dipnxk	       = dipnxk,
		    dipnyk	       = dipnyk,
		    dipe	       = dipe,
		    dipav	       = dipav,
		    dipavm	       = dipavm,
		    svn.stamp.wlr.r    = svn.stamp.wlr.r()	# value likely held in persistent cache, so stamp it
		    ))

}
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
showdip <- function(
	sst    = st,			# strain number
	ds     = dip.sum,		# dip summary list
	see    = c(T,T,T,F,T,T,'both'),	# which data should we show;  parallels legwords, etc., except
	       	 			# last selects which subset: 'low' nonref freq; 'hi'; or 'both'
	mainttl= paste('Coverage around SNPs (', ds$dip.region.name, ') ', st.loc(sst), sep=''),
	subttl = NULL			# plot subtitle
) {
	# extract relevant parts of list
	sdipname <- ds$dip.region.name		# name of region
	sdipwin  <- ds$dip.win		 	# how wide is our window
	sdipm    <- ds$dipm[[sst]] 		# matches
	sdipe    <- ds$dipe[sst]   		# errs
	sdipav   <- ds$dipav[sst]  		# region-wide average coverage
	sdipavm  <- ds$dipavm[sst] 		# region-wide average matches
	sdipc    <- switch(see[7], low=ds$dipcl[[sst]], hi=ds$dipch[[sst]], both=ds$dipc[[sst]]) # coverage
	sdipn    <- switch(see[7], low=ds$dipnl [sst],  hi=ds$dipnh [sst],  both=ds$dipn [sst] ) # number of SNPs

	SNPpos <- sdipwin + 1

	legwords <- c(
		paste('=',sdipn),
		'Coverage',
		'Cov minus mismatches',
		'2*Matches @ SNP',
		'Cov minus "errs"',
		'Average Coverage'
	)
	#default see:  c( T,  T,  T,  F,  T , T)  # which data should we show; parallels legwords, etc.
	legsymbs    <- c('N','o','+','*','x',NA)  # N=SNPcount, o=coverage, +=matches, */x: see below
	legcol      <- c('black', 'black','grey', 'black','gray','blue')
	legwd       <- c(rep(NA,5),0.5)           # only average is a line; other 'wd's = NA

	sdipca <- sdipc/sdipn                                  # average total coverage
	sdipma <- sdipm/sdipn                                  # average match coverage
	sdipstar <- 2 * sdipma[SNPpos]                         # '*' point: twice match coverage at SNP position
	sdipx    <- sdipca[sdipwin+1] - sdipe/sdipn            # 'x' point: coverage minus errors at SNP position

	# set y range for plot
	yrange <- NULL
	if(see[2]){yrange <- range(yrange, sdipca)}           # range of coverage
	if(see[3]){yrange <- range(yrange, sdipma[-SNPpos])}  # range of matches, excluding SNP position
	if(see[4]){yrange <- range(yrange, sdipstar)}         # range of the '*' point
 	if(see[5]){yrange <- range(yrange, sdipx)}            # range of the 'x' point
 	if(see[6]){yrange <- range(yrange, sdipav)}           # range of average line

	plot(0,0, type='n',                                   # no plot, just establish axes, labels
		xlim=c(-sdipwin,sdipwin),  ylim=yrange,
		main= mainttl,
		sub = subttl,
		xlab='Relative Position from a SNP ',ylab='Coverage'
	)

	abline(v = -25,col='grey',lwd=0.3)
	abline(v = +25,col='grey',lwd=0.3)
	if(see[6]){abline(h=sdipav,    lwd=legwd[6],    col=legcol[6])} # horiz line at average coverage
	if(see[5]){points(0, sdipx,    pch=legsymbs[5], col=legcol[5])} # 'x'
	if(see[4]){points(0, sdipstar, pch=legsymbs[4], col=legcol[4])} # '*'
	if(see[3]){points((-sdipwin:sdipwin)[-SNPpos],sdipma[-SNPpos], pch=legsymbs[3], col=legcol[3])} # matches
	if(see[2]){points((-sdipwin:sdipwin),sdipca)}                   # coverage; default sym & color

	see.what <- as.logical(see[1:6])
	legend(ifelse(any(sst==c(0)),'topleft','bottomleft'),           # legend; placement is hand-hacked
		bty='n',
		lwd=legwd[see.what],
		col=legcol[see.what],
		legend=legwords[see.what],
		pch=legsymbs[see.what])
}
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
hilodip1 <- function(sst, ds, setpar=TRUE) {
	if(setpar){
		omar <- par('mar')
		omar[3] <- 0
		opar <- par(mfrow=c(1,3),oma=c(0,0,3,0),mar=omar); on.exit(par(opar))
	}
	# print(par())
	showdip(sst, ds, c( T,  T,  T,  F,  T, T, 'both'), main=NULL, subttl='All SNPs')
	showdip(sst, ds, c( T,  T,  F,  F,  F, T, 'low' ), main=NULL, subttl=paste('SNPS with nonref <', ds$dip.hilo.threshold, 'coverage'))
	showdip(sst, ds, c( T,  T,  F,  F,  F, T, 'hi'  ), main=NULL, subttl=paste('SNPs with nonref >=',ds$dip.hilo.threshold, 'coverage'))
	mtext(paste('Coverage around SNPs (', ds$dip.region.name, ') ', st.loc(sst), sep=''), line=1, outer=TRUE)
}

###############################################################################################
###############################################################################################
hilodip7 <- function(ds) {  ## 2014-08-17 default used to be global dip.sum
	opar <- par(mfrow=c(2,3)); on.exit(par(opar))
	for(strain in 1:length(ds$dipn)){
		hilodip1(strain,ds,setpar=FALSE)
	}
}
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
dip.plots <- function(
	dip.region.name,         		# perhaps 'chr1-all'; used in file names & plot title
	dip.region.start = 1,			# defaults to all of snp.tables
	dip.region.end   = nrow(snp.tables[[1]]),
	write.pdfs       = T,			# if T, write PDFs, else just show onscreen
	snp.tables       = full.tables.01.26.14 # default to full tables (tho that's probably overkill)
){
       #dip.sum <<- dip.summary(dip.region.name, dip.region.start, dip.region.end)	# globalize it
	dip.sum  <- dip.summary(dip.region.name, dip.region.start, dip.region.end)	# 2014-08-17: don't
	if(write.pdfs) {
		pdf(paste(todaydir(), 'SNPcoverage-', dip.region.name, '.pdf',sep=''), onefile=T)
	}
	for(st in 1:length(dip.sum$dipn)){
		showdip(st, dip.sum)  # 2014-08-17: hmm... this default seems to duplicate 1st of hilodip; delete?
	}
	if(write.pdfs) {dev.off()}

	if(write.pdfs) {
		pdf(paste(todaydir(), 'SNPcoverage-hilo-', dip.region.name, '.pdf',sep=''), onefile=T,height=8.5,width=11)
	}
	hilodip7(dip.sum)
	if(write.pdfs) {dev.off()}
}
###############################################################################################


###############################################################################################
#
# since the 5 are so similar, maybe it will be useful to merge their read counts
#
# typical use: 'tables <- c(tables,merge.tables())' will create 8th 'strain'
#
merge.tables <- function(strains=c(1,2,4,5,7), tables=full.tables){
  cat('Merging', strains[1])
  merged <- tables[[strains[1]]]
  for(i in strains[-1]){
    cat(', ', i)
    merged$a <- merged$a + tables[[i]]$a
    merged$g <- merged$g + tables[[i]]$g
    merged$c <- merged$c + tables[[i]]$c
    merged$t <- merged$t + tables[[i]]$t
    merged$n <- merged$n + tables[[i]]$n
    merged$.match <- merged$.match + tables[[i]]$.match
    merged$Cov <- merged$Cov + tables[[i]]$Cov
    merged$snp <- merged$snp + tables[[i]]$snp
    merged$indel <- merged$indel | tables[[i]]$indel
  }
  cat('.\n')
  lofm <<-list(merged)
  names(lofm) <<- paste('M',paste(strains,collapse=''),sep='')
  return(lofm)
}

###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
# scatter plot and histogram of %-non-ref vs coverage.
#   Blue circles:   nonref count >= thresh && coverage <= clip
#   Blue triangles: nonref count >= thresh && coverage >  clip
#   Blue Histo bars: all blue points
#   Gray circles/hist:   thresh - nonref count >= dt (darker grey closer to thresh)
#   Grey '+'s: 0 < nonref count < thresh - dt (darker closer to thresh; not in histo)
#
# todo: marginal histo along Y?  lower cutoff? show some of nref=0? lighten grey?
# add ref<dt to histo, but ylim ~ 1.5 * maxblue?
# todo: clarify thresh/dt/mask in output
#
# 8/12/14: starting to clean up use of global tables; still need to address many '<<-' -- wlr
# 8/20/14: also reordering some params, changing defaults to try to make it more logical -- wlr
#

###
# Show coverage vs nonref-frac via lattice smoothScatter.
# many params inherited from s.a.s, esp. mask and m2 to select points, nref & total for nonref-frac
# this plot is *in addition* to the discrete scatter plot., and includes obnly 'blue' points.
# plausibly we should use it *instead* of the discrete plot, but save that for a later day.
#
show.allele.scatter.smooth <- function(strain,mask,m2,clip,nref,total,ratlab, snp.tables=full.tables.01.26.14){
	library(lattice);
	print(xyplot(snp.tables[[strain]]$Cov[mask][m2] ~ (nref/total)[m2], pch = '+',
		panel = function(...) {panel.smoothScatter(nrpoints = 500, ...)},
		ylab = 'Coverage (ref + max nonref)', xlab = ratlab,
		ylim = c(0, clip), xlim = c(0, 1), main = st.loc(strain)))
}

###
# Make plot legend
#
show.allele.scatter.legend <- function(thresh,nref,m2,tot,dt,colorramp){
	nrcount <- vector('numeric',thresh)
	for(i in 0:(thresh-1)){
		nrcount[i+1] <- sum(nref==i)
	}
	nrcount[thresh+1] <- sum(m2)

	legend <- c(
		paste('Nonref ==',format(0:(thresh-1),width=2),': ', format(nrcount[-(thresh+1)], width=7)),
		paste('Nonref >=',format(   thresh,   width=2),': ', format(nrcount[  thresh+1 ], width=7)),
		paste('Total:         ',                             format(tot,                  width=7))
	)

	# legend symbol example: thresh=3, dt=1
	# legend has thresh+2 rows: 0 1 2 3 tot
	# symbols:                  - - + * -
	# sizes:                    - 2 3 5 -
	legpch <- c(NA, rep( 3,thresh-1),20,NA)
	legcex <- c(NA, rep(.2,thresh-dt-1),rep(.3,dt), 0.5, NA)
	legcol <- c(colorramp, NA)

	# monospaced font to align numbers better
	par(family='mono')
	legend('topright', bty='n', cex=0.5, legend=legend, pch=legpch, col=legcol, pt.cex=legcex)
	par(family='sans')
}

###
### Docs look better in Latex, but I don't want to separate them from code, so embedding them here as char variables.
### See scatter-plus.rnw
###
doc.show.allele.scatter.contours <-
  ' %
  Because counts are integer, (fraction, cov) points all appear along discrete contours.  E.g., points with 3 non-ref
  reads all appear along the line $(x=3/y, y)$.  These patterns are confusing at first;
  \\texttt{show.allele.scatter.contours}, triggered by the \\texttt{contours=TRUE} parameter, optionally draws some
  of these contours, so your eye can get used to seeing them.  Probably best to turn this off to reduce clutter once
  it ceases to be surprising.
  '

show.allele.scatter.contours <- function(show.contours,clip){
	if(show.contours){
		linesat <- c(2,4,6,8,10,40,42,44,46)
		cov <- 10:clip
		for (nr in linesat) {lines(     nr/cov, cov, lty = 1, lwd = 0.5, col = 'yellow')}
		for ( r in linesat) {lines((cov-r)/cov, cov, lty = 1, lwd = 0.5, col = 'cornflowerblue')}
	}
}

###
# make and decorate the discrete scatter plot
#
show.allele.scatter.discrete <-
    function(strain, mask, m2, clip, nref, total, ratlab, ylab, npos, show.contours, thresh, dt, colorramp, dither, defaultmai,
	     ncells, show.loess, color.snps, show.legend=T, show.main.ttl=T, plot.c=T, snp.tables=full.tables.01.26.14,
	     main.title
	     ){
        if(plot.c){
            #par(mai=defaultmai*c(0,1,1,1))
            plot(0, 0, type = 'n', ylab = ylab, xlab = '', ylim = c(0, clip), xlim = c(0, 1), main = main.title)}
        if(show.legend){
            show.allele.scatter.legend(thresh,nref,m2,npos,dt,colorramp)
        }
	show.allele.scatter.contours(show.contours,clip)
        nz <- (nref > 0)
	nrnz <- nref[nz]
	ttnz <- total[nz]
	x <- (nrnz/ttnz)
	y <- ttnz
	npts <- sum(nz)
	mypch <- rep(20,npts)        # by default, all points plot as small, blue circles
	mycex <- rep(.5,npts)
	lo <- (nrnz < (thresh-dt))
	hi <- nrnz >= thresh
	mypch[lo] <- 3               # use small plus for lowest nonref counts
	mycex[lo] <- .2
	mycol <- colorramp[1+pmin(nrnz,thresh)]
	mycex[((thresh-dt) <= nrnz) & (nrnz < thresh)] <- .3
	clipped <- (y > clip)        # if y too high to show, plot small up-triangle @ y=clip+1
	y[clipped] <- clip+1
	mypch[clipped] <- 17
	mycex[clipped] <- .6
	total.mean <- mean(total)
	total.sd   <- sd(total)
	if(color.snps){
	  # mask & nz snps
	  mnzsnp <- snp.tables[[1]]$snp[mask][nz]
	  for(i in 2:7){
	    mnzsnp <- mnzsnp + snp.tables[[i]]$snp[mask][nz]
	  }
	  if(exists('big.consistent')){
	    # just keep consistent ones
	    cat('pruning:', sum(mnzsnp > 0 &!(big.consistent[[2]][mask][nz])), '\n')
	    mnzsnp[!(big.consistent[[2]][mask][nz])] <- 0
	  }
	  mypch[mnzsnp>0] <- utf8ToInt('0')+mnzsnp[mnzsnp>0]
	  mycex[mnzsnp>0] <- .5
	  mycol[mnzsnp>0] <- 'green'
	  mycol[snp.tables[[strain]]$snp[mask][nz]==1] <- 'red'

	  # debug: str(mycol); str(mypch); str(mycex); str(x); str(y); str(snp.tables[[strain]]$snp[mask][nz]==1);
	  # debug: str(total); str(total[nz]); str(mnzsnp==0); str(total[nz][mnzsnp == 0]); str(nref[nz][mnzsnp == 0]);

	  # using GLOBAL unsnps.stats
	  unsnps <-
	    mnzsnp == 0 &
	    nref[nz]/total[nz] < 0.2 &
	    total.mean-1.5*total.sd <= total[nz] &
	    total[nz] <= total.mean+1.5*total.sd
	  if(!exists('unsnp.stats')){
	    unsnp.stats <<- data.frame(strain=1:7, loc='', date=0, points=0, SNPs=0,
                                       unSNPs=0,  unSNPReads=0,  unSNPNonref=0,  unFrac=0,
                                       unSNPs1=0, unSNPReads1=0, unSNPNonref1=0, unFrac1=0,
                                       stringsAsFactors=F)
          }
          unsnp.stats[strain,'loc']    <<- st.loc(strain)
          unsnp.stats[strain,'date']   <<- st.loc(strain,date=T)
          unsnp.stats[strain,'points'] <<- npts
          unsnp.stats[strain,'SNPs']   <<- sum(mnzsnp>0)
          unsnp.stats[strain,'unSNPs'] <<- sum(unsnps)
          unsnp.stats[strain,'unSNPReads']  <<- sum(total[nz][unsnps])
          unsnp.stats[strain,'unSNPNonref'] <<- sum(nref[nz][unsnps])
          unsnp.stats[strain,'unFrac'] <<- unsnp.stats[strain,'unSNPNonref'] / unsnp.stats[strain,'unSNPReads']
          unsnp.stats[strain,'unSNPs1']      <<- sum(unsnps & nref[nz]>1)
          unsnp.stats[strain,'unSNPReads1']  <<- sum(total[nz][unsnps & nref[nz]>1])
          unsnp.stats[strain,'unSNPNonref1'] <<- sum(nref [nz][unsnps & nref[nz]>1])
          unsnp.stats[strain,'unFrac1'] <<- unsnp.stats[strain,'unSNPNonref1'] / unsnp.stats[strain,'unSNPReads1']
          #cat(st.loc(strain), 'Points:', npts, 'SNPs:', sum(mnzsnp>0), 'UnSNPs:',unsnps.count, 'nonref reads:' unsnps\n')
        }
      	if(dither){
	  dx <- rnorm(length(x),0,.002)
	  dy <- rnorm(length(y),0,.8)
	} else {
	  dx <- 0; dy <- 0
	}
	points(x+dx, y+dy, pch=mypch, col=mycol, cex=mycex)
	abline(h=total.mean-2.0*total.sd,lwd=.5,lty=2,col='grey')
	abline(h=total.mean+2.0*total.sd,lwd=.5,lty=2,col='grey')
	abline(h=total.mean,             lwd=.5,      col='grey')
        abline(v=1/2/ncells,lwd=.5,col='red')
        if(show.loess){
          # add loess fit of read coverage vs nonref fraction of above-thresh (blue) points
          hix <- x[hi]
          hiy <- y[hi]
          perm <- order(hix)
          sorted.hix <- hix[perm]
          sorted.hiy <- hiy[perm]
          lines(sorted.hix,predict(loess(sorted.hiy~sorted.hix)),col='red')
        }

        # print('i got to the end of discrete')
	return(list(nrnz= nrnz, ttnz= ttnz, x=x, y=y, npts=npts, mypch=mypch, mycex=mycex, lo=lo, hi=hi,
              mycol=mycol, clipped=clipped))
}

###
# modeled on code in shared-snps.rnw
# is default 'tab' meaningful?  replace it with full.tables.01.26.14 ??  But it's just 1, not 7 --- wlr 8/12/14
#
nref.nuc.new <- function(snp.table = tab, mask=T, thresh.count=0, thresh.rate=0.0){
	# get read count for max nonref nuc
	nref <- apply(snp.tables[mask, c('a', 'g', 'c', 't')], 1, max)
	# where does nref count match a (g,c,t, resp) count
	as <- ifelse(nref == snp.tables[mask,'a'],1,0)
	gs <- ifelse(nref == snp.tables[mask,'g'],2,0)
	cs <- ifelse(nref == snp.tables[mask,'c'],3,0)
	ts <- ifelse(nref == snp.tables[mask,'t'],4,0)
	# most positions will show 3 zeros and one of 1:4, so max identifies max nonref count;
	# ties broken arbitrarily  (a<g<c<t)
	merge <- pmax(as,gs,cs,ts)
	# but if max nonref count is zero or below threshold, return 0
	merge[nref == 0 | nref < thresh.count] <- 0
	merge[nref/snp.tables[mask,'Cov'] < thresh.rate] <- 0
	return(merge)
}

make.non.refs <- function(snp.tables=full.tables.01.26.14, mask=T,thresh.count=c(0,2,4),thresh.rate=c(0.00,0.05,0.10)){
  n <- ifelse(length(mask)==1,nrow(snp.tables[[1]]),sum(mask))
  non.refs <- vector('list',length(thresh.count))
  for(i in 1:length(thresh.count)){
    non.refs[[i]] <- matrix(0,nrow=n,ncol=7)
    for(j in 1:7){
      non.refs[[i]][,j] <- nref.nuc.new(snp.table = snp.tables[[j]],mask=mask,thresh.count=thresh.count[i],thresh.rate=thresh.rate[i])
    }
    colnames(non.refs[[i]]) <- names(tables)
    rownames(non.refs[[i]]) <- paste(tables[[1]]$chr[mask],':',tables[[1]]$pos[mask],sep='')
  }
  return(non.refs)
}

# big.non.refs <- make.non.refs(snp.tables = full.tables.01.26.14)

find.consistent <- function(nr){
  nr.max <- apply(nr,1,max)
  nr.min <- apply(nr,1,function(x){ifelse(max(x)==0,0,min(x[x>0]))})
  return(nr.min == nr.max)
}
# big.consistent  <- lapply(big.non.refs, find.consistent)
# big.consistent.count <- unlist(lapply(big.consistent, sum))
# big.consistent.count
# big.consistent.count/nrow(tables[[1]])


###
# make histogram portion
#

show.allele.scatter.hist <- 
  function(nref, total, ratlab, thresh, dt, colorramp, defaultmai, hist.bins, binom.fobs, ncells, 
           olay, scatter, oversample, models, model.humpth, model.dromedary, model.hump2af, 
           modelD.double, modelD.olay, modelE.erate, main.title, one.grey, all.blue, 
           hist.max, hist.plain, snps
){
  par(mai=c(ifelse(scatter,0.5,0.8), defaultmai[2], 0.5, defaultmai[4]))
  mybreaks <- (0:hist.bins)/hist.bins  ## oddly 'breaks=20' was miscalculated by hist in 2 of 7 plots

  # Note: hist uses bins half-open bins (x,x+delta], except 1st bin, by default (include.lowest=T) 
  # is closed i.e. in our case, if dt=thresh, this will include the 90+% of positions with 0 nonref,
  # so best to set dt < thresh.  This is currently forced in show.allele.scatter, but not if you 
  # call show.allele.scatter.hist directly.
  
  # lines marked '#<<-' used to be global assignments; now exported via deglobal/debug instead	#<<-
  
  nrat <- nref/total											                                                      #<<-
  
  # calculate, but don't plot the blue histo; needed to set bluemax/hclip/ylim
  if(F){ ## && cell.thresh && ncells > 0){
    hist.thresh <- hist(nrat[nrat>=1/2/ncells & nref > 0], breaks=mybreaks, plot=F)
  } else {
    hist.thresh <- hist(nrat[nref>=thresh & nref > 0], breaks=mybreaks, plot=F)
  }
  bluemax <- min(2000, max(hist.thresh$counts))
  if(is.na(hist.max)){
    hclip <- 100*ceiling(1.2*bluemax/100) ## clip vertical axis in histo at a round number > max blue
  } else {
    hclip <- hist.max
  }

  # now actually plot histos (lightest grey/tallest first)
  # if one.grey, just plot the highest grey; else overlay histos for increasingly many nonref reads, 
  # using grey shades in colorramp.  Always end with the blue histo
  # But if hist.plain, color it all blue
  for(nr in seq(from=thresh-dt, to=thresh, by=ifelse(one.grey,dt,1))){
    hist.nr <- hist(nrat[nref>=nr], breaks=mybreaks, main=main.title, 
                    xlab='', ylab='Count', 
                    ylim=c(0,hclip), xlim=0:1,
                    xaxt=ifelse(scatter,'n','s'), add=(nr>(thresh-dt)), 
                    col=ifelse(hist.plain, 'blue', colorramp[nr+1]))
    if(nr == (thresh-dt)){ # save first (biggest) histo
      hist.nrmin <- hist.nr
    }
    if(hist.plain){break}
  }
  # if snp calls (a 0-1 vector) is provided, overlay histo of number of snp calls vs r
  if(!is.null(snps)){
    hist(nrat[snps==1], breaks=mybreaks, main='', xlab='', ylab='', 
         ylim=c(0,hclip), xlim=0:1, xaxt=ifelse(scatter,'n','s'), add=TRUE, 
         col='yellow', border=NA)
  }
  
  if(hist.plain){
    greybig <- NA # for debug blob
  } else {
    # show max counts of any clipped bars (typically grey)
    greybig <- hist.nrmin$counts > hclip
    if(any(greybig)){
      text(hist.nrmin$mids[greybig],1.00*hclip,hist.nrmin$counts[greybig],cex=.5,srt=60,pos=1)
    }
  } 
  # final histo decorations:
  lines(c(0,1),c(0,0)) # histogram floor
  mtext(ifelse(hist.plain,'R',ratlab),side=1,line=ifelse(scatter,0.5,2)) # x axis label; if no scatter, leave room for ticks

  # Models A-D:
  # goal is to overlay lines (in red/green/orange...) with approx same number of positions as in .4 hump,
  # based on simple binomial models of nonref/total reads having same total counts as observed at these positions.
  #
  doc.show.allele.scatter.hist.1 <<- '%
    In models A$\\ldots$D we attempt to account for the ``mappability\'\' bias seen at SNPs.  E.g., at a position where
    we expect $n$ reference fragments and $n$ nonreference fragments to be sequenced, we typically observe the fraction
    of nonref reads actually mapped to the locus to be about 0.4.  We model this by assuming that all reference reads
    map successfully, but there is a factor $\\alpha$ such that only $\\alpha \\cdot n$ of the non-ref reads map
    successfully (see also \\texttt{SNPdip}), and $\\alpha \\cdot n/(n+\\alpha \\cdot n) \\approx .4$.  Solving, we have
      $$  \\alpha = 0.4/(1-0.4) .  $$
    More generally, if we are \\emph{given} that the \\emph{true} non-ref allele frequency at a given locus is $f$, and
    we \\emph{observe} nonref freq $f_{obs}$, then we assume that they are related by
      $$  f_{obs} = \\frac{\\alpha \\cdot f \\cdot n}{\\alpha \\cdot f \\cdot n + (1-f) \\cdot n}  $$
    so
      $$  f_{obs} \\cdot (\\alpha \\cdot f + (1-f)) = \\alpha \\cdot f  $$
      $$  f_{obs} \\cdot \\alpha \\cdot f - \\alpha \\cdot f = - f_{obs} \\cdot (1-f)  $$
      $$  \\alpha \\cdot \\frac{f}{1-f} = \\frac{f_{obs}}{1-f_{obs}}  $$
    which has the simple interpretation that the observed odds ratio of non-ref to ref is the actual odds ratio
    attenuated by the factor $\\alpha$.  Then, if the actual number of fragments sequenced was $c_{hidden}$ with
      $$  \\alpha \\cdot f \\cdot c_{hidden} + (1-f) \\cdot c_{hidden} = c_{obs}  $$
    then
      $$  c_{hidden} = \\frac{c_{obs}}{\\alpha \\cdot f + (1-f)} .  $$
    E.g., for a site with actual nonref frequency $f_{hidden} = 50\\%$, with observed nonref freq$f_{obs}=40\\%$, and
    with observed coverage $c_{obs}=60$, then $\\alpha = .4/(1-.4) = .67$ and the ``true\'\' coverage at that site is
      $$  c_{hidden} = 60/(2/3 \\cdot 1/2+(1-1/2)) = 72  $$
    (36 ref and 36 nonref, but 1/3 of the latter failed to map, so the \\emph{observed} data are 24 nonref of 60 total,
    or 40\\% nonref.)

    For converting between observed and hidden values we have:
      $$  \\alpha \\cdot \\frac{f}{1-f} = \\frac{f_{obs}}{1-f_{obs}}  $$
    so, letting $\\beta = \\alpha \\cdot f/(1-f)$, we have:
      $$  \\beta = f_{obs}/(1-f_{obs}) ;\\ \\  f_{obs} = \\beta(1-f_{obs}) ; \\ \\ f_{obs} = \\beta/(1+\\beta) .  $$
  '

  # canonicalize model.humpth
  if(length(model.humpth) == 1){
    model.humpth <- c(0, model.humpth)
  }
  # cat('model.humpth=', model.humpth, '\n')
  
  # Identify blue points nominally assumed to be in the .4 hump
  if(T){ ## || !cell.thresh || ncells == 0){ # (I tried 2 ways, but prefer the 1st)
    m3  <- (nref >= thresh) & (model.humpth[1] <= nrat) & (nrat <= model.humpth[2])		#<<-
  } else {
    m3  <- (1/2/ncells < nrat) & (nrat <= model.humpth)							                  #<<-
  }
  m3[is.na(m3)] <- F  # exclude NAs (where total==0)							                    #<<-
  nm3 <- sum(m3)											                                                #<<-
  m3.reads <- sum(total[m3])
  if(model.dromedary && ! modelD.olay){
      m3hi <- F
      nm3hi <- 0
      m3hi.reads  <- 0
  } else {
      # in the 2-hump case, also count those in the .8 hump
      m3hi <- (nref >= thresh) & (nrat > model.humpth[2])
      m3hi[is.na(m3hi)] <- F  # exclude NAs (where total==0)
      nm3hi <- sum(m3hi)
      m3hi.reads <- sum(total[m3hi])
  }
      
  deglobal <- list(nrat=nrat, model.humpth=model.humpth, m3=m3, nm3=nm3,
                   model.dromedary=model.dromedary,
                   model.hump2af=model.hump2af,
                   m3hi=m3hi, nm3hi=nm3hi, m3hi.reads=m3hi.reads)					              #<<-

  # style parameters for model plots
  model.lwd <- 3
  model.cex <- .9

  # Model A (black curve):
  # ======================
  # all SNPs have the same non-reference allele frequency 'f', and all sites have the same observed coverage caobs
  # = the average coverage observed at SNPs.

  totblack <- NA # for printout, in case skip code below
  legtxt <- character(0)
  legcol <- character(0)
  fa <- 0.5												#<<-
  faobs <- binom.fobs											#<<-
  alphaa <- faobs/(1-faobs) * (1-fa)/fa									#<<-
  caobs <- round(m3.reads/nm3)										#<<-
  deglobal <- c(deglobal, fa=fa, faobs=faobs, alphaa=alphaa, caobs=caobs)				#<<-
  if(grepl('A',models,fixed=TRUE)){
    sampA <- rbinom(oversample*nm3,rep(caobs,oversample*nm3),faobs)/caobs				#<<-
    #hist(sampA,breaks=mybreaks,main='',xlab='',ylim=c(0,hclip),xlim=0:1,xaxt='n',add=T,border='red', include.lowest=F)
    hhA <- hist(sampA, breaks=mybreaks, plot=F)							        #<<-
    # print(str(hhA))
    lines(hhA$mids,hhA$counts/oversample,pch=20,cex=model.cex,lwd=model.lwd,type='o',col='black')
    totblack <- sum(hhA$counts)/oversample
    legtxt <- c(legtxt, 'Model A')
    legcol <- c(legcol,'black')
    deglobal <- c(deglobal, list(sampA=sampA), list(hhA=hhA))						#<<-
  }

  # Model B (red curve):
  # ====================
  # Sample chidden, attenuated by alpha.  should be the same (but isn't, quite...).

  totred <- NA
  if(grepl('B',models,fixed=TRUE)){
    fb <- 0.5												#<<-
    fbobs <- binom.fobs											#<<-
    alphab <- fbobs/(1-fbobs) * (1-fb)/fb								#<<-
    cbhidden <- round(m3.reads/nm3/(alphab*fb + (1-fb)))						#<<-
    sampB <- rbinom(oversample*nm3,rep(cbhidden,oversample*nm3),fb)*alphab/(cbhidden*fb*(1+alphab))	#<<-
    hhB <- hist(sampB, breaks=mybreaks, plot=F)								#<<-
    lines(hhB$mids,hhB$counts/oversample,pch=20,cex=model.cex,lwd=model.lwd,type='o',col='red')
    totred <- sum(hhB$counts)/oversample
    legtxt <- c(legtxt, 'Model B')
    legcol <- c(legcol,'red')
    deglobal <- c(deglobal, fb=fb, fbobs=fbobs, alphab=alphab, cbhidden=cbhidden, list(sampB=sampB), list(hhB=hhB)) #<<-
  }

  # Model C (Green curve):
  # ======================
  # Read counts are simulated from a binomial with freq 'fb' and simulated coverage 'cb', where fb and cb are
  # estimated so as to match the actual coverage observed in the real data and a target frequency fbobs=binom.fobs
  # after attenuating the non-ref reads by factor 'alpha'.  I.e., we suppose all reference reads map to the given
  # site, but only alpha fraction of the nonref reads succeed in mapping.  E.g., if the nonref fraction at SNP
  # positions averages .4 (=fbobs=binom.fobs) and the nonref allele freq is actually .5, then we want to choose
  # 'alpha' = .4/.6*.5/.5=2/3 as above, and cb=cbobs/[alpha*fb+1-fb]

  totgreen <- NA
  if(grepl('C',models,fixed=TRUE)){
    fc <- .5												#<<-
    fcobs <- binom.fobs											#<<-
    alphac <- fcobs/(1-fcobs) * (1-fc)/fc								#<<-
    sampC <- rbinom(oversample*nm3,rep(total[m3],oversample),binom.fobs)/rep(total[m3],oversample)	#<<-
    # hist(sampC,breaks=mybreaks,main='',xlab='',ylim=c(0,hclip),xlim=0:1,xaxt='n',add=T,border='green', include.lowest=F)
    hhC <- hist(sampC, breaks=mybreaks, plot=F)								#<<-
    lines(hhC$mids,hhC$counts/oversample,pch=20,cex=model.cex,lwd=model.lwd,type='o',col='green')
    totgreen <- sum(hhC$counts)/oversample
    legtxt <- c(legtxt, 'Model C')
    legcol <- c(legcol,'green')
    deglobal <- c(deglobal, fc=fc, fcobs=fcobs, alphac=alphac, list(sampC=sampC), list(hhC=hhC))	#<<-
  }

  # Model D (Orange curve):
  # =======================
  # nm3 SNPs in n diploid cells sampled from a population in Hardy-Weinberg equilibrium.  Assumes NO linkage.
  # assumes WHAT for mappability?????????  assumes WHAT allele freq spectrum?????

  totorange   <- NA
  totorangehi <- NA
  nm3x <- NA
  if(grepl('D',models,fixed=TRUE)){
    # beta = alpha*f/(1-f) ; fobs = beta/(1+beta)
    alphad <- alphaa  ## ???										                  #<<-
    nm3x <- nm3 * ifelse(modelD.double, 2, 1)
    sampD1   <- rbinom(oversample*nm3x, 2*ncells, 0.5)						#<<-
    sampD1hi <- rbinom(oversample*nm3hi,2*ncells, model.hump2af)
    fd   <- sampD1  /2/ncells										                  #<<-
    fdhi <- sampD1hi/2/ncells
    fdobs   <-pmin(1,alphad*fd  /(alphad*fd  +1-fd  )) ## pmin in case expr is 1+epsilon; rbinom chokes	#<<-
    fdhiobs <-pmin(1,alphad*fdhi/(alphad*fdhi+1-fdhi)) ## pmin in case expr is 1+epsilon; rbinom chokes
    sampD2   <- rbinom(oversample*nm3x, rep(total[m3  ],oversample),fdobs  )/rep(total[m3  ],oversample)#<<-
    sampD2hi <- rbinom(oversample*nm3hi,rep(total[m3hi],oversample),fdhiobs)/rep(total[m3hi],oversample)
    # hist(sampD2,breaks=mybreaks,main='',xlab='',ylim=c(0,hclip),xlim=0:1,xaxt='n',add=T,border='orange',lwd=5, include.lowest=F)
    hhD   <- hist(sampD2,   breaks=mybreaks, plot=F)							#<<-
    hhDhi <- hist(sampD2hi, breaks=mybreaks, plot=F)
    orng.show <- ifelse(hist.plain, floor(model.humpth[1]*hist.bins), 1) : hist.bins # hide low bins for plain version
    lines(hhD$mids[orng.show],hhD$counts[orng.show]/oversample,pch=20,cex=model.cex,lwd=model.lwd,type='o',col='orange')
    if(!model.dromedary){
        lines(hhD$mids,hhDhi$counts/oversample,pch=20,cex=model.cex,lwd=model.lwd,type='o',col='orange',lty='dashed')
        lines(hhD$mids,(hhD$counts+hhDhi$counts)/oversample,pch=20,cex=model.cex/2,lwd=model.lwd/2,type='o',col='orange',lty='dashed')
    }
    if(modelD.olay){
      mdo.mid <- floor(0.4 * hist.bins) : ceiling(0.6 * hist.bins)
      mdo.y1 <- max(hhD$counts[mdo.mid])/oversample * 1.2
      if(FALSE){
        mdo.y2 <- hhD$counts[hist.bins]/oversample  * 1.05  # label above last orange pt
      } else {
        mdo.y2 <- hist.thresh$counts[hist.bins]  * 1.2    # label above last blue pt
      }
      mdo.col <- 'darkblue'
      lines(model.humpth,rep(mdo.y1,2), lwd=2, col=mdo.col)
      text(mean(model.humpth), mdo.y1, labels=nm3, pos=3, col=mdo.col)
      lines(c(model.humpth[2],1),rep(mdo.y2,2), lwd=2, col=mdo.col)
      text((model.humpth[2]+1)/2, mdo.y2, labels=nm3hi, pos=3, col=mdo.col)
    }
    totorange   <- sum(hhD$  counts)/oversample
    totorangehi <- sum(hhDhi$counts)/oversample
    legtxt <- c(legtxt, paste('Model D:',ncells,'cells'))
    legcol <- c(legcol,'orange')
    deglobal <- c(deglobal, fd=fd, fdobs=fdobs, alphad=alphad, list(sampD1=sampD1), list(sampD2=sampD2), list(hhD=hhD),
                            fdhi=fdhi, fdhiobs=fdhiobs, list(sampDhi1=sampD1hi), list(sampD2hi=sampD2hi), list(hhDhi=hhDhi)) #<<-
  }

  # Model E (Yellow curve):
  # =======================
  # model read errors (grey bars?)
  #
  # given
  #   - the specified error rate modelE.erate,
  #   - the number of positions presumed to contain error (i.e., all the grey bars; equiv, positions with max nonref < thresh), and
  #   - observed coverage distribution at those positions
  totnzgrey <- NA
  totgrey <- NA
  if(grepl('E',models,fixed=TRUE)){
    cat('\nModel E:\n')

    # m4 is mask for 'error' points--all those with nonref count below thresh
    m4  <- (nref < thresh)
    m4.uncovered <- sum(total[m4] == 0)
    m4[total==0] <- F  # prevent NAs (div by total==0)
    nm4 <- sum(m4)											#<<-
    #m4.reads <- sum(total[m4])
    #cat('***',length(total), nm3,nm4, sum(is.na(total)),sum(is.na(total[m4])),sum(total==0),sum(total[m4]==0),m4.uncovered,'\n')

    # draw binomial sample of the size & coverage defined by m4 @ rate modelE.erate,
    # histogram it, overlay lines.
    ####
    #### Should I do multinomial sample into 4 bins, take max? On reflection, non-indp errors make this impossible
    ####
    sampE <- rbinom(oversample*nm4,rep(total[m4],oversample), modelE.erate)				#<<-
    hhE <- hist((sampE/rep(total[m4],oversample))[sampE>0], breaks=mybreaks, plot=F)			#<<-
    lines(hhE$mids,hhE$counts/oversample,pch=20,cex=model.cex,lwd=model.lwd,type='o',col='yellow')

    # since grey typically clipped, print info about model match for (a) zero nonref positions and
    # (b) first few bins in histogram.  (a) done separately since default dt < thresh excludes them.
    real.zeros <- sum(nref == 0 & total != 0)
    sampE.zeros <- sum(sampE==0)
    # cat('Positions with no nonref reads--in data:', real.zeros,
    #     '; in simulationE:', sampE.zeros/oversample, '\n')		#this is now row 1 of grey.df
    howmany <- ifelse(T,length(mybreaks),20)				# T => all, else 1st 20.
    skim <- 1:howmany
    mybreaks.char <- format(mybreaks[1:(howmany+1)],width=5)
    grey.df <- data.frame(bin=paste('(', mybreaks.char[-howmany-1], '-', mybreaks.char[-1], ']', sep=''),
    	       		  bin.counts.thresh.minus.dt = hist.nrmin$counts[skim],
			  bin.counts.blue            = hist.thresh$counts[skim],
			  bin.counts.grey            = hist.nrmin$counts[skim]-hist.thresh$counts[skim],
			  bin.counts.ModelE	     = hhE$counts[skim]/oversample,
			  stringsAsFactors=F)
    grey.df <- rbind(list('[0.000]',NA,NA,real.zeros,sampE.zeros/oversample),grey.df)
    row.names(grey.df) <- 1:(howmany+1)
    print(grey.df)

    totnzgrey <- sum(hhE$counts)/oversample
    totgrey <- totnzgrey + sampE.zeros/oversample
    legtxt <- c(legtxt, paste('Model E:')) ### ,ncells,'cells')) erate?
    legcol <- c(legcol,'yellow')
    deglobal <- c(deglobal, nm4=nm4, list(sampE=sampE), list(hhE=hhE), real.zeros=real.zeros, sampleE.zeros=sampE.zeros) #<<-
  }

  if(length(legtxt)>0 && ! hist.plain){
    legend('topright',legend=legtxt,col=legcol,pch=rep(20,length(legtxt)),pt.cex=model.cex,lty=1,cex=.8,bty='n',lwd=model.lwd)
  }
  # (conclusion: C has somewhat higher variance than A/B; real data higher still; D highest for small ncell)

  print(rbind(c('blue','nm3','nm3x','nm3hi','red','black','green','orange','ornghi','nzgrey', 'grey'),
              c(sum(hist.thresh$counts),nm3,nm3x,nm3hi,totred,totblack,totgreen,totorange,totorangehi,totnzgrey,totgrey)))

  if(ncells>0 && ! hist.plain){abline(v=1/2/ncells,lwd=.5,col='red')}

  if(!is.null(olay)){points(olay[1,],olay[2,],type='s',col='green')}

  # return lots for debugging...
  return(list(thresh=thresh, dt=dt, hist.bins=hist.bins, smooth=smooth, hist.thresh=hist.thresh, 
              bluemax=bluemax, hclip=hclip, hist.nrmin=hist.nrmin, greybig=greybig, olay=olay,
              deglobal=deglobal, hist.max, hist.plain))
}

show.allele.scatter <-
  function(
    strain  = 1,		   # which strain to analyze
    mask    = T,	     # which positions thereof to analyze
    thresh  = 10, 		 # 'blue' points require >= thresh nonref reads
    dt     	= thresh-1,# number of below-thresh levels to plot (in grey); default=all non-0 nonrefs
    ncells	= 0,		   # nominal number of cells in sequencing culture; place red vertical line at
                       #   x=1/2/ncells in both plots (offscale if 0); also important in modelD sims
    smooth  = F,		   # make separate version of scatter plot using lattice smooth?
    show.main.ttl = T, # show main title (with first of scatter, histo)?
    scatter = T, 		   # show scatter plot? (next few only relevant if T)
      clip       	= 200, # clip y in scatter plot here
      dither     	= F, 	 # dither points in scatterplot to alleviate overplotting?
      plot.c     	= T,	 # of limited use?  if F, no plot call to set up axes, labels; do so outside
      show.ylab 	= T,	 # show y axis label?
      show.legend	= T,	 # show legend?
      show.contours = F, # discrete ratios put all points on integer a/b contours; show some?
      show.loess 	= F,	 # show loess fit through blue points?
      color.snps 	= F, 	 # more detailed annotatation of snps in scatter:
                         #   1) for any position called a SNP in any strain, plot symbol is
                         #      changed to small digit showing how many strains
                         #   2) colored red if called SNP in this strain; else green
    hist	      = T, 	 # show histogram? (next few only relevant if T)
      hist.bins   = 40,  # number of histogram bins
      models     	= '',  # hist may include overlay of expected distr of freqs under any of 5  
                         #   models ABCDE; see show.allele.scatter.hist for details
      oversample 	= 10,	 # Models ABCDE: sample this many times as many reads as real data
      binom.fobs 	= 0,	 # Models ABCD: observed fraction nonref reads at SNPS (~0.4)
      model.humpth= 0.60,# Models ABCD: pair c(l,h); if scalar, pretend its c(0,h); 
                         # blue pts in [l,h] are assumed to be in .4 hump
      model.dromedary=T, # Models ABCD: if T, one hump; if F, two humps (bactrian), count pts above m.humpth
      model.hump2af=0.75,# Models ABCD: if NOT dromedary, assume 2nd hump comes from this allele freq.
      modelD.double = F, # Model D: (for IT/Wales) use 2x middle hump est for D curve
      modelD.olay   = F, # Model D: overlay count totals above 2 humps
      modelE.erate=0.002,# Model E: estimated read error rate, e.g. tot nonref/tot coverage, excl SNPs
      one.grey	= F,     # if T, only plot one shade of grey (=tallest) histo; else dt of them
      hist.max  = NA,    # if nonNA, override auto calc of hist ylim
      hist.plain= F,     # if T, omit various plot decorations (ncells vline, legend, clipped counts)
      show.snps = F,     # if T, overlay histo of SNP counts on histo
    olay    = NULL,    # purpose unclear; if nonNULL, a 2xn matrix of x,y coords of green step-fn overlaid on hist
    mul.col	= T,		   # purpose unclear; changes color ramp; may be buggy... Affects scatter + histo
    debug	  = F,	     # return lots of debugging info
    snp.tables=full.tables.01.26.14
  ) {
    # Set some plot params
    opar <- par(no.readonly=T); on.exit(par(opar))
    defaultmai <- par('mai')
    if(scatter && hist){layout(matrix(1:2,nrow=2),heights=c(3,2))}
    main.title <- NULL
    if(show.main.ttl){ main.title <- st.loc(strain) }
    
    # sanity checks on args
    dt <- min(dt, thresh-1)   # nonsensical if >= thresh.
    
    # lines marked '#<<-' used to be global assignments; export through deglobal/debug instead
    
    # extract key coverage data (into 'mask'-compressed vectors, not genomic coordinates)
    npos <- ifelse(length(mask)>1,sum(mask),nrow(snp.tables[[strain]]))
    nref <- apply(snp.tables[[strain]][mask, c('a', 'g', 'c', 't')], 1, max) #<<-
    ref  <- snp.tables[[strain]]$.match[mask] 	      	   	     	           #<<-
    total <- ref + nref							                                         #<<-
    m2 <- nref >= thresh   # blue mask (relative to 'mask'-compressed coords)
 
    deglobal <- list(nref=nref, ref=ref, total=total)
    
    # more plot setup: colors, labels
    if(mul.col){
      colorramp <- c(gray(.4*(((thresh-1):0)/thresh)+0.55),'blue')}
    else{
      colorramp <- 'blue' }
    ratlab <- '(max nonref) / (ref + max nonref)'
    ylab <- ifelse(show.ylab, 'Coverage (ref + max nonref)', NA)
    
    # call helpers to do the actual plots
    if(smooth) {
      show.allele.scatter.smooth(strain, mask, m2, clip, nref, total, ratlab, snp.tables=snp.tables)
    }
    
    discr <- NULL
    if(scatter){
      discr <- show.allele.scatter.discrete(strain, mask, m2, clip, nref, total, ratlab, ylab, npos,
                                            show.contours, thresh, dt, colorramp, dither, defaultmai, 
                                            ncells, show.loess, color.snps, show.legend = show.legend,
                                            main.title=main.title, plot.c=plot.c, snp.tables=snp.tables)
      main.title <- NULL # don't print title if we did it here
    }
    
    hist.blob <- NULL
    if(hist){
      if(show.snps){
        snps <- snp.tables[[strain]]$snp[mask]
      } else {
        snps <- NULL
      }
      hist.blob <- show.allele.scatter.hist(nref, total, ratlab, thresh, dt, colorramp, defaultmai, 
                                            hist.bins, binom.fobs, ncells, olay, scatter, oversample, 
                                            models         = models, 
                                            model.humpth   = model.humpth, 
                                            model.dromedary= model.dromedary,
                                            model.hump2af  = model.hump2af, 
                                            modelE.erate   = modelE.erate,
                                            modelD.double  = modelD.double,
                                            modelD.olay    = modelD.olay,
                                            main.title=main.title, one.grey=one.grey, 
                                            hist.max=hist.max, hist.plain=hist.plain,
                                            snps=snps)
    }
    
    if(debug){
      print('debug of sas, returns big debug blob')
      deblob <- list(strain=strain, thresh=thresh, dt=dt, mask=mask, clip=clip, hist.bins=hist.bins,
                     show.contours=show.contours, smooth=smooth, dither=dither, discr=discr, 
                     hist.blob=hist.blob, ncells=ncells, olay=olay, deglobal=deglobal)
      return(deblob)
    }
  }

write.allele.scat <- function(strains=1:7, pdf.out=T, randmask = rmask(1e5), color.snps=T, dither=T, ncells=5,
		              snp.tables=full.tables.01.26.14) {
  filename <- paste(todaydir(), 'allele-scatter-CD', ifelse(pdf.out,'.pdf','.png'),sep='')
  if(pdf.out){
    pdf(file=filename, width=8.5, height=11)
  }
  else{
    png(file=filename, width=8.5, height=11)
  }
  on.exit(dev.off())
  for (strain in strains){
    for(nc in ncells){
      show.allele.scatter(strain,thresh=5,mask=randmask,clip=200,hist.bins=40,debug=F,show.loess=T,
                          color.snps=color.snps, dither=dither,
                          binom.fobs=.4,ncells=nc,modelC=T,modelD=T,snp.tables=snp.tables)
    }
  }
  if(length(snp.tables)==8){
    for(nc in ncells){
      show.allele.scatter(8,thresh=10,mask=randmask,clip=1200,hist.bins=100,debug=F,show.loess=T,
                          binom.fobs=.4,ncells=5*nc,modelC=T,modelD=T)
    }
  }
}

write.allele.scatters <- function(data='chr1-100k', strains=1:7, mask=T, pdf.out=F, smooth=F,
		                  snp.tables=full.tables.01.26.14) {
	filename <- paste(todaydir(), 'allele-scatter-', data,
                          ifelse(pdf.out,'.pdf','.png'),sep='')
	if(pdf.out){
		pdf(file=filename, width=8.5, height=11)
	}
	else{
		png(file=filename, width=8.5, height=11)
	}
	on.exit(dev.off())
	for (strain in 1:strains){
		show.allele.scatter(strain, mask = mask, smooth=smooth, hist.b=T)
	}
}
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
# simple hardy-weinberg calc
hw <- function(n=250, p=3/4){
	out <- vector(mode='list',5)
	q <- 1-p
	for(k in 2*(1:5)){
		i <- 0:k
		#cat('\n', k/2, 'founders:\n')
		data <- rbind(i/k, n*choose(k,i)*p^i*q^(k-i))
		colnames(data) <- i
		rownames(data) <- c('nonref fraction','count')
		#print(data, digits=3)
		out[[k/2]] <- data
	}
	return(out)
}
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
# reformat hw output for overlay on show.allele.scatter histo
hwx <- function(x){
	c <- ncol(x)
	dx <- 1/(c-1)/2
	return(rbind(c(x[1,]-dx,1+dx),c(x[2,],0)*(c-1)/40))
}
###############################################################################################
###############################################################################################

###############################################################################################
###############################################################################################
hwe <- function(cells=1:5){
  # plot
  p <-seq(0,1,by=.01)
  q=1-p
  plot(c(0,1),0:1,type='n')
  for(t in 2*cells){
    lines(p,1-p^t-q^t)
  }
  text(0.5,.5,'t=2')
  text(0.5,7/8,'t=4')
  print(rbind(2*cells,.9^27/(1-2/4^cells)^27))
}
###############################################################################################
###############################################################################################

###############################################################################################
###############################################################################################
hwsample <- function(strains=1,n=100,show=F,cells=3){
  counts <- integer(n)
  refsnps <- which(tables[[7]]$snp==1)
  for(i in 1:n){
    samp <- sample(refsnps,27)
    for(st in strains){
      t <- tables[[st]]$snp[samp]
      counts[i] <- counts[i] + sum(t)
      ##if(show){print(c(t,counts[i]))}
    }
  }
  if(show){print(counts)}
  expect <- 27*length(strains)*(1-2/4^cells)
  print(c(summary(counts),sd(counts),expect))
}
###############################################################################################
###############################################################################################

###############################################################################################
###############################################################################################
#
# another question: is coverage completely random about mean, or is it
# consistently hi/lo at particular positions?
#
# answer seems to be significant correlation.  I.e., coverage is probably
# sequence-dependent, e.g. via G+C content or other simple seq stats.
# Pelim examination 12/13/13 suggests cor wrt G+C is modest (e.g. abs < .2, usually)
# BUT some strains (1012, 3369) are consistently cor ~ -.2 wrt gc, while others (1013)
# are consistently at ~ +.2
#
# relatively rare points of high coverage seem to distort plots; clamp at 'max'
#
coverage.splom <- function(n=3000,mask=!desert.mask,maxcov=300,log=F,win=(-50:50),show.gc=T,show.pairs=F,snp.tables=full.tables.01.24.14)
{
	opar <- par(ask=(show.pairs & show.gc)) ; on.exit(par(opar))
	pmf <- par('mfrow')
	#print(pmf)
	samp <- sample(which(mask & !is.na(snp.tables[[1]]$Ref)),n)
	covmatsamp <- cbind(
		snp.tables[[1]]$Cov[samp],
		snp.tables[[2]]$Cov[samp],
		snp.tables[[3]]$Cov[samp],
		snp.tables[[4]]$Cov[samp],
		snp.tables[[5]]$Cov[samp],
		snp.tables[[6]]$Cov[samp],
		snp.tables[[7]]$Cov[samp])
	colnames(covmatsamp) <- names(snp.tables)[1:7]
	averages <- vector('numeric',7)
	for(i in 1:7){
		averages[i] <- mean(snp.tables[[i]]$Cov)
		#cat('i=', i, 'maxcov=', maxcov, 'len=', length(covmatsamp[,i]), '\n')
		covmatsamp[,i] <- pmin(maxcov, covmatsamp[,i])
	}
	if(show.pairs){
		pairs(covmatsamp, pch='.',log=log,lower.panel=function(x,y,...){text(50,50,round(cor(x,y),2))})
	}
	if(show.gc){
	  a.count <- integer(n)
	  g.count <- integer(n)
	  c.count <- integer(n)
	  t.count <- integer(n)
	  for(i in 1:n){
		#a.count[i] <- sum('A' == snp.tables[[1]]$Ref[samp[i]+win],na.rm=T)
		 g.count[i] <- sum('G' == snp.tables[[1]]$Ref[samp[i]+win],na.rm=T)
		 c.count[i] <- sum('C' == snp.tables[[1]]$Ref[samp[i]+win],na.rm=T)
		#t.count[i] <- sum('T' == snp.tables[[1]]$Ref[samp[i]+win],na.rm=T)
	  }
	  gc <- g.count + c.count
	  whackrow <- par(mfrow=c(3,3))
	  # cat('# GC=NA:',sum(is.na(gc)),'\n')
	  # print(str(gc))
	  # cat('\n')
	  # print(str(covmatsamp))
	  # print(colnames(covmatsamp))
	  # cat('\n')
	  for(i in 1:7){
		plot(gc,covmatsamp[,i],pch='.',main=colnames(covmatsamp)[i])
		text(-5+max(gc),-10+1*max(covmatsamp[,i]),round(cor(gc,covmatsamp[,i],use='all.obs'),2))
		cat(colnames(covmatsamp)[i], cor(gc,covmatsamp[,i],use='all.obs'),'\n')
	  }
	}
	#print(par('mfrow'))
	# i'm confused, but 'on.exit' seems NOT to restore mfrow, so do it by hand:
	#print(pmf);print(whackrow)
	par(mfrow=pmf)
	#print(par('mfrow'))
}

###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
#
# show sequence
#
seeseq <- function(x){
	return(paste(tables[[1]]$Ref[x],sep='',collapse=''))
}
###############################################################################################
###############################################################################################

###############################################################################################
###############################################################################################
#
# show read counts for all 7 strains, or a subset
#
#   x:   vector of positions
#   who: which strains to show
#
seecounts <- function(x,who=1:7,snp.tables=full.tables.01.26.14,debug=F,rat=F){
	df <- NULL
	minitab <- lapply(snp.tables,function(st){st[x,]})
	if(debug){print(minitab)}
	for(i in 1:length(x)){
	        if(debug){cat('Seecounts: i =', i, 'x[i]=', x[i])}
		df <- rbind(df, data.frame(
				chr=minitab[[1]]$chr[i],
  				pos=minitab[[1]]$pos[i],
  				Ref=minitab[[1]]$Ref[i], ### was Ref2, here and below; 1/26/14 tables redefined this?
  				Strain='',A='',G='',C='',T='',SNP='',exon='',indel='',nrf='',rat='',stringsAsFactors=F))
  		for(j in who){
		      	if(debug){cat('j =',j)}
			if(rat){# quick+dirty - should find minor nuc consistently
				maxnr <- max(minitab[[j]][i,c('a','g','c','t')])
				maxny <- max(minitab[[7]][i,c('a','g','c','t')])
				rr <- maxnr/(maxnr+minitab[[j]]$.match[i]) / (maxny/(maxny+minitab[[7]]$.match[i]))
				rr <- round(rr,2)
				nrf <- round(maxnr/(maxnr++minitab[[j]]$.match[i]),2)
			} else {
			        rr <- ''; nrf<- ''
			}
			if(debug){print(str(df))}
  			df <- rbind(df, data.frame(
				chr='',pos='',Ref='',
				Strain=names(minitab)[j],
  				A=ifelse(minitab[[1]]$Ref[i]=='A',minitab[[j]]$.match[i],minitab[[j]]$a[i]),
  				G=ifelse(minitab[[1]]$Ref[i]=='G',minitab[[j]]$.match[i],minitab[[j]]$g[i]),
  				C=ifelse(minitab[[1]]$Ref[i]=='C',minitab[[j]]$.match[i],minitab[[j]]$c[i]),
  				T=ifelse(minitab[[1]]$Ref[i]=='T',minitab[[j]]$.match[i],minitab[[j]]$t[i]),
				SNP  =minitab[[j]]$snp[i],
				exon =minitab[[j]]$exon[i],
				indel=minitab[[j]]$indel[i],
				nrf  =nrf,
				rat  =rr,
				stringsAsFactors=F))
  		}
		if(debug){cat('\n')}
	}
	return(df)
}
###############################################################################################
###############################################################################################

###############################################################################################
###############################################################################################

seeindel <- function(x){
	df <- NULL
	for(i in x){
		df <- rbind(df, data.frame(
				chr= full.tables[[1]]$chr[i],
  				pos= full.tables[[1]]$pos[i],
  				Ref= full.tables[[1]]$Ref2[i],
				exon=full.tables[[1]]$exon[i],
  				tp1007=full.tables[[1]]$indel[i],
  				tp1012=full.tables[[2]]$indel[i],
  				tp1013=full.tables[[3]]$indel[i],
  				tp1014=full.tables[[4]]$indel[i],
  				tp1015=full.tables[[5]]$indel[i],
  				tp3367=full.tables[[6]]$indel[i],
  				tp1335=full.tables[[7]]$indel[i],
  				count=0,
  				bin=0,
  				stringsAsFactors=F))
	}
	df$count <- apply(df[,5:11],1,sum)
	for(i in 5:11) df$bin <- df$bin*2 + df[,i]
	return(df)
}
###############################################################################################
###############################################################################################

###############################################################################################
###############################################################################################
#
# visualization: plot total reads (dot), total nonref reads (plus), and
#   total presumed SNP reads (max of nonref at called SNPs; circle) for a range
#   of positions (2*width+1; default width 1000) centered at 'x' .
#   Perhaps confusingly, total positions may be sqrt-transformed (transform='sqrt');
#   this gives a bit more resolution to the nonref counts
#
#   - smooth > 0:       add running median of coverage in  +/- smooth window
#   - smooth.nref > 0:  add running mean of nref, scaled by smooth.nref.scale
#   - low.color.thresh: non-refs below this frac of coverage are colored red
#   - alt.win:          if non-null, a pair of coords; compare cov to 1k flanks, both sides
#
seechunk <- function(strain=1, x, width=1000, transform='identity', ymax=NA, smooth=1000, smooth.nref=0,
	    	     smooth.nref.scale=10, low.color.thresh=0, alt.win=NULL, show.mean=T,
		     samplesz=min(2*width+1, 5000), # subsample some data series for wide plots
		     show.frac=T,                   # show second panel w/ frac nonref
		     show.legend=T,legend.panel=T,  # show/hide legend; in separate or same panel (sep => show)
		     show.hist=T,		    # show histo of nonref fracs
	    	     snp.tables=full.tables.01.26.14){
	opar <- par(no.readonly=T); on.exit(par(opar))
	if(legend.panel){show.legend <- TRUE}
	if(show.frac || show.hist || legend.panel){
	  par(mar=c(1,4,1,0))
	  # if any subpanels, set layout
	  hgt <- c(70,30)
	  wid <- c(75,25)
	  if(!(show.hist || legend.panel)){
	    # main + frac: 2x1 layout
	    nf <- layout(matrix(c(1,2),2,1), heights=hgt)
	  } else if(!(show.frac || show.hist)){
	    # main + legend, 1x2 layout
	    nf <- layout(matrix(c(1,2),1,2), widths=wid)
	  } else {
	    # 2x2 layout
	    # row-maj bool vector saying which panels plotted
	    which.ones <- c(TRUE, legend.panel, show.frac, show.hist)
	    # count number of plotted ones
	    which.order<- cumsum(which.ones)
	    which.mat  <- matrix(ifelse(which.ones,which.order,0),2,2,byrow=T)
	    nf <- layout(which.mat, widths=wid, heights=hgt)
	  }
	}
	# layout.show(nf) # debug layout
	# return()

	# which transform of positions?  Label accordingly
	xform <- switch(transform, sqrt=sqrt, identity=identity)
	ylab  <- switch(transform,
				sqrt='dot=sqrt(total), +=nonref, o=SNP reads',
				identity='dot=total, +=nonref, o=SNP reads')
	sub   <- switch(transform,
				sqrt='(Note: Coverage, but not others, are sqrt-transformed)',identity=NA)

	# range of positions to plot: +/- width centered at x
	range <- (x-width):(x+width)
	# cat('lrange=',length(range),'\n')
	srange <- range %in% sort(sample(range,samplesz))
	# cat('lsrange=',length(srange),'\n')

	erra <- snp.tables[[strain]]$a[range]
	errc <- snp.tables[[strain]]$c[range]
	errg <- snp.tables[[strain]]$g[range]
	errt <- snp.tables[[strain]]$t[range]
	err  <- erra+errc+errg+errt
	cvr  <- err+snp.tables[[strain]]$.match[range]
	snps <- snp.tables[[strain]]$snp[range]
	exns <- snp.tables[[strain]]$exon[range]
	idls <- snp.tables[[strain]]$indel[range]
	nas  <- is.na(snp.tables[[strain]]$Ref[range])

	maxnonref <- pmax(erra,errc,errg,errt)

	# default ymax is max of 3 data series being plotted
	if(is.na(ymax)){ymax<-max(maxnonref, err, xform(cvr))}

	# Does range cross chr boundaries?
	left  <- snp.tables[[strain]]$chr[min(range)]
	right <- snp.tables[[strain]]$chr[max(range)]
	if(left == right){
	  the.chrs <- as.character(left)
	  chr.starts <- integer(0)
	} else {
	  # range crosses a chr boundary; find start(s) of chr(s) in range & label x axis accordingly
	  chr.starts <- which(snp.tables[[strain]]$pos[range] == 1) + min(range) - 1
	  the.chrs <- paste( as.character(snp.tables[[strain]]$chr[unique(c(min(range),chr.starts))]),
	  	      	     collapse=' : ')
	}

	# plot (transformed) positions
	xcvr <- xform(cvr)
	clipped <- xcvr > ymax
	subrange <- clipped | srange
	plot(range[subrange], pmin(xcvr[subrange],ymax),
		pch=ifelse(clipped[subrange],17,46),
		col=ifelse(clipped[subrange],'red','black'),
		cex=.5,
		ylim=c(-.05,1)*ymax, ylab=ylab,
		xaxt='n',
		xlab=ifelse(show.frac,NA,the.chrs),
		main=st.loc(strain), sub=sub)
	axis(1,labels=!show.frac)
	# if range spans 2 or more chr's, mark border(s)
	for(chr.start in chr.starts){
	  abline(v=chr.start-0.5, col='green', lty='dashed')
	}

	# plot maxnonref in blue if < err; else shades of grey (lower counts lighter (gray(0)=black, 1=white))
	mxerr <- max(err)
	#points(range,err, pch=3, col=ifelse(maxnonref<err,'blue',gray(0.99*((1-err/mxerr)^2))),cex=.5)
	mnrnz <- maxnonref > 0
	points(range[mnrnz & srange],maxnonref[mnrnz & srange], pch=3, col=gray(0.99*((1-err/mxerr)^2)),cex=.5)
	diff <- (maxnonref < err)
	points(range[diff & srange], err[diff & srange], pch=3, col='blue', cex=.5)
	# join blue/grey pts with blue line
	for(i in which(diff & srange)){
	  lines(rep(range[i],2),c(maxnonref[i],err[i]), col='blue',lwd=1)
	}

	# this plots all called SNPs; low nonref fraction colored red
	snp.color <- ifelse(maxnonref[snps>=1] < low.color.thresh * cvr[snps>=1],'red','blue')
	points(range[snps>=1],maxnonref[snps>=1],col=snp.color, pch=1)

	# any marked indels?
	points(range[idls],rep(-3,times=2*width+1)[idls],pch=17,col='green',cex=.5)

	# exons?
	points(range[exns & srange],rep(-ymax/20, times=sum(exns&srange)),pch=15,cex=.5)

	# any NA positions in ref?
	points(range[nas],rep(-3,times=2*width+1)[nas],pch=4,col='red',cex=.5)

	if(smooth > 0){
		# add running average of coverage in 2*smooth+1 width window centered at x
		#library('caTools')
		#smooth.cov <- runmean(snp.tables[[strain]]$Cov[(x-width-smooth):(x+width+smooth)],
		#		2*smooth+1, alg='C', endrule='trim')
		library('stats')
		smooth.cov <- runmed(snp.tables[[strain]]$Cov[(x-width-smooth):(x+width+smooth)],2*smooth+1)
		lines(range[srange],xform(smooth.cov[(smooth:(smooth+2*width))[srange]]),lwd=.5,col='red')
		}
	if(smooth.nref > 0){
		# NOT YET UPDATED FOR SRANGE
		# add running average of nonref reads in 2*smooth.nref+1 width window centered at x
		library('caTools')
		smooth.nref.y <- runmean(snp.tables[[strain]]$Cov   [(x-width-smooth.nref):(x+width+smooth.nref)]
					-snp.tables[[strain]]$.match[(x-width-smooth.nref):(x+width+smooth.nref)],
				2*smooth.nref+1, alg='fast', endrule='trim')
		# print(str(smooth.nref.y))
		lines(range, smooth.nref.scale * smooth.nref.y, lwd=.5,col='yellow')
		if(!is.null(alt.win)){
			alt.nref.mean   <- mean(snp.tables[[strain]]$Cov   [alt.win[1]:alt.win[2]]
					       -snp.tables[[strain]]$.match[alt.win[1]:alt.win[2]])
			alt.nref.mean.l <- mean(snp.tables[[strain]]$Cov   [alt.win[1]-(1:1000)  ]
					       -snp.tables[[strain]]$.match[alt.win[1]-(1:1000)  ])
			alt.nref.mean.r <- mean(snp.tables[[strain]]$Cov   [alt.win[2]+(1:1000)  ]
					       -snp.tables[[strain]]$.match[alt.win[2]+(1:1000)  ])
			alt.nref.mean.flank <- (alt.nref.mean.l+alt.nref.mean.r)/2
			nref.ratio <- alt.nref.mean/alt.nref.mean.flank
			lines(alt.win, rep( smooth.nref.scale * alt.nref.mean,2),       col='green', lwd=2)
			lines(alt.win, rep( smooth.nref.scale * alt.nref.mean.flank,2), col='green', lwd=1, lty='dashed')
		}
	}
	if(!is.null(alt.win)){
		#for looking at regions with anomalous coverage;  plot average in defined window, ratio to 1k flanks
		alt.mean   <- mean(snp.tables[[strain]]$Cov[alt.win[1]:alt.win[2]])
		alt.mean.l <- mean(snp.tables[[strain]]$Cov[alt.win[1]-(1:1000)  ])
		alt.mean.r <- mean(snp.tables[[strain]]$Cov[alt.win[2]+(1:1000)  ])
		alt.mean.flank <- (alt.mean.l+alt.mean.r)/2
		ratio <- alt.mean/alt.mean.flank
		lines(alt.win, rep(xform(alt.mean),2),       col='blue', lwd=2)
		lines(alt.win, rep(xform(alt.mean.flank),2), col='blue', lwd=1, lty='dashed')
	}

	if(show.mean){
		mu <- mean(snp.tables[[strain]]$Cov)
		abline(h=xform(mu),col='orange',lty='dashed')
	}

	if(show.legend){
	  leg               <- data.frame(stringsAsFactors=F, pch=46, col='black', pt.cex= 2, lwd=NA, lty=NA, txt='Coverage')
	  if(samplesz < 2*width+1){
	    leg$txt[1] <- 'Coverage (Sample)'
	  }
	  if(any(clipped)){
	    leg[2,]         <- data.frame(stringsAsFactors=F, pch=17, col='red'  , pt.cex=.7, lwd=NA, lty=NA, txt=paste('Cov clipped, max',max(cvr)))
	  }
	  if(show.mean){
	    leg[nrow(leg)+1,] <- data.frame(stringsAsFactors=F, pch=NA, col='orange'  , pt.cex=.7, lwd= 1, lty='dashed', txt='Overall Mean Cov')
	  }
	  leg[nrow(leg)+1,] <- data.frame(stringsAsFactors=F, pch=NA, col='red'  , pt.cex=.7, lwd= 1, lty=NA, txt='Running Med Cov')
	  leg[nrow(leg)+1,] <- data.frame(stringsAsFactors=F, pch= 3, col='blue' , pt.cex=.7, lwd=NA, lty=NA, txt='Tot Nonref')
	  leg[nrow(leg)+1,] <- data.frame(stringsAsFactors=F, pch= 3, col='gray' , pt.cex=.7, lwd=NA, lty=NA, txt='Max Nonref')
	  leg[nrow(leg)+1,] <- data.frame(stringsAsFactors=F, pch= 1, col='blue' , pt.cex=.7, lwd=NA, lty=NA, txt='Max NR @ SNP')
	  leg[nrow(leg)+1,] <- data.frame(stringsAsFactors=F, pch=17, col='green', pt.cex=.7, lwd=NA, lty=NA,
	  		       		  txt=paste(sum(idls), ' Indels (', format((2*width+1)/sum(idls),digits=1), 'bp/indel)', sep=''))
	  leg[nrow(leg)+1,] <- data.frame(stringsAsFactors=F, pch=15, col='black', pt.cex=.7, lwd=NA, lty=NA, txt='Exon')

	  if(any(nas)){
	    leg[nrow(leg)+1,] <- data.frame(stringsAsFactors=F, pch=4, col='red', pt.cex=.7, lwd=NA, lty=NA,
	    		       		    txt=paste(sum(nas),'NA Refs'))
  	  }
	  if(!is.null(alt.win)){
	    leg[nrow(leg)+1,] <- data.frame(stringsAsFactors=F, pch=NA, col='blue', pt.cex=NA, lwd=2, lty=NA,
	  				    txt=paste('Cov Ratio:',format(ratio,digits=3)))
	  }
	  if(smooth.nref > 0){
	    leg[nrow(leg)+1,] <- data.frame(stringsAsFactors=F, pch=NA, col='yellow', pt.cex=NA, lwd=.5, lty=NA,
	  				    txt=paste('Smoothed nonref rate x',smooth.nref.scale))
	    if(!is.null(alt.win)){
	      leg[nrow(leg)+1,] <- data.frame(stringsAsFactors=F, pch=NA, col='green', pt.cex=NA, lwd=2, lty=NA,
	  				      txt=paste('Nref Ratio:',format(nref.ratio,digits=3)))
	    }
	  }
	  if(legend.panel){
	    par(mar=c(1,1,1,1))
	    # to show legend in separate panel, create empty plot
	    plot(0,0,type='n',bty='n',xaxt='n',yaxt='n',xlab='',ylab='')
	  }
	  legend('topleft', bty='n', legend=leg$txt, lwd=leg$lwd, lty=leg$lty, pch=leg$pch, col=leg$col, pt.cex=leg$pt.cex, cex=.8)
	}
	if(show.frac){
	  par(mar=c(4,4,0,0))
	  frac <- maxnonref/(maxnonref+snp.tables[[strain]]$.match[range])
	  urange <- srange | snps>=1
	  plot(range[urange],frac[urange], col=ifelse(snps>=1,'blue','black')[urange],
	  				   pch=ifelse(snps>=1,19,46)[urange],
					   cex=.3,xaxt='n',xlab=NA,
					   ylim=0:1, ylab='nref frac')
	  axis(side=1,labels=T)
	  # if range spans 2 or more chr's, mark border(s)
	  for(chr.start in chr.starts){
	    abline(v=chr.start-0.5, col='green', lty='dashed')
	  }
	  mtext(the.chrs, side=1, line=3)
	}
	if(show.hist){
	  par(mar=c(4,1,0,1))
	  #frac.hist.unsnp <- hist(frac[, breaks=(0:20)/20, plot=FALSE)
	  frac.hist.snp   <- hist(frac[snps>=1], breaks=(0:20)/20, plot=FALSE)
	  #barplot(frac.hist.all$counts, horiz=TRUE)
	  barplot(frac.hist.snp$counts, horiz=TRUE, col='blue')
	}
}
###############################################################################################
###############################################################################################

###
# 
# show desert at specific row of strain in desert.df table
# plot is centered at desert center offset by dx, wide enough to show full desert +/- margin.
#
seedesert <- function(strain,row,margin=100,dx=0,d.df=des.df,snp.tables=snp.tables.full){
  b <- d.df[[strain]]$iStart[row]
  e <- d.df[[strain]]$iEnd[row]
  l <- d.df[[strain]]$Length[row]
  width <- ceiling(l/2)+1
  seechunk(strain,(b+e)/2+dx,width+margin,snp.tables=snp.tables)
}

###############################################################################################
###############################################################################################
#
# Transversion analysis: find positions where both possible transversions are observed
#
dualtransversions <- function(st){
	refR <- full.tables[[st]]$Ref2 =='A' | full.tables[[st]]$Ref2 =='G'
	refY <- full.tables[[st]]$Ref2 =='C' | full.tables[[st]]$Ref2 =='T'
	refR[is.na(refR)] <- F
	refY[is.na(refY)] <- F
	dual <- vector('logical',length(full.tables[[st]]$Ref2))
	dual[refR] <- full.tables[[st]]$t[refR] > 0 & full.tables[[st]]$c[refR] > 0
	dual[refY] <- full.tables[[st]]$a[refY] > 0 & full.tables[[st]]$g[refY] > 0
	return(dual)
}
###############################################################################################
###############################################################################################

###############################################################################################
###############################################################################################

# a simple check for independence of errors: we know transitions are more probable than transversions, but within the
# latter class, 'independent errors' would imply that among all positions with a reference pyrimidine ('T', say), and
# with c nonreference purine reads, the observed counts of of A,G should be binomially distributed as Bin(c, p),
# possibly with p != 0.5.  The following procedure tabulates these stats for one strain ('st'), for all 'c' from 1 to
# 'cmax'.  When c is large, we are likely to include a number of heterozygous transversion positions in the counts; to
# minimize this effect, we restrict to positions where at least 70% or reads match the reference. Probably should also
# filter out unusually hi- and low-coverage positions, but I have not yet done that.

# Bottom line is that these stats do NOT look binomial -- there is a large excess of positions where most reads are
# either one or the other of the allowed alternatives.  This may be due to pollution of the sequencing input by rare
# minor alleles, some bias in sequence assembly/mapping, or correlated read errors by the sequencer, but in any case,
# modeling read errors as independent is highly suspect.

# 'masked' version uses an arbitrary mask to select positions, but is slow; cf. 'counted...' below.
#
masked.transv <- function(st=7,cmax=4,mask=desert.region){
	tab   <- vector('list',    cmax)
	untab <- c(0,0)
	for(c in 1:cmax){
		tab[[c]] <- matrix(0,4,c+1)
		row.names(tab[[c]]) <- c('a','g','c','t')
	}
	if(is.na(mask)){mask <- rep(T,length(full.tables[[st]]$Ref2))}
	n <-sum(mask)
	for(i in 1:n){
		if(!is.na(full.tables[[st]]$Ref2[mask][i]) &&
		   0.7 < full.tables[[st]]$.match[mask][i]/full.tables[[st]]$Cov[mask][i]){
			ref.idx <- pmatch(full.tables[[st]]$Ref2[mask][i], c('A','G','C','T'))
			if(ref.idx == 1 || ref.idx == 2){
				# ref is purine, count pyrimidines
				c1 <- full.tables[[st]]$c[mask][i]
				c2 <- full.tables[[st]]$t[mask][i]
			} else {
				# ref is pyrimidine, count purines
				c1 <- full.tables[[st]]$a[mask][i]
				c2 <- full.tables[[st]]$g[mask][i]
			}
			nref <- c1 + c2
			if(nref == 0){
				untab[1] <- untab[1] +1
			} else if(nref <= cmax){
				tab[[nref]][ref.idx, c1+1] <- tab[[nref]][ref.idx, c1+1] + 1
			} else {
				untab[2] <- untab[2] + 1
			}
		}
	}
	return(list(st,st.loc(st),tab,untab))
}
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
#
# as above, but runs on a contiguous region; faster than 'masked.'
#
counted.transv <- function(st=7,cmax=4,region.start=1,region.length=desert.length){
	tab   <- vector('list',    cmax)
	untab <- c(0,0)
	for(c in 1:cmax){
		tab[[c]] <- matrix(0,4,c+1)
		row.names(tab[[c]]) <- c('a','g','c','t')
	}
	for(i in region.start:(region.start+region.length)){
		if(!is.na(full.tables[[st]]$Ref2[i]) &&
		   0.7 < full.tables[[st]]$.match[i]/full.tables[[st]]$Cov[i]){
			ref.idx <- pmatch(full.tables[[st]]$Ref2[i], c('A','G','C','T'))
			if(ref.idx == 1 || ref.idx == 2){ # ref is purine, count pyrimidines
				c1 <- full.tables[[st]]$c[i]
				c2 <- full.tables[[st]]$t[i]
			} else {                          # ref is pyrimidine, count purines
				c1 <- full.tables[[st]]$a[i]
				c2 <- full.tables[[st]]$g[i]
			}
			nref <- c1 + c2
			if(nref == 0){
				untab[1] <- untab[1] +1
			} else if(nref <= cmax){
				tab[[nref]][ref.idx, c1+1] <- tab[[nref]][ref.idx, c1+1] + 1
			} else {
				untab[2] <- untab[2] + 1
			}
		}
	}
	return(list(st,st.loc(st),tab,untab))
}
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
#
# from Tony's genome-wide SNP stats
#
loci.snp.rate <- function(strain=1, stat.tab, loci.type='Desert', percent='5%'){
	opar <- par(ask=T,no.readonly=T);on.exit(par(opar))
	n <- nrow(stat.tab[[strain]])
	k5 <- stat.tab[[strain]][,5]
	n5 <- stat.tab[[strain]][,1]/10
	p5 <- k5/n5

	kd <- stat.tab[[strain]][,2]
	nd <- stat.tab[[strain]][,1]
	pd <- kd/nd

	yy <- max(p5,pd)
	plot(1:n,1:n,ylim=c(0,yy), xlab=paste(loci.type,'count', sep=' '), ylab='SNP rate',
             main=paste(loci.type, ' SNP rate vs ', percent, ' flank:',
                 st.loc(strain)), type='n')
	for(i in 1:n){
		lines(rep(i,2),p5[i]+c(-2,2)*sqrt(p5[i]*(1-p5[i])/n5[i]),
                      col='green',lwd=.5)

		lines(rep(i,2),pd[i]+c(-2,2)*sqrt(pd[i]*(1-pd[i])/nd[i]),
                      col='blue',lwd=.5)
	}
	points(1:n,p5,pch=18,col='green')
	points(1:n,pd,pch=18,col='blue')
	hist(p5,breaks=25,col='green', main=paste(loci.type,
                                           ' SNP rate vs ', percent, ' flank:',
                                           st.loc(strain)),
             xlab='SNP rate')
	hist(pd,breaks=25,,col='blue', add=T)
}
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
#
# Load CNVnator tables
#
# This:
#  * updates the default read.delim interpretation of 'filtered' to make it Bool (instead of a
#    factor), 
#  * does some simple error checking, 
#  * optionally truncate tables to delete features in organelles/BD_s, and 
#  * adds fields iStart, iEnd giving start/end indices for each feature in bigtable (in addition to
#    chr-relative start/end provided by CNVnator).  The later depends on chr.to.index; if we ever 
#    need to run this with non-standard bigtable coordinates, load the appropriate table first and
#    do chr.to.index(reset=T); see comments at chr.to.index.
#
load.cnv.tables <- function(
  path='../../../data/cnv.txt',  # relative to usual scripts/larrys/xx
  chrs.only=TRUE                 # remove organelle/BD_ data
){
  # Read:
  cnv <- read.delim(path)
  
  # Fix:
  cnv$filtered <- (cnv$filtered == 'True')     ## convert factor to Bool
  
  # Error checks:
  if(!all(cnv$length == (cnv$end-cnv$start+1))){
    cat('load.cnv.tables: WARNING - start/end/length NOT as expected.\n')
  }
  for(i in 1:9){
    if(any(is.na(cnv[,i]))){
      cat('load.cnv.tables: WARNING - unexpected NA in column',i,'.\n')
    }
  }
  
  # Truncate (optional):
  if(chrs.only){
    cnv <- cnv[substr(as.character(cnv$chr),1,3) == 'Chr',] # remove non-chromosomes 
  }
  
  # Add bigtable indices:
  cnv <- cbind(cnv, iStart=NA, iEnd=NA)               # add start/end as bigtable indices
  for(i in 1:nrow(cnv)){
    cnv$iStart[i] <- chr.to.index(cnv$chr[i], cnv$start[i], snp.tables=NULL)
    cnv$iEnd[i]   <- chr.to.index(cnv$chr[i], cnv$end[i],   snp.tables=NULL)
  }
  
  # Remove spurious row names
  row.names(cnv) <- NULL
  return(cnv)
}


###############################################################################################
###############################################################################################
#
# convert desert tables to data frames
#
# desert table is a list (entry per strain) of list (entry per chr) 
# of n x 3 matrices giving start, end, length of deserts.  E.g. str(des[[1]]) has
# 27 entries such as:
#
#   $ Chr16a   : num [1:14, 1:3] 1 12863 47749 64248 150198 ...
#   ..- attr(*, "dimnames")=List of 2
#   .. ..$ : NULL
#   .. ..$ : chr [1:3] "desert origin" "desert terminate" "Length"
#
# At times, a single data frame per strain would be more convenient.  This returns:
#
#  > str(des.df)
#   List of 7
#   $ :'data.frame':	897 obs. of  4 variables:
#   ..$ chr             : Factor w/ 27 levels "Chr1","Chr2",..: 1 1 1 1 1 1 1 1 1 1 ...
#   ..$ desert.origin   : num [1:897] 1 8952 19297 91986 211997 ...
#   ..$ desert.terminate: num [1:897] 8518 12507 25865 173031 215771 ...
#   ..$ Length          : num [1:897] 8516 3554 6567 81044 3773 ...
#
# Hmmm.  As of 3/17/2016,  I now think origin is the first pos of a desert, whereas terminate is 
# the first NON-desert pos following the end of the desert.  Then Length should be terminate minus 
# origin, but data in des.rda seems to have recorded length as terminate minus origin minus 1. E.g.,
# 1st entry above is 1..8518 with "length" 8516. (Tony's memory concurs.) So, updating this to add
# one to lengths.  More debatably, I think it will be more convenient to have an "end" field giving
# last pos of desert, shorted names, include big-table indices in addition to per-chromosome
# offsets, and optionally truncate to Chromosomes only (no mito, plastid or BD).  So, new return
# value looks like:
#
# > str(des.to.df(des))
# List of 7
# $ tp1007 :'data.frame':	897 obs. of  6 variables:
# ..$ Chr   : Factor w/ 27 levels "Chr1","Chr2",..: 1 1 1 1 1 1 1 1 1 1 ...
# ..$ Start : num [1:897] 1 8952 19297 91986 211997 ...
# ..$ End   : num [1:897] 8517 12506 25864 173030 215770 ...
# ..$ Length: num [1:897] 8517 3555 6568 81045 3774 ...
# ..$ iStart: num [1:897] 1 8952 19297 91986 211997 ...
# ..$ iEnd  : num [1:897] 8517 12506 25864 173030 215770 ...
# $ tp1012 :'data.frame':	915 obs. of  6 variables:
# ...
# 
des.to.df <- function(des, chr.only=TRUE, snp.tables=NULL){
  des.df <- vector('list', 7)
  names(des.df) <- names(des)
  for(st in 1:7){
    df <- NULL
    for(ch in 1:(ifelse(chr.only, 27, length(des[[st]])))){
      if(nrow(des[[st]][[ch]]) >= 1){
        df <- rbind(df, data.frame(Chr=names(des[[st]])[ch], des[[st]][[ch]], -42, -42))
      }
    }
    names(df) <- c('Chr', 'Start', 'End', 'Length', 'iStart', 'iEnd')
    df$End    <- df$End - 1
    df$Length <- df$Length + 1
    for(i in 1:nrow(df)){
      # convert chromosome-relative positions in des table to bigtable indices, & save them
      df$iStart[i] <- chr.to.index(df$Chr[i], df$Start[i], snp.tables=snp.tables)
      df$iEnd[i]   <- chr.to.index(df$Chr[i], df$End[i],   snp.tables=snp.tables)
    }
    des.df[[st]] <- df
  }
  return(des.df)
}


###############################################################################################
###############################################################################################
#
# convert chromosome name to index in full tables of the first pos of that chr.  If optional 
# in-chromosome offsets are given, map them to indices into big table in corresponding positions.
#
# This uses the GLOBAL data.frame '.chr.to.index.map' (defined below the function body) to
# accomplish this mapping. If the big tables ever change, or if you need to dynamically shift
# between table sets, do rm('.chr.to.index.map') before calling this, or call with reset=T.  In
# either of these cases, on first call this map is GLOBALLY built and cached.  Note that a non-NULL
# snp.tables argument is needed in these cases.
# 
# To avoid the load time for the full tables, after I created the map, I used 
# "chr.to.index.map.dumper" below to generate the assignment below, then copy/pasted it here so this
# map gets loaded whenever this file is loaded.  As noted, if the tables ever change, you can
# dynamically override this via reset=T, and/or use dump/copy/paste to replace the assignment as
# indicated. (line breaks inserted by hand).
#
chr.to.index <- function(chr, offsets=1, reset=FALSE, snp.tables = NULL){
  if(reset || !exists('.chr.to.index.map')){
    strain     <- 1 # arbitrary; all should be the same
    chr.starts <- which(snp.tables[[strain]]$pos == 1) 
    chrs       <- snp.tables[[strain]]$chr[chr.starts]
    .chr.to.index.map <<- data.frame(Chr=chrs, Start=chr.starts)  ## GLOBAL assignment
  }
  base <- .chr.to.index.map[which(as.character(chr) == .chr.to.index.map$Chr), 'Start']
  return(base + offsets - 1)
}

.chr.to.index.map <-
  data.frame(
    Chr = c(
      'Chr1', 'Chr2', 'Chr3', 'Chr4', 'Chr5', 'Chr6', 'Chr7', 'Chr8', 'Chr9', 'Chr10',
      'Chr11a', 'Chr11b', 'Chr12', 'Chr13', 'Chr14', 'Chr15', 'Chr16a', 'Chr16b', 'Chr17',
      'Chr18', 'Chr19a_19', 'Chr19b_31', 'Chr19c_29', 'Chr20', 'Chr22', 'Chr23', 'Chr24',
      'mitochondria.fasta', 'chloroplast.fasta', 'BD1_7', 'BD2_100', 'BD3_105', 'BD4_130',
      'BD5_160', 'BD6_46', 'BD7_47', 'BD8_51', 'BD9_52', 'BD10_65', 'BD11_74', 'BD12_85',
      'BD13_1', 'BD14_19', 'BD15_19', 'BD16_19', 'BD17_19', 'BD18_19', 'BD19_4', 'BD20_5',
      'BD21_5', 'BD22_7', 'BD23_33', 'BD24_33', 'BD25_33', 'BD26_41', 'BD27_41', 'BD28_107',
      'BD29_110', 'BD30_155', 'BD31_227', 'BD32_49', 'BD33_54', 'BD34_588', 'BD35_67',
      'BD36_69', 'BD37_91'
    ),
    Start = c(
      1, 3042586, 5749781, 8189833, 10592156, 12898128, 14969608, 16962042, 18229240,
      19420300, 20525968, 21332110, 21414953, 22543335, 23595531, 24594174, 25525442,
      25985218, 26154594, 26814518, 27641571, 28248810, 28400487, 28691681, 29491915,
      30549480, 31004434, 31301783, 31345610, 31474424, 31500794, 31522319, 31536713,
      31548410, 31567957, 31647613, 31685246, 31736624, 31752873, 31778283, 31795661,
      31809131, 31909423, 31940157, 31961523, 31976765, 31988566, 31993083, 32059743,
      32143133, 32146782, 32169968, 32308518, 32329250, 32335654, 32399524, 32404243,
      32415269, 32425824, 32433914, 32439069, 32509920, 32553378, 32555660, 32577073,
      32596174
    )
  )

chr.to.index.map.dumper <- function(cmap=.chr.to.index.map){
  strx <- character(6)
  strx[1] <- '.chr.to.index.map <- data.frame(Chr='
  strx[2] <- 'c(\''
  strx[3] <- paste(as.character(cmap$Chr), collapse="', '")
  strx[4] <- '\'), Start=c('
  strx[5] <- paste(cmap$Start, collapse=', ')
  strx[6] <- '))'
return(paste(strx, collapse=''))
}

###
#
# draw.des.fig/draw.des.row: Create Fig2A for paper: distribution of deserts across Chr1
#
# draw pattern for one strain (row)
draw.des.row <- function(x, y, height, width, strain, chr, des, gap.tbl, draw.nondes, 
                         min.desert, min.gap, d.col, nd.col, gap.col, twotone){
  # Draw all deserts (longer than min.desert) on a non-desert background
  rect(x, y, x+width, y+height, col=nd.col, border=NA)
  n <- nrow(des[[strain]][[chr]])
  for(i in 1:n){
    d.start <- des[[strain]][[chr]][i,1]
    d.end   <- des[[strain]][[chr]][i,2]
    if(d.end-d.start+1 >= min.desert){
      rect(d.start, y, d.end, y+height, col=d.col, border=NA)
    } else if(!is.na(twotone)) {
      rect(d.start, y, d.end, y+height, col=twotone, border=NA)
    }
  }
  # show gaps (excluding small ones; probably invisible anyway)
  # if gap.tbl is a list, presume it has an entry per strain; o.w. presume it applies to all 7
  if(!is.null(gap.tbl)){
    if(typeof(gap.tbl)=='list'){
      this.gap.tbl <- gap.tbl[[strain]]
    } else {
      this.gap.tbl <- gap.tbl
    }
    for(i in 1:nrow(this.gap.tbl)){
      g.start <- this.gap.tbl[i,'start']
      g.end   <- this.gap.tbl[i,'end']
      if(g.end-g.start+1 > min.gap){
        rect(g.start, y, g.end, y+height, col=gap.col, border=NA)
      }
    }
  }
  if(draw.nondes){
    # Draw all non-deserts on a desert background in top half of row
    rect(x, y+height/2, x+width, y+height, col=d.col, border=NA)
    if(des[[strain]][[chr]][1,1] > 1){ # nondes @ left edge
      rect(1, y+height/2, des[[strain]][[chr]][1,1], y+height, col=nd.col, border=NA)
    }
    for(i in 1:(n-1)){
      nd.start <- des[[strain]][[chr]][i,2]
      nd.end   <- des[[strain]][[chr]][i+1,1]
      rect(nd.start, y+height/2, nd.end, y+height, col=nd.col, border=NA)
    }
    if(des[[strain]][[chr]][n,2]<width){ # nondes @ right edge
      rect(des[[strain]][[chr]][n,2], y+height/2, width, y+height, col=nd.col, border=NA)
    }
  }
  rect(x, y, x+width, y+height, col=NA, border='black', lwd=0.75)
}

# draw the full fig (all 7 strains)
draw.des.fig <- function(des, gap.tbl=NULL, width=chr1.len, 
                         draw.nondes=FALSE, min.desert=000, min.gap=width/3000,
                         d.col='deepskyblue1', nd.col='white', gap.col='pink',
                         row.spacing=50,row.separation=10, row.order=strain.order,
                         chr='Chr1', xlab='Chromosome 1 Position (Mb)',
                         panel.label=NA, twotone=NA, do.par=TRUE
){
  # For the 2A/2B combo plot, par here messes with layout, but do it for stand-alone plot [default]
  if(do.par){
    opar <- par(no.readonly=TRUE); on.exit(par(opar))
    par(mar=c(3,3,1,1),oma=c(0,0,0,0),tcl=-0.2)
  }
  row.height <- row.spacing - row.separation
  plot(0, 0, type='n', xlim=c(0,width), ylim=c(-7*row.spacing, 0), xaxt='n', xlab='', 
       yaxt='n', ylab='', lab=c(3,7,99), lwd=0.5)
  st.ids <- sub('tp', '', names(des), fixed=TRUE) # shorten to numeric ID (& 'tp' is obsolete)
  xlabels <- c('0', '', '1', '', '2', '', '3')
  axis(1, at=(0:6)*0.5e6, labels=xlabels, padj=-1.2) #was -1
  axis(2, at=-(1:7)*row.spacing+row.height/2, labels=st.ids[row.order], lab=c(3,7,99),las=1, hadj=.65)#was .7
  mtext(xlab, side=1, line=1.4) #was 1.8
  for(i in 1:7){
    draw.des.row(0, -i*row.spacing, row.height, width=width,strain=row.order[i], chr=chr, des=des, 
                 gap.tbl=gap.tbl, draw.nondes=draw.nondes, min.desert=min.desert, min.gap=min.gap,
                 d.col=d.col, nd.col=nd.col, gap.col=gap.col, twotone=twotone)
  }
  if(!is.na(panel.label)){
    text(70000,-row.spacing+row.height/2, panel.label, cex=1.1)#was 1.2
  }
}

