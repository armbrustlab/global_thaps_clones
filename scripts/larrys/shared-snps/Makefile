# WARNING: this makefile is perhaps messed up, proceed with caution.
# Unlike others is parallel dirs, plan here is to run shared-snps.rnw 4 times,
# each with different parameters, RENAMING the resulting tex file after each.
# (4 = {truncated tables, Chr1 only} x {unfiltered, qfiltered} )
#
# "make four" should accomplish this, after which I think (but haven't carefully
# tested) "make all" will build the pdf's

DIRS	  := ./
EXCEPTRNW := 
EXCEPTTEX := 
STYS	  := 

RNWS	:= $(filter-out $(EXCEPTRNW), $(wildcard $(addsuffix *.rnw, $(DIRS))))

# .texs built from .rnws:
RNW-TEX	:= $(patsubst %.rnw,%.tex,$(RNWS))

TEXS	:= $(wildcard $(addsuffix *.tex, $(DIRS)))
TEXS	:= $(filter-out $(RNW-TEX), $(TEXS))
##TEXS    += $(RNW-TEX)
TEXS    := $(filter-out $(EXCEPTTEX), $(TEXS))

# .pdfs built from .texs:
TEX-PDF	:= $(patsubst %.tex,%.pdf,$(TEXS))

.PHONY:	all tex four dryrun pull echo

.SECONDARY:	$(RNW-TEX)

FOURTEX	:= shared-snps-trunc-unfiltered.tex \
	   shared-snps-trunc-qfiltered.tex \
	   shared-snps-chr1-unfiltered.tex \
	   shared-snps-chr1-qfiltered.tex

four:	truncun truncq chr1un chr1q

all:	$(TEX-PDF)
	@echo Made.

## knitr/Sweave 
# knitr doesn't need it but for Sweave - put following in preamble of .rnw(.tex) file)
# % to use Sweave, need this package to latex this file, but if I only use \Sexpr{}, then all other traces of sweave 
# % gone from resulting .tex file, so this is not needed in actual HW sols.tex (tho easily added if desired; see 
# % &texheader in weblib.pl)
# \usepackage{/Library/Frameworks/R.framework/Resources/share/texmf/tex/latex/Sweave}
# 
# in either case, keep the .tex file read-only, so I remember to edit the .rnw
tex:
	if [ -f shared-snps.tex ] ; then mv  -f shared-snps.tex{,.$$$$.bak} ; fi
	R --no-save -e "library(knitr);knit('shared-snps.rnw')"
	chmod a-wx shared-snps.tex                      ## edit .rnw, not .tex

# args: load.tb (order: full.un, chr1.un, full.qfil,  chr1.qfil), pri, clearcache, nboot, truncate

truncun:	shared-snps.rnw Makefile
	if [ -f shared-snps-trunc-unfiltered.tex ] ; then mv  -f shared-snps-trunc-unfiltered.tex{,.$$$$.bak} ; fi
	time R --no-save -e "library(knitr);knit('shared-snps.rnw')" --args "c(T,F,T,F),1:4,T,101,T"
	mv shared-snps{,-trunc-unfiltered}.tex
	chmod a-wx shared-snps-trunc-unfiltered.tex            ## edit .rnw, not .tex

truncunbootless:	shared-snps.rnw Makefile
	if [ -f shared-snps-trunc-unfiltered.tex ] ; then mv  -f shared-snps-trunc-unfiltered.tex{,.$$$$.bak} ; fi
	time R --no-save -e "library(knitr);knit('shared-snps.rnw')" --args "c(T,F,T,F),1:4,T,1,T"
	mv shared-snps{,-trunc-unfiltered}.tex
	chmod a-wx shared-snps-trunc-unfiltered.tex            ## edit .rnw, not .tex

truncq:	shared-snps.rnw Makefile
	if [ -f shared-snps-trunc-qfiltered.tex ] ; then mv  -f shared-snps-trunc-qfiltered.tex{,.$$$$.bak} ; fi
	time R --no-save -e "library(knitr);knit('shared-snps.rnw')" --args "c(T,F,T,F),c(3,4,1,2),T,101,T"
	mv shared-snps{,-trunc-qfiltered}.tex
	chmod a-wx shared-snps-trunc-qfiltered.tex            ## edit .rnw, not .tex

truncqbootless:	shared-snps.rnw Makefile
	if [ -f shared-snps-trunc-qfiltered.tex ] ; then mv  -f shared-snps-trunc-qfiltered.tex{,.$$$$.bak} ; fi
	time R --no-save -e "library(knitr);knit('shared-snps.rnw')" --args "c(T,F,T,F),c(3,4,1,2),T,1,T"
	mv shared-snps{,-trunc-qfiltered}.tex
	chmod a-wx shared-snps-trunc-qfiltered.tex            ## edit .rnw, not .tex

chr1un:	shared-snps.rnw Makefile
	if [ -f shared-snps-chr1-unfiltered.tex ] ; then mv  -f shared-snps-chr1-unfiltered.tex{,.$$$$.bak} ; fi
	time R --no-save -e "library(knitr);knit('shared-snps.rnw')" --args "c(F,T,F,T),1:4,T,5,F"
	mv shared-snps{,-chr1-unfiltered}.tex
	chmod a-wx shared-snps-chr1-unfiltered.tex            ## edit .rnw, not .tex

chr1q:	shared-snps.rnw Makefile
	if [ -f shared-snps-chr1-qfiltered.tex ] ; then mv  -f shared-snps-chr1-qfiltered.tex{,.$$$$.bak} ; fi
	time R --no-save -e "library(knitr);knit('shared-snps.rnw')" --args "c(F,T,F,T),c(3,4,1,2),T,5,F"
	mv shared-snps{,-chr1-qfiltered}.tex
	chmod a-wx shared-snps-chr1-qfiltered.tex            ## edit .rnw, not .tex

./%.tex:	%.rnw
	if [ -f $(notdir $*).tex ] ; then mv  -f $(notdir $*).tex{,.$$$$.bak} ; fi
	time R --no-save -e "library(knitr);knit('$(notdir $*).rnw')"
	chmod a-wx  $(notdir $*).tex            ## edit .rnw, not .tex

./%.pdf:	%.tex Makefile $(STYS)			## make .pdf from .tex
	(cd $(dir $@); \
	 pdflatex $(notdir $*).tex; \
	 grep "Label(s) may have changed. Rerun to get" $(notdir $*).log && pdflatex $(notdir $*).tex; \
	 grep "Label(s) may have changed. Rerun to get" $(notdir $*).log && pdflatex $(notdir $*).tex; \
	 grep "Label(s) may have changed. Rerun to get" $(notdir $*).log && pdflatex $(notdir $*).tex; \
	 grep "Label(s) may have changed. Rerun to get" $(notdir $*).log && pdflatex $(notdir $*).tex; \
	 rm -f $(notdir $*).{out} \
		)

# Pull files fron cycle server: quick and dirty
# (NB: slash following TEST is important to rsync semantics)
REXCLUDES	:= .DS_Store 
REXCLUDES	:= $(addprefix --exclude=, $(REXCLUDES))
CYCLEPATH	:= barb.cs.washington.edu:Documents/g/projects/thaps/Thaps_7_strains/code/snpNB/scripts/larrys/shared-snps/
FILES		:= shared-snps-trunc-qfiltered.pdf  \
		   shared-snps-trunc-unfiltered.pdf \
		   shared-snps-chr1-qfiltered.pdf  \
		   shared-snps-chr1-unfiltered.pdf \
		   shared-snps-trunc-qfiltered.tex  \
		   shared-snps-trunc-unfiltered.tex \
		   shared-snps-chr1-qfiltered.tex  \
		   shared-snps-chr1-unfiltered.tex \
		   paperfig-medium-tree-trunc-qfiltered.pdf  \
		   paperfig-medium-tree-trunc-unfiltered.pdf \
		   paperfig-medium-tree-Chr1-qfiltered.pdf  \
		   paperfig-medium-tree-Chr1-unfiltered.pdf \
		   paperfig-medium-tree-trunc.pdf
FILES		:=  $(addprefix $(CYCLEPATH), $(FILES))

dryrun:	
	@echo PULL FROM \*CYCLE DRY RUN . . . . . . $(REXCLUDES)
	rsync --dry-run -avz  $(REXCLUDES) $(CYCLEPATH) fromcycle/

pull:	
	@echo PULL FROM \*CYCLE
	rsync           -avz  $(REXCLUDES) $(CYCLEPATH) fromcycle/


echo:
	@echo "DIRS:      $(DIRS)"
	@echo "EXCEPTRNW: $(EXCEPTRNW)"
	@echo "EXCEPTTEX: $(EXCEPTTEX)"
	@echo "STYS:      $(STYS)"
	@echo "RNWS:      $(RNWS)"
	@echo "TEXS:      $(TEXS)"
	@echo "RNW-TEX:   $(RNW-TEX)"
	@echo "TEX-PDF:   $(TEX-PDF)"
	@echo "CYCLEPATH: $(CYCLEPATH)"
	@echo "FILES:     $(FILES)"
