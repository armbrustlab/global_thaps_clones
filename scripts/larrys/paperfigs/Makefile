TARGET	  := Fig1-mscat Fig2A-desert-distribution FigS5-SNPdip FigS7-hwe-histo
DIRS	  := ./
EXCEPTRNW := 
EXCEPTTEX := 
STYS	  := 

RNWS	:= $(filter-out $(EXCEPTRNW), $(wildcard $(addsuffix *.[Rr]nw, $(DIRS))))

# .texs built from .rnws:
RNW-TEX	:= $(patsubst %.rnw,%.tex,$(RNWS))

TEXS	:= $(wildcard $(addsuffix *.tex, $(DIRS)))
TEXS	:= $(filter-out $(RNW-TEX), $(TEXS))
TEXS    += $(RNW-TEX)
TEXS    := $(filter-out $(EXCEPTTEX), $(TEXS))

# .pdfs built from .texs:
TEX-PDF	:= $(patsubst %.tex,%.pdf,$(TEXS))

.PHONY:	all

.SECONDARY:	$(RNW-TEX)

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
tex:	../../../R/wlr.R
	@echo 'tex rule'
	if [ -f $(TARGET).tex ] ; then mv  -f $(TARGET).tex{,.$$$$.bak} ; fi
	R --no-restore --no-save -e "library(knitr);knit('$(TARGET).rnw')"
	chmod a-wx $(TARGET).tex                      ## edit .rnw, not .tex

./%.tex:	%.rnw ../../../R/wlr.R
	@echo 'fancy rule'
	if [ -f $(notdir $*).tex ] ; then mv  -f $(notdir $*).tex{,.$$$$.bak} ; fi
	R --no-restore --no-save -e "library(knitr);knit('$(notdir $*).rnw')"
	chmod a-wx  $(notdir $*).tex            ## edit .rnw, not .tex

./%.pdf:	%.tex Makefile $(STYS)			## make .pdf from .tex
	(cd $(dir $@); \
	 pdflatex $(notdir $*).tex; \
	 grep "Label(s) may have changed. Rerun to get" $(notdir $*).log && pdflatex $(notdir $*).tex; \
	 grep "re-run latex to see which.snp.tables()" $(notdir $*).tex && pdflatex $(notdir $*).tex; \
	 rm -f $(notdir $*).{aux,out,log} \
		)

# Pull files fron cycle server: quick and dirty *** AND OUT OF DATE ***
# (NB: slash following TEST is important to rsync semantics)
REXCLUDES	:= .DS_Store 
REXCLUDES	:= $(addprefix --exclude=, $(REXCLUDES))
CYCLEPATH	:= barb.cs.washington.edu:Documents/g/projects/thaps/Thaps_7_strains/code/snpNB/scripts/larrys/paperfigs/
FILES		:= shared-snps-full-qfiltered.pdf  \
		   shared-snps-full-unfiltered.pdf \
		   shared-snps-chr1-qfiltered.pdf  \
		   shared-snps-chr1-unfiltered.pdf \
		   shared-snps-full-qfiltered.tex  \
		   shared-snps-full-unfiltered.tex \
		   shared-snps-chr1-qfiltered.tex  \
		   shared-snps-chr1-unfiltered.tex \
		   paperfig-medium-tree-full-qfiltered.pdf  \
		   paperfig-medium-tree-full-unfiltered.pdf \
		   paperfig-medium-tree-Chr1-qfiltered.pdf  \
		   paperfig-medium-tree-Chr1-unfiltered.pdf \
		   paperfig-medium-tree-full.pdf
FILES		:=  $(addprefix $(CYCLEPATH), $(FILES))

#dryrun:
#	@echo PULL FROM \*CYCLE DRY RUN . . . . . . $(REXCLUDES)
#	rsync --dry-run -avz  $(REXCLUDES) $(CYCLEPATH) fromcycle/

#pull:	
#	@echo PULL FROM \*CYCLE
#	rsync           -avz  $(REXCLUDES) $(CYCLEPATH) fromcycle/


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
