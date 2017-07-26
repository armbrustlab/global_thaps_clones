TARGET	  := tic
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
# knitr doesn't neet it but for Sweave - put following in preamble of .rnw(.tex) file)
# % to use Sweave, need this package to latex this file, but if I only use \Sexpr{}, then all other traces of sweave 
# % gone from resulting .tex file, so this is not needed in actual HW sols.tex (tho easily added if desired; see 
# % &texheader in weblib.pl)
# \usepackage{/Library/Frameworks/R.framework/Resources/share/texmf/tex/latex/Sweave}
# 
# in either case, keep the .tex file read-only, so I remember to edit the .rnw
tex:
	if [ -f $(TARGET).tex ] ; then mv  -f $(TARGET).tex{,.$$$$.bak} ; fi
	R --no-save -e "library(knitr);knit('$(TARGET).rnw')"
	chmod a-wx $(TARGET).tex                      ## edit .rnw, not .tex

./%.tex:	%.rnw
	if [ -f $(notdir $*).tex ] ; then mv  -f $(notdir $*).tex{,.$$$$.bak} ; fi
	R --no-save -e "library(knitr);knit('$(notdir $*).rnw')"
	chmod a-wx  $(notdir $*).tex            ## edit .rnw, not .tex

./%.pdf:	%.tex Makefile $(STYS)			## make .pdf from .tex
	(cd $(dir $@); \
	 pdflatex $(notdir $*).tex; \
	 grep "Label(s) may have changed. Rerun to get" $(notdir $*).log && pdflatex $(notdir $*).tex; \
	 grep "Label(s) may have changed. Rerun to get" $(notdir $*).log && pdflatex $(notdir $*).tex; \
	 grep "Label(s) may have changed. Rerun to get" $(notdir $*).log && pdflatex $(notdir $*).tex; \
	 rm -f $(notdir $*).{out} \
		)

echo:
	@echo "DIRS:      $(DIRS)"
	@echo "EXCEPTRNW: $(EXCEPTRNW)"
	@echo "EXCEPTTEX: $(EXCEPTTEX)"
	@echo "STYS:      $(STYS)"
	@echo "RNWS:      $(RNWS)"
	@echo "TEXS:      $(TEXS)"
	@echo "RNW-TEX:   $(RNW-TEX)"
	@echo "TEX-PDF:   $(TEX-PDF)"
