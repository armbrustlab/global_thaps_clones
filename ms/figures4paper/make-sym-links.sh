#!/bin/bash

# make sym links to the various figures generated in scripts/larrys
#
# each individual 'ln' below will fail if a file with that target name
# already exists.  I.e., it's a no-op if the existing symlink already
# points to the right file, but delete an existing symlink before
# running this if trying to change where it points (or add -f to force
# removal/replacement)

ln -s ../../scripts/larrys/paperfigs/Fig1-mscat-figs/mscat-6-2-7.pdf Fig1-mscat-6-2-7.pdf
ln -s ../../scripts/larrys/paperfigs/Fig2-glue-figs-mine/Fig2.pdf Fig2.pdf
ln -s ../../scripts/larrys/shared-snps/figs-mine/paperfig-medium-tree-trunc-unfiltered--Fig3proto.pdf Fig3-paperfig-medium-tree-trunc-unfiltered.pdf
ln -s ../../scripts/larrys/tic/figs-mine/FigS4-hemizygosity.pdf FigS4-hemizygosity.pdf
ln -s ../../scripts/larrys/paperfigs/FigS5-SNPdip-figs/snpdip-chr1-1335.pdf FigS5-snpdip-chr1-1335.pdf
ln -s ../../scripts/larrys/paperfigs/Fig1-mscat-figs/mscat-1-4-7.pdf FigS6-1-mscat-1-4-7.pdf
ln -s ../../scripts/larrys/paperfigs/Fig1-mscat-figs/mscat-5-3-7.pdf FigS6-2-mscat-5-3-7.pdf
ln -s ../../scripts/larrys/paperfigs/Fig1-mscat-figs/mscat-null-3-6.pdf FigS6-3-mscat-null-3-6.pdf
ln -s ../../scripts/larrys/paperfigs/FigS7-hwe-histo-figs-mine/S7-full-qfiltered-1013chronly.pdf FigS7A-full-qfiltered-1013chronly.pdf
ln -s ../../scripts/larrys/paperfigs/FigS7-hwe-histo-figs-mine/S7-full-qfiltered-3367chronly.pdf FigS7B-full-qfiltered-3367chronly.pdf
ln -s ../../scripts/larrys/hwe/figure/qonetenth_homnr-1.pdf FigS8-hwe-qonetenth_homnr.pdf
ln -s ../../scripts/larrys/paperfigs/FigS9-desert-len-boxplot-figs-mine/FigS9-desert-len-boxplot-fig.pdf FigS9-desert-len-boxplot-fig.pdf
ln -s ../../scripts/larrys/asex/asex-figs-mine/Fig-S10A.pdf FigS10A-sim-complex-trait.pdf
ln -s ../../scripts/larrys/asex/asex-figs-mine/Fig-S10B.pdf FigS10B-sim-recessive.pdf
