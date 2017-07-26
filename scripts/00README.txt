Scripts Directory
=================

2017-07-23:
I have reorganized most code initially located in Thaps_7_strains/code/ (and elsewhere) into various subdirectories of this directory (originally Thaps_7_strains/code/snpNB/scripts/, now Thaps_7_strains/scripts).  

Warning #1: I have not updated paths occasionally embedded in these files, so some scripts may break.

Warning #2: I was guessing as to the purpose of many of these scripts, so the categorization outlined below is doubtless imperfect, and some code may be misclassified.

That said, the subdirectories are:

  CNV            —  copy number variation analysis
  exploratory    —  general exploratory analysis
  larrys         —  my code
  misc-misc      —  miscellany
  misc-seastar   —  originally Thaps_7_strains/code/snpNB/inst/extdata/seastar
  Nao            —  Nao’s phasing analysis
  qual           —  part of read qual-filtering analysis, I think
  readDepth      —  originally Thaps_7_strains/code/snpNB/inst/extdata/readDepth;
                    includes code to build the genome-wide .rda tables
  SexGenes       —  Julie’s analysis of the the 3 sex genes
  snpNB          —  stuff related to the Neg-Binomial model, deserts, hot spots, etc.
  util           —  general utility code
  vcf            —  vcf file utils & SNP calling

Additionally, scripts & data related to differential expression analysis were moved to Thaps_7_strains/analysis-RNAseq, some old figures and notes were consolidated in Thaps_7_strains/ms/old/,  etc.

Finally, files that appeared with the same name in different places are generally differentiated by appending the last-modified file date to the file name; presumably the newest version is the most relevant one.
