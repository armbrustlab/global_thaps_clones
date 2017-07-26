#!/usr/bin/env python2.7
"""Call SNPs in a BAM file"""
import os, sys
import argparse
from numpy import median, ceil
from itertools import chain
#import bamfilter_cov
#No longer a snp calling script. I have hacked CB's script so that
#it produces an mpileup file 
if __name__ == "__main__":
    parser = argparse.ArgumentParser(
    description="""Call SNPs in a BAM file, output in BCF and VCF format""",
    formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("-c", dest="median_cov_mult", default=3, type=int,
    help="""Coverage cutoff for SNP positions in units of median Chr
    coverage.""")
    parser.add_argument("-t", dest="t", default=".007", type=str,
    help="""-t argument passed to bcftools view.  Scaled mutation rate.""")
    parser.add_argument(dest="ref", help="""Reference fasta file""")
    parser.add_argument(dest="bam", help="""BAM file""")
    parser.add_argument(dest="out_prefix", help="""Out file prefix""")
    args = parser.parse_args()
    
    #cov = bamfilter_cov.calc_coverage(args.bam, no_strand=True)
    #for refname in [k for k in cov if (not k.startswith("Chr"))]:
    #    del cov[refname]
    #median_cov = int(ceil(median(list(chain(*cov.values())))))
    #max_cov = args.median_cov_mult*median_cov
    #print "median coverage = %i" % median_cov
    #print "max coverage considered = 3*median = %i" % max_cov
    
    # -C50 = penalize high mismatch reads
    # -A = use reads in anomalous pairs
    # -B = turn off BAQ correction
    # -u = uncompressed BCF output
    # -g = output genotyping likelihoods in BCF format
    # -q1 = only use reads with MAPQ >= 1
    # -f = faidx indexed reference file
    cmd = "samtools mpileup -C50 -A -B -q1 -f -Q30 %s %s >%s.mpu" % (args.ref,
                                                                  args.bam, args.out_prefix)
    # -b = output compressed BCF format
    # -g = call genotypes at variant sites
    # -t = scaled substitution mutation rate
    #cmd += "bcftools view -b -g -t %s - >%s.raw_var.bcf &&" % (args.t,
    #                                                           args.out_prefix)
    #cmd += "bcftools view %s.raw_var.bcf | " % (args.out_prefix)
    # -D = max coverage
    #cmd += "vcfutils.pl varFilter -D %i >%s.filt_var.vcf " % (max_cov,
    #                                                           args.out_prefix)
    #cmd += "vcfutils.pl varFilter -D %i >%s.filt_var.vcf " % (10000,
    #                                                          args.out_prefix)
    #cmd += "&& igvtools index %s.filt_var.vcf" % (args.out_prefix)
    if os.system(cmd):
	sys.stderr.write("SNP calling pipeline exited with errors\n")
	sys.exit(1)
    #else:
    #    os.remove("igv.log")
