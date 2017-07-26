#!/bin/bash -e

ROOTSYS=/root  # or wherever ROOT is installed (https://root.cern.ch/)

cd CNVnator
if [[ ! -e 20120525 ]]; then
    mkdir 20120525
fi
cd 20120525
if [[ ! -e seq ]]; then
    mkdir seq
fi

echo "Creating separated chromosome fasta files"
seqtools ids ../../reference/Thaps3.all.fasta | perl -lne 'if (/^Chr/) {$x=lc($_); print "$x\n$_"};' | \
parallel -N2 'seqtools subseq -q {2} ../../reference/Thaps3.all.fasta >seq/{1}.fa'

bamfiles=()
bamfiles+=(../../solid_runs/Tp1335/bam_20110505/Tp1335.bam)
bamfiles+=(../../solid_runs/Tp1007MP1/bam_20110505/Tp1007MP1.trim.noMateDups.bam)
bamfiles+=(../../solid_runs/Tp1012MP1/bam_20110505/Tp1012MP1.trim.noMateDups.bam)
bamfiles+=(../../solid_runs/Tp1013MP1/bam_20110505/Tp1013MP1.trim.noMateDups.bam)
bamfiles+=(../../solid_runs/Tp1014MP1/bam_20110505/Tp1014MP1.trim.noMateDups.bam)
bamfiles+=(../../solid_runs/Tp1015MP1/bam_20110505/Tp1015MP1.trim.noMateDups.bam)
bamfiles+=(../../solid_runs/IT/bam_20110505/IT.bam)

for bf in "${bamfiles[@]}"; do
#for bf in ../../solid_runs/IT/bam_20110505/IT.bam; do
    strain="$(basename "$bf")";
    strain=${strain%%.*};
    echo "Extracting chromsomes with lower case names for $strain"
    samtools view -h "$bf" | perl -lne 'if ($_ =~ /^@SQ.+Chr/ || /^\S+\s+\S+\s+Chr.*/) {s/Chr/chr/g; print}' | \
    samtools view -Sb - >"${strain}.bam"
    echo "Running CNVnator on $strain"
    
    for binsize in 50 100 150 200; do
        cnvnator -root "${strain}.$binsize.ngc.root" -unique -tree "${strain}.bam"
        cnvnator -root "${strain}.$binsize.ngc.root" -d seq -his "$binsize"
        cnvnator -root "${strain}.$binsize.ngc.root" -stat "$binsize"
        cnvnator -root "${strain}.$binsize.ngc.root" -partition "$binsize" -ngc
        cnvnator -root "${strain}.$binsize.ngc.root" -call "$binsize" -ngc >"${strain}.$binsize.ngc.calls"
        cnvnator -root "${strain}.$binsize.root" -tree "${strain}.bam"
        cnvnator -root "${strain}.$binsize.root" -d seq -his "$binsize"
        cnvnator -root "${strain}.$binsize.root" -stat "$binsize"
        cnvnator -root "${strain}.$binsize.root" -partition "$binsize"
        cnvnator -root "${strain}.$binsize.root" -call "$binsize" >"${strain}.$binsize.calls"
    done
    rm "${strain}.bam"
    echo
done
