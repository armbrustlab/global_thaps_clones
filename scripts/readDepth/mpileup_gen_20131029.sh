#!/bin/bash
python ./mpileup_samtools.py /share/projects/Thaps-strain-genomes/reference/Thaps3.all.fasta \
        /share/projects/Thaps-strain-genomes/solid_runs/Tp1335/bam_20110505/Tp1335.bam Tp1335 >Tp1335.log 2>&1
python ./mpileup_samtools_.py /share/projects/Thaps-strain-genomes/reference/Thaps3.all.fasta \
	/share/projects/Thaps-strain-genomes/solid_runs/Tp1007MP1/bam_20110505/Tp1007MP1.trim.noMateDups.bam \
	Tp1007 >Tp1007.log 2>&1
python ./mpileup_samtools.py /share/projects/Thaps-strain-genomes/reference/Thaps3.all.fasta \
	/share/projects/Thaps-strain-genomes/solid_runs/Tp1012MP1/bam_20110505/Tp1012MP1.trim.noMateDups.bam \
	Tp1012 >Tp1012.log 2>&1 
python ./mpileup_samtools.py /share/projects/Thaps-strain-genomes/reference/Thaps3.all.fasta \
	/share/projects/Thaps-strain-genomes/solid_runs/Tp1013MP1/bam_20110505/Tp1013MP1.trim.noMateDups.bam \
	Tp1013 >Tp1013.log 2>&1  
python ./mpileup_samtools.py /share/projects/Thaps-strain-genomes/reference/Thaps3.all.fasta \
	/share/projects/Thaps-strain-genomes/solid_runs/Tp1014MP1/bam_20110505/Tp1014MP1.trim.noMateDups.bam \
	Tp1014 >Tp1014.log 2>&1  
python ./mpileup_samtools.py /share/projects/Thaps-strain-genomes/reference/Thaps3.all.fasta \
	/share/projects/Thaps-strain-genomes/solid_runs/Tp1015MP1/bam_20110505/Tp1015MP1.trim.noMateDups.bam \
	Tp1015 >Tp1015.log 2>&1  
python ./mpileup_samtools.py /share/projects/Thaps-strain-genomes/reference/Thaps3.all.fasta \
	/share/projects/Thaps-strain-genomes/solid_runs/IT/bam_20110505/IT.bam \
	Tp3369 >Tp3369.log 2>&1  
