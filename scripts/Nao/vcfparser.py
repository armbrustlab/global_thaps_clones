import sys
import matplotlib.pyplot as plt
import math
import numpy as np

class VCF:
    def __init__(self, filename):
        self.source = filename
        self.variants = []
        self.SNPs = []
        self.variants_dict = {}
        self.countSNP = 0
        self.countINDEL = 0
        self.countVAR = 0
        self.countHAPBLOCK = 0
        self.lengthTotal = 0
        self.avgDEPTH = 0

        #reading in a file
        f = open(filename, 'r')
        lines = f.readlines()
        for line in lines:
            if line[0] != "#":
                v = Variant(line)
                self.variants.append(v)
                self.variants_dict[(v.chrom, v.pos)] = v
                if v.isSNP:
                    self.countSNP += 1
                    self.SNPs.append(v)
                else:
                    self.countINDEL += 1
            else:
                #currently ignoring vcf headers
                pass

        self.countVAR = len(self.variants)

        #construct haplotype blocks
        self.hapDict = {}
        self.constructHapDict()
        self.lengthTotal = self.findTotalDistanceCovered()

        #calculating average depth
        depth = []
        for v in self.variants:
            depth.append(int(v.info["DP"]))
        self.avgDEPTH = sum(depth)*1.0/len(depth)
        self.stdevDEPTH = np.std(np.array(depth))
        
        self.print_stats()

    def constructHapDict(self, pq_threshold=20):
        for v in self.variants:
            #If it is phased and PQ is higher than the threshold
            if v.isPhased and float(v.format["PQ"])>pq_threshold:
                temp = self.hapDict.get((v.chrom, v.format["HP"].pos), [])
                temp.append(v)
                self.hapDict[(v.chrom, v.format["HP"].pos)] = temp

        for k, v in self.hapDict.items():
            if len(v) == 1:
                del self.hapDict[k]

        self.countHAPBLOCK = len(self.hapDict)

    def getHapBlockByPos(self, chrom, pos):
        for key in self.hapDict.keys():
            if key[0] == chrom:
                cand = self.hapDict[key]
                for snp in cand:
                    if pos == snp.pos:
                        return cand
        return None

    def getVariant(self, chrom, pos):
        return self.variants_dict[(chrom, pos)]
    
    def print_stats(self):
        print "======="+self.source+"======="
        print "#SNPs:", self.countSNP 
        print "#INDELs:", self.countINDEL 
        print "#VARs:", self.countVAR 
        print "#HAPBLOCKs:", self.countHAPBLOCK
        print "AVG SNP DEPTH:", self.avgDEPTH
        print "TOTAL LENGTH COVERED:", self.lengthTotal, "bps"
        print

    def findTotalDistanceCovered(self):
        covered = {}
        dist = 0

        #Tuplizing snp data
        for key in self.hapDict.keys():
            #computing a tuple that indicates the covered region.
            block = self.hapDict[key]
            _min = float('inf')
            for snp in block:
                if snp.pos < _min:
                    _min = snp.pos
            _max = -1*float('inf')
            for snp in block:
                if snp.pos > _max:
                    _max = snp.pos
            region = (_min,_max)
                
            chrom = key[0]
            temp = covered.get(chrom, [])
            temp.append(region)
            covered[chrom] = temp

        for chrom in covered.keys():
            subdist = 0
            cover_chr = covered[chrom]
            cover_chr = sorted(cover_chr)
            total = []
            for r in cover_chr:
                if len(total) == 0:
                    total.append(r)
                else:
                    if total[-1][1] > r[0]:
                        if total[-1][1] > r[1]:
                            pass
                        else:
                            temp = total[-1]
                            total[-1] = (temp[0], r[1])
                    else:
                        total.append(r)

            for i in total:
                subdist = subdist + (i[1]-i[0])

            dist += subdist
        return dist

class Variant:
    #CHROM  POS     ID      REF     ALT     QUAL    FILTER  INFO    FORMAT
    def __init__(self, line):
        #spliting first by tab
        temp = line.split("\t")
        self.chrom = temp[0]
        self.pos = int(temp[1])
        self.ID = temp[2]
        self.ref = temp[3]
        self.alt = temp[4]
        self.qual = float(temp[5])
        self.filter = temp[6]

        if len(self.ref) == 1 and len(self.alt) == 1:
            self.isSNP = True
        else:
            self.isSNP = False

        #building a dictionary of tagged values
        tempinfo = temp[7]
        tempinfo = tempinfo.split(";")
        self.info = {}
        for i in tempinfo:
            element = i.split("=")
            if len(element)!=1:
                self.info[element[0]] = element[1]
            else:
                self.info[element[0]] = element[0]
                
        tempformatkeys = temp[8]
        tempformatkeys = tempformatkeys.split(":")

        tempformatvals = temp[9].rstrip('\n')
        tempformatvals = tempformatvals.split(":")
        self.format = {}
        for i in range(len(tempformatvals)):
            self.format[tempformatkeys[i]] = tempformatvals[i]

        self.isPhased = False

        #extracting haplotype related information
        if self.format.get("HP", False):
            self.isPhased = True
            phasedinfo = self.format["HP"]
            self.format["HP"] = block_info(phasedinfo)

            #adjust PQ values
            if self.format.get("PQ", None)==None:
                self.format["PQ"] = float("inf")
            else:
                self.format["PQ"] = float(self.format["PQ"])

    #implement str function
    def __str__(self):
        return str((self.chrom, self.pos))
    
    def __repr__(self):
        return str((self.chrom, self.pos))

class block_info:
    def __init__(self, info):
        temp = info.split(",")
        self.pos = int(temp[0].split("-")[0])
        self.ref = int(temp[0].split("-")[-1])
        self.alt = int(temp[1].split("-")[-1])
            
def main():
    #f = open("1014_phased.vcf", 'r')
    VCF("1014_phased.vcf")

            
if __name__ == "__main__":
    main()



