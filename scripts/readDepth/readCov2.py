#1/usr/bin/env python2.7

#Need to write a shell script to take in the various .mpu files
#and write out the tables to read into R. Should work in the
#pipeline which starts from the .BAM files

mpufiles = ['Tp1007.mpu','Tp1012.mpu','Tp1013.mpu','Tp1014.mpu',
'Tp1015.mpu','Tp3369.mpu','Tp1335.mpu']
st = ['Tp1007','Tp1012','Tp1013','Tp1014','Tp1015','Tp3369','Tp1335']
#mpufiles = ['Tp1335Ch1-Q30.mpu']
#st = ['Tp1335']

#snpfiles = ['tp1007.pos.tab','tp1012.pos.tab','tp1013.pos.tab',
#'tp1014.pos.tab','tp1015.pos.tab','tp3369.pos.tab', 'tp1335.pos.tab']

import string
import re
import collections
import sys

def file_len(fn):
    with open(fn) as f:
        for i, l in enumerate(f):
            pass
    return i + 1

c = collections.Counter
qualThresh = 50

for strain in zip(mpufiles, st):
    fname = strain[0]

    column = 6
    fd = open(fname, 'r')
    data0 = list()
    data1 = list()
    data2 = list()
    tst = list()
    countList = list()
    combList = []

    for line in fd:
        tokens = line.split('\t')
        data0.append(tokens[0:4])
        data1.append(tokens[column-2])
        data2.append([ord(x) - 33 for x in
                      tokens[column-1][0:(len(tokens[column-1])-1)]])

    caret = re.compile('\^')
    modCaret = list()


    for string in data1:
        length = len(string)
        string = str.lower(string)
        cList = list(caret.finditer(string))

        if(len(cList)==0):
            modCaret.append(string)
        else:
            newstring = ""
            start = 0
            stop = 0
            for i in range(len(cList)):
                stop = cList[i].start()
                newstring += string[start:stop]
                start = cList[i].end()+1
            if start == length:
                newstring += string[start - 1]
            elif start < length:
                newstring += string[start:len(string)]
            modCaret.append(newstring)


    dsandstar = re.compile('[$]')
    modDSstar = list()

    for string in modCaret:
        length = len(string)
        dssList = list(dsandstar.finditer(string))
        if(len(dssList) == 0):
            modDSstar.append(string)
        else:
            newstring = ""
            start = 0
            stop = 0
            for i in range(len(dssList)):
                stop = dssList[i].start()
                newstring += string[start:stop]
                start = dssList[i].end()
                if start > length:
                    break
            if start == length:
                print("")
            elif start < length:
                newstring += string[start:len(string)]
            modDSstar.append(newstring)


    plusMinus = re.compile('[-+]')
    jj = 0
    for qstring in zip(modDSstar, data2):
        string = qstring[0]
        qual = qstring[1]
        length = len(string)
        pmList = list(plusMinus.finditer(string))
        pmDeets = plusMinus.findall(string)
        if(len(pmList)==0):
            if(len(string) == len(qual)):
                qstring = [a[0] for a in zip(string, qual) if a[1] > qualThresh]
                cnt =c(qstring)
                countList.append([str(cnt['a']), str(cnt['g']), str(cnt['c']),
                                  str(cnt['t']), str(cnt['n']), str(cnt['.']+cnt[','])])
                jj = jj+1
            else:
                print("Number of bases does not equal the number of qualities")
                print(jj)
                sys.exit()
        else:
            newstring = ""
            start = 0
            stop = 0
            for i in range(len(pmList)):
                stop = pmList[i].start()
                newstring += string[start:stop]

                if(string[pmList[i].end()+1].isdigit()):
                    start = pmList[i].end() + int(string[pmList[i].end()]+string[pmList[i].end()+1]) + 2
                else:
                    start = pmList[i].end() + int(string[pmList[i].end()]) + 1

            if start < length:
                newstring += string[start:]

            if(len(newstring) == len(qual)):
                qnewstring = [a[0] for a in zip(newstring, qual) if a[1] > qualThresh]
                cnt = c(qnewstring)
                countList.append([str(cnt['a']), str(cnt['g']), str(cnt['c']), str(cnt['t']),
                                  str(cnt['n']), str(cnt['.']+cnt[','])])
                jj = jj+1
            else:
                print("Number of bases does not equal the number of qualities")
                print(jj)
                sys.exit()

    for i in range(len(data0)):
        combList.append(data0[i]+countList[i])

    combList.insert(0, ['Chr', 'Pos', 'Ref', 'Cov', 'a', 'g', 'c', 't', 'n', '.match' ])

    with open(strain[1]+'-Q50.tab', 'w') as file:
        file.writelines('\t'.join(i) + '\n' for i in combList)

        fd.close()


