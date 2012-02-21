#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright © 2011 University of Zürich
# Author: Rico Sennrich <sennrich@cl.uzh.ch>

from __future__ import division
import sys
from math import log

# identify the start of a new "real" sentence (i.e. a new block of n alternatives)
def identify_newsent(line,outputformat):
    
    if outputformat == 'prolog' and "'#0'" in line and line.split(',',2)[1].strip()=='1':
        return True
        
    elif outputformat == 'conll' and line.startswith("1\t#0\t#0"):
        return True
        
    elif outputformat == 'moses' and line.startswith('#0|'):
        return True
    
    return False


# extract the features that we later use for the selection
def feature_extract(sentbuf,outputformat):
    
    featurelist = []
    
    if outputformat == 'prolog':
        prob = float(sentbuf[0].split(',')[4].strip('\' '))
        featurelist.append(prob)
        root_count = sum((item.count('root') for item in sentbuf))-sum((item.count('$.') for item in sentbuf))-sum((item.count('$,') for item in sentbuf))-sum((item.count('$(') for item in sentbuf))
        featurelist.append(max(1,root_count))
        return featurelist

    elif outputformat == 'conll':
        prob = float(sentbuf[0].split()[3].rstrip())
        featurelist.append(prob)
        root_count = sum((item.count('\troot\t') for item in sentbuf))-sum((item.count('\t$.\t') for item in sentbuf))-sum((item.count('\t$,\t') for item in sentbuf))-sum((item.count('\t$(\t') for item in sentbuf))
        featurelist.append(max(1,root_count))
        return featurelist
        
        
    elif outputformat == 'moses':
        prob = float(sentbuf[0].split('|',2)[1])
        featurelist.append(prob)
        root_count = sum((item.count('root|root') for item in sentbuf))-sum((item.count('|$.|') for item in sentbuf))-sum((item.count('|$,|') for item in sentbuf))-sum((item.count('|$(|') for item in sentbuf))
        featurelist.append(max(1,root_count))
        return featurelist
        
        
def process_input(outputformat):
    
    features = {}
    sentlist = []
    sentbuf = []
    i = 1
    for line in sys.stdin:
    
        # empty line: sentence delimiter
        if line == '\n':
            if sentbuf:
                features[len(sentlist)] = feature_extract(sentbuf,outputformat)
                sentlist.append(sentbuf)
            sentbuf = []
            continue
    
        # new block of alternatives starts
        if identify_newsent(line,outputformat) and sentlist:
            sentence = select_output(features,sentlist)
            produce_output(i,sentence,outputformat)
            i += 1
            features = {}
            sentlist = []

        if outputformat == 'moses':
            features[len(sentlist)] = feature_extract([line],outputformat)
            sentlist.append([line])
        else:
            sentbuf.append(line)

    if sentbuf:
        features[len(sentlist)] = feature_extract(sentbuf,outputformat)
        sentlist.append(sentbuf)

    if sentlist:
        sentence = select_output(features,sentlist)
        produce_output(i,sentence,outputformat)


def fitness(feature):
    
    if feature[0] == 0:
        return float('inf')
        
    p = 1*log(feature[0])
    p += -5*log(feature[1])
    
    return -p
    

def select_output(features,sentlist):
    
    best = sorted(features, key=lambda i: fitness(features[i]))[0]
    return sentlist[best]
    

def produce_output(i,sentence,outputformat):
    
    if outputformat == 'prolog':
        for j,line in enumerate(sentence):
            if j == 0:
                continue
            print("word({0}, {1}, {2}".format(i,j,line.split(',',2)[-1])),
        print('')

    elif outputformat == 'conll':
        for j,line in enumerate(sentence):
            if j == 0:
                continue
            print("{0}\t{1}".format(j,line.split('\t',1)[-1])),
        print('')

    elif outputformat == 'moses':
        print(' '.join(sentence[0].split()[1:]))

if __name__ == '__main__':
    
    process_input(sys.argv[1])
