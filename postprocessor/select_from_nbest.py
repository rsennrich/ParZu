#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright © 2011 University of Zürich
# Author: Rico Sennrich <sennrich@cl.uzh.ch>

from __future__ import division
import sys
from math import log, exp

# identify the start of a new "real" sentence (i.e. a new block of n alternatives)
def identify_newsent(line,outputformat):

    if outputformat == 'prolog' and "'#0'" in line and line.split(',',2)[1].strip()=='1':
        return True

    elif outputformat == 'conll' and line.startswith("1\t#0\t#0"):
        return True

    return False


def get_rank(sentence, outputformat):
    if outputformat == 'prolog':
        return int(sentence[0].split(',')[3].strip('\' #'))
    elif outputformat == 'conll':
        return int(sentence[0].split()[2].strip('\t  #'))
    elif outputformat == 'moses':
        return int(sentence[0].split('|',2)[0].strip('#'))


def get_tagging_probability(sentence, outputformat):
    if outputformat == 'prolog':
        return float(sentence[0].split(',')[4].strip('\' '))
    elif outputformat == 'conll':
        return float(sentence[0].split()[3].rstrip())
    elif outputformat == 'moses':
        return float(sentence[0].split('|',2)[1])


def get_number_of_unattached_nodes(sentence, outputformat):
    root = 'root'
    ignore_tags = ['$.', '$,', '$(']

    if outputformat == 'conll':
        root = '\t' + root + '\t'
        ignore_tags = ['\t' + tag + '\t' for tag in ignore_tags]
    elif outputformat == 'moses':
        root = '|' + root + '|'
        ignore_tags = ['|' + tag + '|' for tag in ignore_tags]

    return sum((item.count(root) for item in sentence)) - sum((item.count(tag) for item in sentence for tag in ignore_tags))


def get_number_of_bad_labels(sentence, outputformat):
    bad_labels = ['app', 'cj', 'kon']

    if outputformat == 'prolog':
        bad_labels = [' ' + tag + ',' for tag in bad_labels]
    elif outputformat == 'conll':
        bad_labels = ['\t' + tag + '\t' for tag in bad_labels]
    elif outputformat == 'moses':
        bad_labels = ['|' + tag + '|' for tag in bad_labels]

    return sum((item.count(label) for item in sentence for label in bad_labels))


def feature_extract(sentbuf,outputformat):
    """for each sentence, extract a number of features which are used for the selection.
    """
    featurelist = []

    featurelist.append(get_tagging_probability(sentbuf, outputformat))
    featurelist.append(exp(get_number_of_unattached_nodes(sentbuf, outputformat)))
    featurelist.append(exp(get_rank(sentbuf, outputformat)))
    featurelist.append(exp(get_number_of_bad_labels(sentbuf, outputformat)))

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


def dot_product(a,b):
    """calculate dot product from two lists"""

    s = 0
    i = 0
    for x in a:
        s += x * b[i]
        i += 1

    return s


def fitness(features):
    """log-linear model that computes a score for each hypothesis, based on a weighted features.
    Weights have been optimized on development set"""

    try:
        weights = [1, -5, -0.5, -1.5]
        features = [log(f) for f in features]
        return -dot_product(weights, features)
    except ValueError:
        return float('inf')



def select_output(features,sentlist):
    
    best = sorted(features, key=lambda i: fitness(features[i]))[0]
    return sentlist[best]

def produce_output(i,sentence,outputformat):
    
    if outputformat == 'prolog':
        for j,line in enumerate(sentence):
            if j == 0:
                continue
            print("word({0}, {1}, {2}".format(i,j,line.split(',',2)[-1].lstrip())),
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
