#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright: University of Zurich
# Author: Rico Sennrich

# convert the Hamburg Dependency Treebank into CoNLL format (for training of statistics)
# usage: hamburg_treebank_to_conll.py input_folder > output

from __future__ import print_function
import sys
import os

coarsetag = {'ADJD':'ADV',
    'PDAT':'ART',
    'PIAT':'ART',
    'PIDAT':'ART',
    'PPOSAT':'ART',
    'PRELAT':'ART',
    'PWAT':'ART',
    'NE':'N',
    'NN':'N',
    'APPO':'PREP',
    'APPR':'PREP',
    'APPRART':'PREP',
    'PDS':'PRO',
    'PIS':'PRO',
    'PPER':'PRO',
    'PPOSS':'PRO',
    'PROP':'PROP',
    'PRELS':'PRO',
    'PRF':'PRO',
    'PWS':'PRO',
    'VAFIN':'V',
    'VAIMP':'V',
    'VAINF':'V',
    'VAPP':'V',
    'VMFIN':'V',
    'VMINF':'V',
    'VMPP':'V',
    'VVFIN':'V',
    'VVIMP':'V',
    'VVINF':'V',
    'VVIZU':'V',
    'VVPP':'V'
    }

def print_sentence(sentence):
    for word in sentence:
        if word['token'].startswith('\'') and word['token'].endswith('\''):
                word['token'] = word['token'][1:-1]
        if 'lemma' in word and word['lemma'].startswith('\'') and word['lemma'].endswith('\''):
            word['lemma'] = word['lemma'][1:-1]
        if word['pos'].startswith('\'') and word['pos'].endswith('\''):
            word['pos'] = word['pos'][1:-1]
        word['token'] = word['token'].replace('\\','')
        if 'lemma' in word:
            word['lemma'] = word['lemma'].replace('\\','')
        if word['label'] == "''":
            word['label'] = '-PUNCT-'
        try:
            print('\t'.join((word['position'], word['token'], word.get('lemma', word['token']), coarsetag.get(word['pos'], word['pos']), word['pos'], word.get('feature', '--'), word['head'], word['label'])))
        except KeyError:
            sys.stderr.write('error: info incomplete in file ' + f + '\n')
            raise
    print('')

files = [f for f in os.listdir(sys.argv[1]) if f.endswith('.cda')]

for f in sorted(files, key= lambda x: int(x[5:-4])):

    word = {}
    sentence = []
    for line in open(os.path.join(sys.argv[1],f)):
        if line == ',\n' or line == ';\n':
            sentence.append(word)
            word = {}

        elif line == '\n':
            continue

        else:
            line = line.split()
            if line[0] == 'base':
                word['lemma'] = line[2]
            elif line[0] == 'cat':
                word['pos'] = line[2]
            elif line[0] == 'case':
                word['feature'] = line[2][0]
            elif line[0] == 'SYN':
                try:
                    word['head'] = line[4]
                except IndexError:
                    sys.stderr.write('warning: failed to find head in sentence ' + f + '\n')
                    word['head'] = '0'
                word['label'] = line[2]
                # we don't distinguish between dative object and 'ethischer dativ'
                if word['label'] == 'ETH':
                    word['label'] = 'OBJD'
            else:
                try:
                    int(line[0])
                    word['position'] = line[1]
                    word['token'] = line[2]
                except:
                    pass

    print_sentence(sentence)
