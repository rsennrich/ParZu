# -*- coding: utf-8 -*-
import sys

try:
    mode = sys.argv[1]
except:
    sys.stderr.write('first argument must be \'word\' or \'lemma\'\n')
    sys.stderr.write('example call: python conll2prolog.py word < tueba6_goldpos.utf8.conll > tueba.pl\n')
    exit()

i = 1

refl_mapping = {'uns':'wir', 'sich':'er','mich':'ich','dich':'du','mir':'ich','dir':'du','euch':'ihr'}

for line in sys.stdin:
    if line == '\n':
        i += 1
        continue
        
    pos,word,lemma,ctag,tag,morph,head,label = line.strip().split()[:8]
    

    # map some lemmas in Tüba-D/Z to a format more compatible with morphological analyzers
    if mode == 'lemma':

        if lemma == '#refl':
            try:
                lemma = refl_mapping[word.lower()]
            except:
                lemma = word
            
        if len(lemma) > 1:
            
            if '%' in lemma:
                lemma = lemma[:lemma.index('%')]
            if '|' in lemma:
                lemma = lemma[:lemma.index('|')]
            lemma = lemma.replace("#","")
        
        token = lemma
        
    elif mode == 'word':
        token = word
    
    #escape prolog special characters
    token = token.replace("\\","\\\\")
    token = token.replace("'","\\\'")
    
    label = label.upper()

    ## parser output has different morphology format than Tüba: This maps it to the right form (only for prepositions,
    ## which are the only thing that matter for the current statistics)
    if '|' in morph:
        if ctag == 'PREP':
            if morph != '_':
                morph = [morph[0].lower()]
            else:
                morph = ['-']
        else:
            morph = ['-','-']

    else:
        morph = list(morph)


    # in TüBa, we reserve first 4000 sentences for development/testing; can be disabled for other corpora
    #if i > 4000:
    print "w({0}, {1}, '{2}', '{3}', '{4}', {5}, {6}).".format(i,pos,token,tag,label,head,morph)
