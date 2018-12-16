#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright ©2011 University of Zürich
# Author: Rico Sennrich <sennrich@cl.uzh.ch>

from __future__ import unicode_literals, print_function
import sys
import re
import codecs
from collections import defaultdict


map_stts = {}
map_stts['DEM'] = 'PD'
map_stts['INDEF'] = 'PI'
map_stts['POSS'] = 'PPOS'
map_stts['REL'] = 'PREL'
map_stts['WPRO'] = 'PW'
map_stts['PPRO'] = 'PPER'
map_stts['PREP/ART'] = 'APPRART'
map_stts['PREPART'] = 'APPRART'
map_stts['PREP'] = 'APPR'
map_stts['ORD'] = 'ADJA'
map_stts['POSTP'] = 'APPO'
map_stts['CIRCP'] = 'APZR'
map_stts['VPART'] = 'PTKVZ'
map_stts['VPRE'] = 'PTKVZ'
map_stts['PROADV'] = 'PAV'
map_stts['INTJ'] = 'ITJ'
map_stts['SYMBOL'] = 'XY'
map_stts['WADV'] = 'PWAV'
map_stts['CHAR'] = 'XY'
map_stts['NPROP'] = 'NE'

word = ''
re_mainclass = re.compile('<\+(.*?)>')
re_any = re.compile('<(.*?)>')
re_segment = re.compile('<([A-Z#\-~]*?)>')
re_last = re.compile('(?:^|\W)([\w\.]+?)(?:<[\w\-\^]*>)*?<\+',re.UNICODE)
re_hyphenation = re.compile('{(.+?)}-')

def get_repr(key,d):
    if key in d:
        return "'"+d[key].replace("'",r"\'")+"'"
    else:
        return '_'
    
def get_repr2(word):
    word = word.replace("\\","\\\\")
    return "'"+word.replace("'",r"\'")+"'"

#create a prolog list with morphological information in the right order
def create_morph(word,pos,args):
    if pos == 'NN':
        morph = "[{gender},{case},{number},_]".format(gender=get_repr('gender',args),case=get_repr('case',args),number=get_repr('number',args))
        
    elif pos == 'NE':
        morph = "[{gender},{case},{number}]".format(gender=get_repr('gender',args),case=get_repr('case',args),number=get_repr('number',args))

    elif pos == 'ART':
        morph = "[{definiteness},{gender},{case},{number}]".format(definiteness=get_repr('definiteness',args),gender=get_repr('gender',args),case=get_repr('case',args),number=get_repr('number',args))

    elif pos in ['APPR','APPO','APZR']:
        morph = "[{case}]".format(case=get_repr('case',args))
        
    elif pos == 'APPRART':
        morph = "[{gender},{case}]".format(gender=get_repr('gender',args),case=get_repr('case',args))

    elif pos in ('PDS','PDAT','PIDAT','PIS','PIAT','PPOSAT','PPOSS','PRELS','PRELAT','PWS','PWAT'):
        morph = "[{gender},{case},{number}]".format(gender=get_repr('gender',args),case=get_repr('case',args),number=get_repr('number',args))
      
    elif pos == 'PRF':
        morph = "[{person},{number},{case}]".format(person=get_repr('person',args),case=get_repr('case',args),number=get_repr('number',args))
        
    elif pos == 'PPER':
        morph = "[{person},{number},{gender},{case}]".format(person=get_repr('person',args),gender=get_repr('gender',args),case=get_repr('case',args),number=get_repr('number',args))
        
    elif pos == 'ADJA':
        morph = "[{grade},{gender},{case},{number},{declension}]".format(grade=get_repr('grade',args),gender=get_repr('gender',args),case=get_repr('case',args),number=get_repr('number',args),declension=get_repr('declension',args))
        
    elif pos in ['ADJD']:
        morph = '[{grade}]'.format(grade=get_repr('grade',args))
        
    elif pos.endswith('FIN'):
        morph = "[{person},{number},{tense},{aspect}]".format(aspect=get_repr('aspect',args),tense=get_repr('tense',args),number=get_repr('number',args),person=get_repr('person',args))
        
    elif pos.endswith('IMP'):
        morph = "[{number}]".format(number=get_repr('number',args))
   
    else:
        if not pos in ['ADV','CARD','ITJ','PTKVZ','PAV','PTKA','PTKZU','PTKANT','PTKNEG','PTKVZ','PWAV','KOUS','KOKOM','KON','KOUI','TRUNC','VVPP','VAPP','VMPP','VAINF','VMINF','VVINF','VVIZU','XY','$(','$,','$.']:
            sys.stderr.write('undefined PoS: ' + str(pos) + '\n')
        morph = '_'
        
    return morph
    
#get all morphological features from the morphisto output
def extract(line):
    
    d = {}
    
    features = re_any.findall(line)
    
    for feature in features:
        
        if feature in ['Masc','Fem','Neut']:
            d['gender'] = feature
            
        elif feature in ['Sg','Pl']:
            d['number'] = feature
            
        elif feature in ['Nom','Akk','Acc','Dat','Gen']:
            d['case'] = feature
        
        elif feature in ['Sw','St','St/Mix', 'Wk']:
            d['declension'] = feature
            
        elif feature in ['Pos','Comp','Sup']:
            d['grade'] = feature
            
        elif feature in ['Ind','Konj','Subj']:
            d['aspect'] = feature
            
        elif feature in ['1','2','3']:
            d['person'] = feature
            
        elif feature in ['Pres', 'Past']:
            d['tense'] = feature
            
        elif feature in ['Def', 'Indef']:
            d['definiteness'] = feature
            
        elif feature == 'PPres':
            d['derivation'] = '<PPRES'
            
        elif feature == 'PPast':
            d['derivation'] = '<PPAST'

    if '<~>end<+ADJ>' in line or '<~>nd<+ADJ>' in line:
        d['derivation'] = '<PPRES'

    return d
    

#get stts part_of_speech tag from morphisto output
def get_true_pos(raw_pos,line):
    pos = map_stts.get(raw_pos,raw_pos)
    pos2 = None
    other = ''

    if raw_pos == 'V': 
    
        #stts tagset distinguishes between VV, VA and VM
        if line.startswith('<CAP>'):
            line = line[5:]
        if line.startswith('haben') or line.startswith('hab<~>en') or line.startswith('werden') or line.startswith('werd<~>en') or line.startswith('sein'):
            pos += 'A'
        elif line.startswith('dürfen') or line.startswith('dürf<~>en') or line.startswith('können') or line.startswith('könn<~>en') or line.startswith('sollen') or line.startswith('soll<~>en') or line.startswith('müssen') or line.startswith('müss<~>en') or line.startswith('mögen') or line.startswith('mög<~>en') or line.startswith('wollen') or line.startswith('woll<~>en'):
            pos += 'M'
        else:
            pos += 'V'
        
        #stts tagset distinguishes between VVINF, VVFIN, VVPP and VVIZU
        if '<Inf>' in line:
            if '<zu>' in line:
                pos += 'IZU'
            else:
                pos += 'INF'
        elif '<PPast>' in line:
            pos += 'PP'
        elif '<Ind>' in line or '<Konj>' in line or '<Subj>' in line:
            pos += 'FIN'
            
        elif '<Imp>' in line:
            pos += 'IMP'
            
        elif '<PPres>' in line:
            pos = 'ADJD'
            
        else:
            sys.stderr.write('FIN or INF or PP?: '+line+'\n')
    
    #distinction between ADJA and ADJD
    elif raw_pos == 'ADJ':
        if '<Pred>' in line or '<Adv>' in line:
            pos += 'D'
        else:
            pos += 'A'
    
    #map pronouns to stts tagset
    elif pos in ['PD','PI','PP','PREL','PW','PPOS']:
        
        if '<pro>' in line or '<Pro>' in line:
            if pos == 'PI' and ('<mD>' in line or '<Invar>' in line):
                pos2 = pos + 'DAT'
            else:
                pos2 = pos + 'AT'
            pos += 'S'
        elif '<subst>' in line or '<Subst>' in line:
            pos += 'S'
        else:
            if pos == 'PI' and ('<mD>' in line or '<Invar>' in line):
                pos += 'DAT'
            else:
                pos += 'AT'
           
    elif raw_pos == 'KONJ' or raw_pos == 'CONJ':
        if '<Vgl>' in line or '<Compar>' in line:
            pos = 'KOKOM'
        elif '<Inf>' in line:
            pos = 'KOUI'
        elif '<Sub>' in line:
            pos = 'KOUS'
        elif '<Kon>' in line or '<Coord>' in line:
            pos = 'KON'
            
    elif raw_pos == 'PTKL' or raw_pos == 'PTCL':
        if '<Ant>' in line or '<Ans>' in line:
            pos = 'PTKANT'
        elif '<Neg>' in line:
            pos = 'PTKNEG'
        elif '<zu>' in line:
            pos = 'PTKZU'
        elif '<Adj>' in line:
            pos = 'PTKA'
        elif '<Vz>' in line:
            pos = 'PTKVZ'
          
    elif pos == 'PPER':
        if '<refl>' in line or '<Refl>' in line:
            pos = 'PRF'
        elif '<prfl>' in line or '<Prfl>' in line:
            pos = 'PRF'
            pos2 = 'PPER'
            
    elif pos == 'PUNCT' or pos == 'IP':
        if '<Left>' in line or '<Right>' in line or '<links>' in line or '<rechts>' in line:
            pos = '$('
        elif '<Norm>' in line:
            pos = '$.'
        elif '<Comma>' in line or '<Komma>' in line:
            pos = '$,'

    return pos,pos2


def getlemma(line,word,pos):

    # map {CDU}-Fraktion to CDU-Fraktion
    line = re_hyphenation.sub(r'\1-', line)
    lemma = re_any.sub('',line) #delete all markup, leaving what we'll use as lemma


    #SMOR gives the same lemma to er/sie/es; keep the distinction in ParZu
    #(this is slightly redundant, since we can get the same info from the gender, but the grammar looks at the lemma to find cases of expletive 'es')
    if lemma == 'sie':
        if '<3><Sg><Neut>' in line:
            lemma = 'es'
        elif '<3><Sg><Masc>' in line:
            lemma = 'er'
        elif '<1><Sg>' in line:
            lemma = 'ich'
        elif '<2><Sg>' in line:
            lemma = 'du'
        elif '<1><Pl>' in line:
            lemma = 'wir'
        elif '<2><Pl>' in line:
            lemma = 'ihr'

    return lemma


# print analyses with fewest morphemes first
# useful because parser uses greedy lemmatisation
def print_cache(cache, outstream, outputs):
    printed = set()
    for pos in cache:
        for item in sorted(cache[pos]):
            if item[1] not in printed:
                printed.add(item[1])
                output = item[1]
                if outstream is None:
                    outputs.append(output)
                else:
                    outstream.write(output + '\n')


#longest common subsequence code from http://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Longest_common_subsequence
def LCS(X, Y):
    m = len(X)
    n = len(Y)
    # An (m+1) times (n+1) matrix
    C = [[0] * (n+1) for i in range(m+1)]
    for i in range(1, m+1):
        for j in range(1, n+1):
            if X[i-1] == Y[j-1]:
                C[i][j] = C[i-1][j-1] + 1
            else:
                C[i][j] = max(C[i][j-1], C[i-1][j])
    return C

def backTrack(C, X, Y, i, j):
    if i == 0 or j == 0:
        return ()
    elif X[i-1] == Y[j-1]:
        return backTrack(C, X, Y, i-1, j-1) + (i-1,)
    else:
        if C[i][j-1] < C[i-1][j]:
            return backTrack(C, X, Y, i-1, j)
        else:
            return backTrack(C, X, Y, i, j-1)

def main(instream, outstream=None):
    """read input file (or list of lines) of morphological analyses in SMOR format,
    produced by an SMOR/SMORLemma model, and convert it into a prolog-readable format.
    Also does some mapping of feature names for legacy reasons, like mapping POS tags into STTS.

    Output is either a file-like object or None. If output is None, function returns a list of strings."""

    cache = defaultdict(set)
    outputs = []

    for line in instream:
        
        lemma = ''
        pos = ''
        morph = []
        other = "''"
        
        line = line.rstrip()

        if line.startswith('> ') or line == '>':
            word = line[2:]
            print_cache(cache, outstream, outputs)
            cache = defaultdict(set)
            continue

        elif line.startswith('no result'):
            output = "gertwol({0},'<unknown>',_,_,_).".format(get_repr2(word))
            if outstream is None:
                outputs.append(output)
            else:
                outstream.write(output + '\n')
            continue

        elif line.startswith('><+') or line.startswith('<<+'):
            output = "gertwol({0},{0},'$(',_,'').".format(get_repr2(line[0]))
            if outstream is None:
                outputs.append(output)
            else:
                outstream.write(output + '\n')
            continue

        try:
            raw_pos = re_mainclass.search(line).group(1)
        except:
            sys.stderr.write(line)
            raise

        pos,pos2 = get_true_pos(raw_pos,line)
            
        #we analyse the present participle as adverb/adjective, and discard this analysis (which has no equivalent in STTS)
        if pos == 'VV' and '<PPres>' in line:
            continue
            
        extracted_morph = extract(line)
        morph = create_morph(word,pos,extracted_morph)
        
        if 'derivation' in extracted_morph:
            other = get_repr('derivation',extracted_morph)

        if pos2:
            morph2 = create_morph(word,pos2,extracted_morph)

        lemma = getlemma(line,word,pos)
        
        #score number of morphemes; heuristic adopted from SFST
        segments = len(re_segment.findall(line))
        if line.startswith('<CAP>'):
            segments -= 1

        cache[pos].add((segments,"gertwol({0},{1},{2},{3},{4}).".format(get_repr2(word),get_repr2(lemma),get_repr2(pos),morph,other)))
        if pos2:
            cache[pos2].add((segments,"gertwol({0},{1},{2},{3},{4}).".format(get_repr2(word),get_repr2(lemma),get_repr2(pos2),morph2,other)))

    print_cache(cache, outstream, outputs)

    if outstream is None:
        return outputs

if __name__ == '__main__':

    if sys.version_info < (3, 0):
        sys.stdout = codecs.getwriter('UTF-8')(sys.stdout)
        sys.stderr = codecs.getwriter('UTF-8')(sys.stderr)
        sys.stdin = codecs.getreader('UTF-8')(sys.stdin)

    main(sys.stdin, sys.stdout)
