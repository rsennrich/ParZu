#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright ©2011 University of Zürich
# Author: Rico Sennrich <sennrich@cl.uzh.ch>

import sys
import re
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
re_mainclass = re.compile(u'<\+(.*?)>')
re_any = re.compile(u'<(.*?)>')
re_segment = re.compile(u'<([A-Z]*?)>')
re_last = re.compile(u'(?:^|\W)([\w\.]+?)(?:<[\w\-\^]*>)*?<\+',re.UNICODE)

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
            
        elif feature in ['Nom','Akk','Dat','Gen']:
            d['case'] = feature
        
        elif feature == 'Acc':
            d['case'] = 'Akk'
        
        elif feature in ['Sw','St','St/Mix']:
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
        if line.startswith('haben') or line.startswith('werden') or line.startswith('sein'):
            pos += 'A'
        elif line.startswith(u'dürfen') or line.startswith(u'können') or line.startswith('sollen') or line.startswith(u'müssen') or line.startswith(u'mögen') or line.startswith(u'wollen'):
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
            sys.stderr.write('FIN or INF or PP?: '+line.encode("UTF-8")+'\n')
    
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

    if pos == 'NN':
    #sadly complicated hack to get desired lemmata for nouns. Morphisto normalizes all morphemes in the stem, which we don't want.
    #using longest common subsequence matching to find boundary, taking normalized last_morpheme, unnormalized stem.
        last_morpheme = re_last.search(line).group(1)
        word_lc = word.lower()
        last_morpheme_lc = last_morpheme.lower()
        try:
            joinpoint = backTrack(LCS(word_lc,last_morpheme_lc),word_lc,last_morpheme_lc,len(word_lc),len(last_morpheme_lc))[0]
        except IndexError:
            lemma = re_any.sub('',line)
            try:
                return lemma[0] + lemma[1:].lower()
            except IndexError:
                return lemma
        if joinpoint > 1:
            if word[joinpoint-1] == '-':
                return re_any.sub('',word[:joinpoint])+last_morpheme
            else:
                return re_any.sub('',word[:joinpoint])+last_morpheme_lc
        else:
            return last_morpheme
    
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

    try:
        return lemma[0] + lemma[1:].lower()
    except IndexError:
        return lemma


# print analyses with fewest morphemes first
# useful because parser uses greedy lemmatisation
def print_cache(cache):
    for pos in cache:
        for item in sorted(cache[pos]):
            print item[1].encode('UTF-8')


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



cache = defaultdict(list)

for line in sys.stdin:
    
    lemma = ''
    pos = ''
    morph = []
    other = "''"
    
    line = line.rstrip().decode('UTF-8')

    if line.startswith('>'):
        word = line[2:]
        print_cache(cache)
        cache = defaultdict(list)
        continue

    if line.startswith('no result'):
        print(u"gertwol({0},'<unknown>',_,_,_).".format(get_repr2(word)).encode('UTF-8'))
        continue
    
    raw_pos = re_mainclass.search(line).group(1)

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

    cache[pos].append((segments,u"gertwol({0},{1},{2},{3},{4}).".format(get_repr2(word),get_repr2(lemma),get_repr2(pos),morph,other)))
    if pos2:
        cache[pos2].append((segments,u"gertwol({0},{1},{2},{3},{4}).".format(get_repr2(word),get_repr2(lemma),get_repr2(pos2),morph2,other)))

print_cache(cache)