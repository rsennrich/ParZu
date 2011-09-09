#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
import os

try:
    conll = open(sys.argv[1],'r')
except IndexError:
    sys.stderr.write('Error: missing argument. Example usage:\n')
    sys.stderr.write('./create_devsets tueba6_goldpos.utf8.conll\n')
    exit()

i = 1

refl_mapping = {'uns':'wir', 'sich':'er','mich':'ich','dich':'du','mir':'ich','dir':'du','euch':'ihr'}

evalset = open(os.path.join(sys.path[0],'tueba_eval0-1.pl'),'w')
evalinput = open(os.path.join(sys.path[0],'tueba_words0-1'),'w')

for line in conll:
    if line == '\n':
        i += 1
        evalinput.write('\n')
        evalset.write('\n')
        continue
        
    pos,word,lemma,ctag,tag,morph,head,label = line.strip().split()[:8]
    
    
    #escape prolog special characters
    token = word.replace("\\","\\\\")
    token = token.replace("'","\\\'")
    
    label = label.lower()
    
    if i <= 1000:
        evalset.write("w({0},{1},'{2}','{3}','{4}',{5}).\n".format(i,pos,token,tag,label,head))
        evalinput.write(word +'\n')
        
    else:
        evalset.close()
        evalinput.close()
        break