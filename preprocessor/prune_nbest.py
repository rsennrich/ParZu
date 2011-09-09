#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright © 2011 University of Zürich
# Author: Rico Sennrich <sennrich@cl.uzh.ch>

# Take n-best-list from tagger and discard all analyses that are below a cutoff point (which is relative to the best analysis)

from __future__ import division
import sys
from math import log
from collections import defaultdict

cutoff = float(sys.argv[1])

best = 1
newsent = 1
on = 1
for line in sys.stdin:

    if line == '\n':
        newsent = 1

    if newsent and line.startswith('#0'):
        best = float(line.split()[-1])
        if not best: #if all analyses have a probability of 0, discard all but the first one
            best = 50
        newsent = 0
        on = 1
        
    elif newsent and line.startswith('#'):
        current = float(line.split()[-1])
        newsent = 0
        
        if current/best > cutoff:
            on = 1
        else:
            on = 0
            
    if on:
        print(line),