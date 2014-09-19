#!/usr/bin/python
# -*- coding: utf-8 -*-
# Author: Rico Sennrich <sennrich@cl.uzh.ch>

# input: tokenized text, one sentence per line, whitespace to indicate token boundary
# output: tokenized text, one token per line, empty lines to indicate sentence boundary

from __future__ import print_function

import sys

for line in sys.stdin:
       line = line.replace(' \xe2\x80\x8b ', ' \xc2\xad ') #zero-width space behaves weirdly in some python versions
       for word in line.split():
            print(word)
       print('')