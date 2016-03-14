#!/usr/bin/python
# -*- coding: utf-8 -*-
# Author: Rico Sennrich <sennrich@cl.uzh.ch>

# input: tokenized text, one sentence per line, whitespace to indicate token boundary
# output: tokenized text, one token per line, empty lines to indicate sentence boundary

from __future__ import print_function, unicode_literals

import sys
import codecs

# python 2/3 compatibility
if sys.version_info < (3, 0):
  sys.stderr = codecs.getwriter('UTF-8')(sys.stderr)
  sys.stdout = codecs.getwriter('UTF-8')(sys.stdout)
  sys.stdin = codecs.getreader('UTF-8')(sys.stdin)

for line in sys.stdin:
       line = line.replace(' \u200b ', ' \xad ') #zero-width space behaves weirdly in some python versions
       for word in line.split():
            print(word)
       print('')
