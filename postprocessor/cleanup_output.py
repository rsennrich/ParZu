#!/usr/bin/python
# -*- coding: utf-8 -*-

from __future__ import unicode_literals

import sys
import codecs
import re

re_prologpreds_start = re.compile(r"<PROLOGPREDS \d>")
re_prologpreds_end = re.compile(r"</PROLOGPREDS>")
re_sent = re.compile(r"sent\(")
re_analyses = re.compile(r"analyses\(")

def cleanup_conll(txtin):
    """remove debugging output from ParZu output (CoNLL format)"""

    active = 0
    outlines = []

    for line in txtin:
        line = line.strip()
        if re_prologpreds_start.match(line):
            active = 1
        elif re_prologpreds_end.match(line):
            active = 0
            if outlines:
                yield '\n'.join(outlines) + '\n\n'
            outlines = []
        elif active and line:
            outlines.append(line)

    if outlines:
        yield '\n'.join(outlines) + '\n'

def cleanup_prolog(txtin):
    """remove debugging output from ParZu output (Prolog format), and add sentence numbers"""

    active = 0
    outlines = []
    count = 0
    analyses = 0

    for line in txtin:
        line = line.strip()
        if re_sent.match(line):
            count += 1
        if re_analyses.match(line):
            analyses += 1
        if re_prologpreds_start.match(line):
            active = 1
        elif re_prologpreds_end.match(line):
            active = 0
            if outlines:
                yield '\n'.join(outlines) + '\n\n'
            outlines = []
        elif active and line:
            line = line.lstrip('word(')
            line = 'word(' + str(count) + ',' + line
            outlines.append(line)

    if outlines:
        yield '\n'.join(outlines) + '\n'

if __name__ == '__main__':

    outputformat = sys.argv[1]

    if sys.version_info < (3, 0):
        sys.stdout = codecs.getwriter('UTF-8')(sys.stdout)
        sys.stdin = codecs.getreader('UTF-8')(sys.stdin)

    if outputformat == 'conll':
        func = cleanup_conll
    elif outputformat == 'prolog':
        func = cleanup_prolog

    for sentence in func(sys.stdin):
        sys.stdout.write(sentence)
