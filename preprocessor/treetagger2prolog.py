#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright © 2011 University of Zürich
# Author: Rico Sennrich <sennrich@cl.uzh.ch>

from __future__ import print_function, unicode_literals
import sys
import codecs

def spelling_variations(word):
    """Deal with spelling variations that morphology system may not know"""

    variants = set([word])

    for old, new in [('Ae','Ä'), ('Oe','Ö'), ('Ue','Ü'), ('ae','ä',), ('oe','ö'), ('ue','ü'), ('ss','ß')]:
        for variant in list(variants):
            if old in variant:
                segments = variant.split(old)
                for i in range(len(segments)-1):
                    variants.add(segments[i] + new + segments[i+1])

    for variant in variants:
        yield variant


def prolog_escape(word):
    """escape Prolog meta characters"""

    return word.replace("\\","\\\\").replace("'","\\'")


def format_conversion(line):
    """format conversion into Prolog format"""

    try:
        word, pos = line.split()
        newline = "w('{0}', '{1}', ['{0}_{1}'], '{0}').".format(prolog_escape(word),prolog_escape(pos))
        return word, newline
    except:
        if line == '\n':
            return '', "w('ENDOFSENTENCE','{0}',['._{0}'],'ENDOFSENTENCE').".format(sentdelim)
        else:
            sys.stderr.write('Error: Line does not have word and POS tag: {0}\n'.format(line))
            raise


if __name__ == '__main__':

    if len(sys.argv) != 3:
        sys.stderr.write('Usage: ' + sys.argv[0] + ' temporary_file_for_morphology sentence_delimiter\n')
        sys.exit(1)

    morphology_input_path = sys.argv[1]
    sentdelim = sys.argv[2]

    if sys.version_info < (3, 0):
        sys.stdout = codecs.getwriter('UTF-8')(sys.stdout)
        sys.stdin = codecs.getreader('UTF-8')(sys.stdin)

    #used for morphology tool
    to_analyze = set()
    morphology_tempfile = codecs.open(morphology_input_path,'w', 'UTF-8')

    for line in sys.stdin:
        word, line = format_conversion(line)
        print(line)

        #expand word forms for query (to also include spelling variants)
        for variant in spelling_variations(word):
            to_analyze.add(variant)


    print("w('ENDOFDOC','{0}',['._{0}'],'ENDOFDOC').".format(sentdelim))

    for item in to_analyze:
        morphology_tempfile.write(item + '\n')

    morphology_tempfile.close()