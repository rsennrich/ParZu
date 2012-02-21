#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright © 2011 University of Zürich
# Author: Rico Sennrich <sennrich@cl.uzh.ch>

# Simple wrapper that deals with the TreeTagger's way of denoting sentence boundaries.
# Requires TreeTagger to be installed and TREETAGGER_BIN and TREETAGGER_MODEL to be set properly.

TREETAGGER_BIN = ""
TREETAGGER_MODEL = ""

#TREETAGGER_BIN = "/opt/tagger/treetagger/treetagger-3.2/bin/tree-tagger"
#TREETAGGER_MODEL = "/opt/tagger/treetagger/treetagger-3.2/lib/german-utf8.par"

import sys
from subprocess import Popen, PIPE

if not TREETAGGER_BIN or not TREETAGGER_MODEL:
    sys.stderr.write('ERROR: set paths TREETAGGER_BIN and TREETAGGER_MODEL in ' + sys.argv[0] + '\n')
    exit()

convert_to_sgml = Popen(['sed', r's/^$/<\/s>/'],stdin=sys.stdin,stdout=PIPE)
tag = Popen([TREETAGGER_BIN, '-token', '-sgml', '-eos-tag', '</s>', TREETAGGER_MODEL],stdin=convert_to_sgml.stdout,stdout=PIPE)
convert_to_blank_line = Popen(['sed', r's/^<\/s>$//'],stdin=tag.stdout)

convert_to_blank_line.wait()