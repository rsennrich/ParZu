# -*- coding: utf-8 -*-
import sys, os, getopt
from subprocess import Popen, PIPE


inputfile = 'tueba_words0-1'
evalfile = 'tueba_eval0-1.pl'


########################
#config file parser

COMMENT_CHAR = '#'
OPTION_CHAR =  '='


def parse_config(filename):
    options = {}
    f = open(filename)
    for line in f:
        # First, remove comments:
        if COMMENT_CHAR in line:
            # split on comment char, keep only the part before
            line, comment = line.split(COMMENT_CHAR, 1)
        # Second, find lines with an option=value:
        if OPTION_CHAR in line:
            # split on option char:
            option, value = line.split(OPTION_CHAR, 1)
            # strip spaces:
            option = option.strip()
            value = value.strip()
            # store in dictionary:
            options[option] = value
    f.close()
    return options


########################


options = parse_config(os.path.join(sys.path[0], '..', 'config.ini'))

if options['tempdir'] == 'local':
    options['tempdir'] = os.path.join(sys.path[0],'..','tmp')

infile = open(os.path.join(sys.path[0],inputfile), 'r')
parsedfile = open(os.path.join(options['tempdir'],'tueba_parsed0-1.pl'), 'w')

parser = Popen(["python", "parzu", "--input=tokenized", "--output=prolog"],cwd= os.path.join(sys.path[0],".."), stdin=infile,stdout=parsedfile,stderr=sys.stderr)

parser.wait()
parsedfile.close()

parsedfile = os.path.join(options['tempdir'],'tueba_parsed0-1.pl')

evaluation = Popen(['swipl', '-q', '-s', 'eval.pl', '-g', 'style_check(-discontiguous)', '-t', 'start(\''+evalfile+'\',\'' + parsedfile + '\').'], cwd=sys.path[0], stdin=sys.stdin, stdout=sys.stdout, stderr=sys.stdout)

evaluation.wait()

#clean temp directory
if options['deltemp'] == '1':
	os.remove(os.path.join(options['tempdir'],'tueba_parsed0-1.pl'))
