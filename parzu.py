#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Copyright © 2009-2011 University of Zürich
# Author: Rico Sennrich <sennrich@cl.uzh.ch>

from __future__ import unicode_literals

import sys 
import os
import getopt
import shlex
import pipes
from subprocess import Popen, PIPE


if sys.version_info < (2, 6):
    enable_multiprocessing = 0
else:
    enable_multiprocessing = 1


#list of output formats created by postprocessing_module.pl.
#if you create a new one, put it here.
outputformats = ["oldconll","conll","moses","prolog"]


#if you want to use yap (version >= 6.2), change this to 'yap'
prolog = 'swipl'
#prolog = 'yap'

#yap and swipl have different command line arguments: this maps to the correct one
prolog_load = {'yap':'-l',
         'swipl':'-s',
         'pl':'-s'}

#smor and 'keep' morphology options are internally treated as Gertwol morphology.
morphology_mapping = {'gertwol':'gertwol',
                        'smor':'gertwol',
                        'none':'off',
                        'keep':'gertwol',
                        'tueba':'tueba'}

# root directory of ParZu if file is run as script
root_directory = sys.path[0]
# root directory of ParZu if file is loaded as a module
if '__file__' in globals():
    root_directory = os.path.dirname(os.path.abspath(__file__))

########################
#Command line argument parser

def usage():
    bold = "\033[1m"
    reset = "\033[0;0m"
    sys.stderr.write(bold + '\nParZu options\n' + reset + '\n')
    sys.stderr.write('\t' + bold + '-h' + reset + ', ' + bold + '--help' + reset + '\n')
    sys.stderr.write('\t\tprint usage information\n\n')
    sys.stderr.write('\t' + bold + '-i' + reset + ', ' + bold + '--input' + reset + ' type\n')
    sys.stderr.write('\t\tdefine input type. Supported so far:\n')
    sys.stderr.write('\t\t' + bold +'plain' + reset + ': plain text\n')
    sys.stderr.write('\t\t' + bold +'preprocessed' + reset + ': parser-ready input (generated with output option "preprocessed")\n')
    sys.stderr.write('\t\t' + bold +'tokenized' + reset + ': tokenized text. one token per line, empty line for sentence boundary\n')
    sys.stderr.write('\t\t' + bold +'tokenized_lines' + reset + ': tokenized text. one sentence per line, tokens separated by whitespace\n')
    sys.stderr.write('\t\t' + bold +'tagged' + reset + ': POS-tagged text. token [tab] tag [newline]\n\n')
    sys.stderr.write('\t' + bold + '-l' + reset + ', ' + bold + '--linewise' + reset + '\n')
    sys.stderr.write('\t\tparse text line by line (suppress automatic sentence splitting).\n\n')
    sys.stderr.write('\t' + bold + '-o' + reset + ', ' + bold + '--output' + reset + ' type\n')
    sys.stderr.write('\t\tdefine output type. Supported so far:\n')
    sys.stderr.write('\t\t' + bold +'conll' + reset + ': CoNLL 2007 data format\n')
    sys.stderr.write('\t\t' + bold + 'graphical' + reset + ': generate one SVG image per sentence in your current directory, using DepSVG.\n')
    sys.stderr.write('\t\t' + bold +'moses' + reset + ': factored format used by Moses SMT system\n')
    sys.stderr.write('\t\t' + bold +'preprocessed' + reset + ': return preprocessed input (unparsed, but parser-ready)\n')
    sys.stderr.write('\t\t' + bold +'prolog' + reset + ': prolog-readable format\n')
    sys.stderr.write('\t\t' + bold +'raw' + reset + ': parser output without postprocessing (mostly for debugging)\n\n')
    sys.stderr.write('\t' + bold + '-p'  + reset + ', ' + bold + '--processes' + reset + ' int\n')
    sys.stderr.write('\t\tnumber of parallel parser processes\n\n')
    sys.stderr.write('\t' + bold + '-q'  + reset + ', ' + bold + '--quiet' + reset + '\n')
    sys.stderr.write('\t\tsend warnings and errors to temporary log file instead of stderr\n')
    sys.stderr.write('\t' + bold + '--projective' + reset + '\n')
    sys.stderr.write('\t\talso print projective output. Uses the last two columns of the CoNLL dependency format (or two extra arguments in the Prolog format).\n')
    sys.stderr.write('\t' + bold + '--secedges' + reset + '\n')
    sys.stderr.write('\t\talso print secondary edges. Uses the last two columns of the CoNLL dependency format (or two extra arguments in the Prolog format).\n\n')

def load_arguments():
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hi:lo:p:q", ["help", "input=", "linewise", "output=", "processes=", "quiet", "secedges", "projective"])
    except getopt.GetoptError as err:
        # print help information and exit:
        sys.stderr.write(err) # will print something like "option -a not recognized"
        usage()
        sys.exit(2)
    output = None
    processes = None
    inputformat = None
    linewise = False
    verbose = True
    extrainfo = 'no'
    for o, a in opts:
        if o in ("-h", "--help"):
            usage()
            sys.exit()
        elif o in ("-p", "--processes"):
            processes = a
        elif o in ("-o", "--output"):
            output = a
        elif o in ("-i", "--input"):
            inputformat = a
        elif o in ("-l", "--linewise"):
            linewise = True
        elif o in ("--secedges",):
            if extrainfo == 'projective':
                sys.stderr.write('Error: Cannot display secondary edges and projective tree at same time. Aborting.\n')
                sys.exit()
            extrainfo = 'secedges'
        elif o in ("--projective",):
            if extrainfo == 'secedges':
                sys.stderr.write('Error: Cannot display secondary edges and projective tree at same time. Aborting.\n')
                sys.exit()
            extrainfo = 'projective'
        elif o in ("-q", "--quiet"):
            verbose = False
        else:
            assert False, "unhandled option"
    return processes,output,inputformat,linewise,verbose,extrainfo


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


def try_Popen(*args, **kwargs):
    try:
        ret = Popen(*args, **kwargs)
        return ret
    except OSError as err:
        sys.stderr.write('OSError: {0}\ncommand was:\n\n'.format(err))
        sys.stderr.write(' '.join(pipes.quote(s) for s in args[0]) + '\n\n')
        sys.exit(1)

#sanity checks and merging config and command line options
def process_arguments():

    #get config options
    options = parse_config(os.path.join(root_directory,'config.ini'))

    #get command line options
    arg_processes,arg_output,arg_input,options['linewise'],verbose,options['extrainfo'] = load_arguments()

    if arg_output:
        options['outputformat'] = arg_output

    if not options['outputformat'] in ["raw","preprocessed","graphical"] + outputformats:
        sys.stderr.write("Error: Output option not recognized: " + options['outputformat'] + "\n")
        usage()
        sys.exit(2)

    if arg_input:
        options['inputformat'] = arg_input

    if not options['inputformat'] in ["plain","tokenized","tokenized_lines","tagged","preprocessed"]:
        sys.stderr.write("Error: Input option not recognized: " + options['inputformat'] + "\n")
        usage()
        sys.exit(2)

    if arg_processes:
        options['processes'] = arg_processes

    try:
        int(options['processes'])
    except ValueError:
        # print help information and exit:
        sys.stderr.write("Error: Number of processes must be int\n")
        usage()
        sys.exit(2)

    #calculate real number of parser processes spawned
    if int(options['processes']) < 1 and enable_multiprocessing:
        options['num_processes'] = multiprocessing.cpu_count() + int(options['processes'])
    else:
        options['num_processes'] = int(options['processes'])

    options['nbestmode'] = int(options['nbestmode'])
    
    if options['nbestmode']:
        options['nbest_cutoff'] = float(options['nbest_cutoff'])
        
    options['sentdelim'] = '$newline'
    options['returnsentdelim'] = 'no'
    
    if options['tempdir'] == 'local':
        options['tempdir'] = os.path.join(root_directory,'tmp')


    if options['uniquetmp'] == '1':
        uniquetmp = str(os.getpid())
    else:
        uniquetmp = ''

    options['tagfilepath'] = os.path.join(options['tempdir'],'tags' + uniquetmp + '.pl')
    options['morphinpath'] = os.path.join(options['tempdir'],'morphin' + uniquetmp)
    options['errorpath'] = os.path.join(options['tempdir'],'error' + uniquetmp)
    options['morphpath'] = os.path.join(options['tempdir'], 'morph' + uniquetmp + '.pl')

    options['taggercmd'] = shlex.split(options['taggercmd'])

    if verbose:
        options['senderror'] = sys.stderr
    else:
        options['senderror'] = open(options['errorpath'],'w')

    if options['morphology'] == 'morphisto':
        sys.stderr.write('Warning: deprecated value \'morphisto\' for option \'morphology\'. Use \'smor\' instead.\n')
        options['morphology'] = 'smor'

    return options
      
      
def main(options, input_stream, output_stream):
    
    #mapping between processing steps and the corresponding function
    steps = {10:tokenize,
             20:tag,
             30:preprocess,
             40:parse,
             50:postprocess,
             60:generate_graphics}
    
    #mapping between input/output options and corresponding processing step
    states = {"plain":0,
              "tokenized_lines":0,
              "tokenized":10,
              "tagged":20,
              "preprocessed":30,
              "raw":40,
              "graphical":60}
              
    for f in outputformats:
        states[f] = 50
    
    first_step = states[options['inputformat']]
    last_step = states[options['outputformat']]

    #create list of all functions to run
    todo = [steps[i] for i in sorted(steps) if i > first_step and i <= last_step]
    
    for i,func in enumerate(todo):
    
        #first function takes stdin as input; all other use the output of the previous step
        if i == 0:
            instream = input_stream
        else:
            instream = last_step.stdout

        #last step writes to stdout; all other write to subprocess.PIPE for use in the next step
        if i == len(todo)-1:
            outstream = output_stream
        else:
            outstream = PIPE

        last_step = func(instream,outstream,options)
        
    last_step.wait()
    cleanup(options)
    
    
#sentence splitting and tokenization
#input: plain text
#output: one token per line; empty lines mark sentence boundaries
def tokenize(instream,outstream,options):
    
    options['senderror'].write("Starting tokenizer\n")
    
    if options['linewise']:
        tokenizer_in = instream
    elif options['inputformat'] == 'tokenized_lines':
      tokenizer = try_Popen(["python", os.path.join(root_directory,"preprocessor","tokenized_lines.py")], stdin=instream, stdout=outstream, stderr=options['senderror'])
      return tokenizer
    else:
        sentences = try_Popen(["python", os.path.join(root_directory,"preprocessor","punkt_tokenizer.py")], stdin=instream, stdout=PIPE, stderr=options['senderror'])
        tokenizer_in = sentences.stdout
    
    tokenizer = try_Popen(["perl", os.path.join(root_directory,"preprocessor","tokenizer.perl"), "-l","de"], stdin=tokenizer_in, stdout=outstream, stderr=options['senderror'])
    
    return tokenizer


#pos tagging
#input: one token per line
#output: token \t tag \n
def tag(instream,outstream,options):
    
    options['senderror'].write("Starting POS-tagger\n")
    
    if options['nbestmode']:
        tagger = try_Popen(options["taggercmd"]+['-n',str(options['nbestmode'])], stdin=instream, stdout=PIPE, stderr=options['senderror'],cwd=root_directory)
        tagger_formatted = try_Popen(['python',os.path.join(root_directory,"preprocessor","prune_nbest.py"),str(options['nbest_cutoff'])], stdin=tagger.stdout, stdout=outstream, stderr=options['senderror'])

    else:
        tagger_formatted = try_Popen(options['taggercmd'], stdin=instream, stdout=outstream, stderr=options['senderror'],cwd=root_directory) 

    return tagger_formatted


#convert to prolog-readable format
#do morphological analysis
#identify verb complexes
def preprocess(instream,outstream,options):

    options['senderror'].write("Starting preprocessor\n")

    tagfile = open(options['tagfilepath'],'w')
    #convert to prolog-readable format
    #additionally, create list of word types to be analysed (written to 'morphinpath')
    treetagger2prolog = try_Popen(["python", os.path.join(root_directory,"preprocessor","treetagger2prolog.py"), options['morphinpath'],options['sentdelim']], stdin=instream, stdout=tagfile, stderr=options['senderror'])

    treetagger2prolog.wait()
    morphfile = open(options['morphpath'],'w')

    #morphological analysis
    if options['morphology'] == 'gertwol' or options['morphology'] == 'smor':

        if options['morphology'] == 'gertwol':
            morphin = open(options['morphinpath'],'r')
            gertwol = try_Popen(["python", "gertwol_parzu.py", options['gertwolcmd'], options['gertwolscorecmd']], cwd=os.path.join(root_directory,'preprocessor','morphology'),stdin=morphin,stdout=morphfile,stderr=options['senderror'])
            gertwol.wait()

        elif options['morphology'] == 'smor':
            try:
                morphin = open(options['morphinpath'],'r')
                morphisto = try_Popen(["fst-infl2", options['smor_model']],stdin=morphin,stdout=PIPE,stderr=options['senderror'])
                morphisto2prolog = try_Popen(["python", "morphisto2prolog.py"], cwd=os.path.join(root_directory,'preprocessor','morphology'),stdin=morphisto.stdout,stdout=morphfile,stderr=options['senderror'])
                morphisto2prolog.wait()
                morphisto.wait()
                if morphisto.returncode > 0:
                    sys.stderr.write('fst-infl2 failed; does model exist? trying fst-infl (in case model is non-compact)\n')
                    raise RuntimeError
            except RuntimeError:
                morphin = open(options['morphinpath'],'r')
                morphisto = try_Popen(["fst-infl", options['smor_model']],stdin=morphin,stdout=PIPE,stderr=options['senderror'])
                morphisto2prolog = try_Popen(["python", "morphisto2prolog.py"], cwd=os.path.join(root_directory,'preprocessor','morphology'),stdin=morphisto.stdout,stdout=morphfile,stderr=options['senderror'])
                morphisto2prolog.wait()
                morphisto.wait()
            except OSError as e:
                if e.errno == 2:
                    sys.stderr.write('Error: fst-infl2 not found. Please install sfst.\n')
                    sys.exit(1)

    #having at least one entry makes sure that the preprocessing script doesn't crash
    morphfile.write('gertwol(\'<unknown>\',\'<unknown>\',_,_,_).')

    morphfile.close()
    tagfile.close()
    tagfile = open(options['tagfilepath'],'r')

    #integrate morph. information and do verb complex analysis
    preprocessing =  try_Popen([prolog, '-q', prolog_load[prolog], os.path.join(root_directory,'preprocessor','preprocessing.pl'), '-g', 'retractall(lemmatisation(_)),retractall(morphology(_)),assert(lemmatisation('+ options['morphology'] +')),assert(morphology('+ options['morphology'] +")),retract(sentdelim(_)),assert(sentdelim('"+ options['sentdelim'] +"')),start('" + options['morphpath'] + "'),halt."], stdout=outstream, stdin=tagfile, stderr=options['senderror'])

    return preprocessing



#convert to prolog-readable format
#identify verb complexes
#same as preprocess, but assuming that you use a static morphology file (you also need to override options['morphpath'])
#main advantage is that we no longer need to wait() for the morphological analysis to finish (and generate no temporary files)
def preprocess2(instream,outstream,options):

    options['senderror'].write("Starting preprocessor\n")

    #convert to prolog-readable format
    treetagger2prolog = try_Popen(["python", os.path.join(root_directory,"preprocessor","treetagger2prolog.py"), '/dev/null',options['sentdelim']], stdin=instream, stdout=PIPE, stderr=options['senderror'])

    #integrate morph. information and do verb complex analysis
    preprocessing =  try_Popen([prolog, '-q', prolog_load[prolog], os.path.join(root_directory,'preprocessor','preprocessing.pl'), '-g', 'retractall(lemmatisation(_)),retractall(morphology(_)),assert(lemmatisation('+ options['morphology'] +')),assert(morphology('+ options['morphology'] +")),retract(sentdelim(_)),assert(sentdelim('"+ options['sentdelim'] +"')),start('" + options['morphpath'] + "'),halt."], stdout=outstream, stdin=treetagger2prolog.stdout, stderr=options['senderror'])

    return preprocessing
    

#main parsing step
def parse(instream,outstream,options):
    
    options['senderror'].write("Starting parser\n")
    
    if options['outputformat'] == 'graphical':
        outputformat = 'conll'
    else:
        outputformat = options['outputformat']
    
    #main parsing step
    args = 'retract(outputformat(_)),assert(outputformat('+ outputformat +")),retract(sentdelim(_)),assert(sentdelim('"+ options['sentdelim'] +"')),retract(returnsentdelim(_)),assert(returnsentdelim("+ options['returnsentdelim'] +')),retract(nbestmode(_)),assert(nbestmode('+ str(options['nbestmode']) +')),retractall(morphology(_)),assert(morphology('+ morphology_mapping[str(options['morphology'])] +')),retractall(lemmatisation(_)),assert(lemmatisation('+ morphology_mapping[str(options['morphology'])] +')),retractall(extrainfo(_)),assert(extrainfo('+ options['extrainfo'] +')),start,halt.'
    
    if enable_multiprocessing and options['num_processes'] > 1:
        parsing = try_Popen(["python", os.path.join(root_directory,"core","multiprocessed_parsing.py"), str(options['num_processes']), args, options['sentdelim'], os.path.join(root_directory,'core'),prolog, prolog_load[prolog]], stdin=instream,stdout=outstream,stderr=options['senderror'])

    else:
        runCMD = [prolog, '-q', prolog_load[prolog], 'ParZu-parser.pl', '-g', args]
        
        if prolog.endswith('swipl'):
            runCMD += ['-G248M', '-L248M']
        
        parsing = try_Popen(runCMD, cwd=os.path.join(root_directory,'core'), stdin=instream, stdout=outstream, stderr=options['senderror'])
        
    return parsing


#de-projectivization and removal of debugging output
def postprocess(instream,outstream,options):
    
    options['senderror'].write("Starting postprocessor\n")
    
    if options['nbestmode']:
        cleanup_out = PIPE
    else:
        cleanup_out = outstream

    if options['outputformat'] == 'graphical':
        outputformat = 'conll'
    else:
        outputformat = options['outputformat']

    #postprocessing
    #add sentence number
    if outputformat == "prolog":
        arg = 'prolog'
    else:
        arg = 'conll'

    clean_parse = try_Popen(["python", os.path.join(root_directory,"postprocessor","cleanup_output.py"), arg], stdin=instream, stdout=cleanup_out, stderr=options['senderror'])

    #select one analysis from n-best
    if options['nbestmode']:
        return_best = try_Popen(["python", os.path.join(root_directory,'postprocessor','select_from_nbest.py'), outputformat], stdin=clean_parse.stdout,stdout=outstream,stderr=options['senderror'])

        return return_best

    return clean_parse


#use DepSVG for graphical output
def generate_graphics(instream,outstream,options):
    
    options['senderror'].write("Starting to generate graphics in your current directory (1.svg, ..., n.svg)\n")
    
    conll2svg = try_Popen(["perl", os.path.join(root_directory,'postprocessor','DepSVG','conll_to_svg.perl'),'--dir='+os.getcwd()], cwd=os.path.join(root_directory,'postprocessor','DepSVG'), stdin=instream,stdout=outstream,stderr=options['senderror'])

    return conll2svg


#clean temp directory
def cleanup(options):
    if options['deltemp'] == '1':
        for fname in [options['tagfilepath'],options['morphinpath'],options['errorpath'],options['morphpath']]:
            if os.path.isfile(fname):
                os.remove(fname)


if __name__ == '__main__':

    options = process_arguments()
    main(options, sys.stdin, sys.stdout)
