#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Copyright © 2009-2011 University of Zürich
# Author: Rico Sennrich <sennrich@cl.uzh.ch>

from __future__ import unicode_literals

import sys
import os
import getopt
import shlex
import pipes
import pexpect
import tempfile
import threading
import codecs
import re
from subprocess import Popen, PIPE

# root directory of ParZu if file is run as script
root_directory = sys.path[0]
# root directory of ParZu if file is loaded as a module
if '__file__' in globals():
    root_directory = os.path.dirname(os.path.abspath(__file__))

sys.path.append(os.path.join(root_directory, 'preprocessor'))
import punkt_tokenizer
import treetagger2prolog
sys.path.append(os.path.join(root_directory, 'preprocessor', 'morphology'))
import morphisto2prolog
sys.path.append(os.path.join(root_directory, 'postprocessor'))
import cleanup_output

from parzu import outputformats, morphology_mapping

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
    sys.stderr.write('\t' + bold + '-v'  + reset + ', ' + bold + '--verbose' + reset + '\n')
    sys.stderr.write('\t\tsend warnings and errors to stderr\n')
    sys.stderr.write('\t' + bold + '--projective' + reset + '\n')
    sys.stderr.write('\t\talso print projective output. Uses the last two columns of the CoNLL dependency format (or two extra arguments in the Prolog format).\n')
    sys.stderr.write('\t' + bold + '--secedges' + reset + '\n')
    sys.stderr.write('\t\talso print secondary edges. Uses the last two columns of the CoNLL dependency format (or two extra arguments in the Prolog format).\n\n')

def load_arguments():
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hi:lo:p:vq", ["help", "input=", "linewise", "output=", "verbose", "secedges", "projective", "quiet"])
    except getopt.GetoptError as err:
        # print help information and exit:
        sys.stderr.write(err) # will print something like "option -a not recognized"
        usage()
        sys.exit(2)
    output = None
    inputformat = None
    linewise = False
    verbose = False
    extrainfo = 'no'
    for o, a in opts:
        if o in ("-h", "--help"):
            usage()
            sys.exit()
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
        elif o in ("-v", "--verbose"):
            verbose = True
        else:
            assert False, "unhandled option"
    return output,inputformat,linewise,verbose,extrainfo


#config file parser

COMMENT_CHAR = '#'
OPTION_CHAR =  '='

EMPTY_SVG = """<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox='0 0 16 132' width='16' height='132'>

<title/>
<defs><marker id="a" viewbox="0 0 8 4" refX="8" refY="2" markerUnits="strokeWidth"
markerWidth="8" markerHeight="4" orient="auto">
<path d="M 0 0 L 8 2 L 0 4 Z" fill='#800'/>
</marker>
</defs>
</svg>"""

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


#sanity checks and merging config and command line options
def process_arguments(commandline=True):

    #get config options
    options = parse_config(os.path.join(root_directory,'config.ini'))

    #get command line options
    if commandline:
        arg_output,arg_input,options['linewise'],verbose,options['extrainfo'] = load_arguments()
    else:
        arg_output = False
        arg_input = False
        verbose = False
        options['linewise'] = False
        options['extrainfo'] = 'no'

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

    options['nbestmode'] = int(options['nbestmode'])

    if options['nbestmode']:
        sys.stderr.write('Warning: nbest tagging mode is disabled in ParZu class.\n')

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

    options['verbose'] = verbose
    options['senderror'] = sys.stderr

    if options['morphology'] == 'morphisto':
        sys.stderr.write('Warning: deprecated value \'morphisto\' for option \'morphology\'. Use \'smor\' instead.\n')
        options['morphology'] = 'smor'

    return options

class Parser():

    def __init__(self, options, timeout=10):

        # launch punkt_tokenizer for sentence splitting
        self.punkt_tokenizer = punkt_tokenizer.PunktSentenceTokenizer()
        self.punkt_tokenizer._params.collocations = punkt_tokenizer.punkt_data_german.collocations
        self.punkt_tokenizer._params.ortho_context = punkt_tokenizer.punkt_data_german.ortho_context
        self.punkt_tokenizer._params.abbrev_types = punkt_tokenizer.punkt_data_german.abbrev_types
        self.punkt_tokenizer._params.sent_starters = punkt_tokenizer.punkt_data_german.sent_starters

        # launch moses tokenizer
        tokenizer_cmd = "perl " + os.path.join(root_directory,"preprocessor","tokenizer.perl") + " -l de"
        self.tokenizer = pexpect.spawn(tokenizer_cmd, echo=False, encoding='utf-8')
        self.tokenizer.expect("Tokenizer v3\r\nLanguage: de\r\n")
        self.tokenizer.delaybeforesend = 0

        # launch clevertagger for POS tagging
        clevertagger_dir = os.path.dirname(options["taggercmd"][0])
        sys.path.append(clevertagger_dir)
        import clevertagger
        self.tagger = clevertagger.Clevertagger()

        # launch SMOR morphological analyzer
        try:
            self.morph = pexpect.spawn("fst-infl2", ['-q', options['smor_model']], echo=False, encoding='utf-8')
            self.morph.delaybeforesend = 0
        except:
            sys.stderr.write('fst-infl2 failed; does model exist? trying fst-infl (in case model is non-compact)\n')
            try:
                self.morph = pexpect.spawn("fst-infl", ['-q', options['smor_model']], echo=False, encoding='utf-8')
                self.morph.delaybeforesend = 0
            except:
                sys.stderr.write('Error: fst-infl2 not found. Please install sfst and smor_model in config.ini points to a valid model.\n')
                sys.exit(1)

        # test morphological analyzer
        self.morph.send('\n')
        out1 = self.morph.readline().strip()
        out2 = self.morph.readline().strip()
        if not (out1 == '>' and out2 == 'no result for'):
            sys.stderr.write('Error: fst-infl2 returned unexpected output:\n')
            sys.stderr.write(out1)
            sys.stderr.write(out2 + '\n')
            sys.exit(1)

        # get swi-prolog version
        swipl_version, _ = Popen(['swipl', '--version'], stdout=PIPE, text=True).communicate()
        swipl_version = swipl_version.split()
        try:
            swipl_version = list(map(int,swipl_version[swipl_version.index('version')+1].split('.')))
            if swipl_version[0] >= 8 or (swipl_version[0] == 7 and swipl_version[1] >= 7):
                prolog_newstacks = True
            else:
                prolog_newstacks = False
        except IndexError:
            prolog_newstacks = False

        # launch morphological preprocessing (prolog script)
        self.prolog_preprocess = pexpect.spawn('swipl',
                                               ['-q', '-s', os.path.join(root_directory,'preprocessor','preprocessing.pl')],
                                               echo=False,
                                               encoding='utf-8',
                                               timeout=timeout)

        self.prolog_preprocess.expect_exact('?- ')
        self.prolog_preprocess.delaybeforesend = 0

        # launch main parser process (prolog script)
        args = ['-q', '-s', 'ParZu-parser.pl']
        if prolog_newstacks:
            args += ['--stack-limit=496M']
        else:
            args += ['-G248M', '-L248M']
        self.prolog_parser = pexpect.spawn('swipl',
                                           args,
                                           echo=False,
                                           encoding='utf-8',
                                           cwd=os.path.join(root_directory,'core'),
                                           timeout=timeout)

        self.prolog_parser.expect_exact('?- ')
        self.prolog_parser.delaybeforesend = 0

        # initialize parser parameters
        parser_init = "retract(sentdelim(_))," \
                + "assert(sentdelim('" + options['sentdelim'] +"'))," \
                + "retract(returnsentdelim(_))," \
                + "assert(returnsentdelim("+ options['returnsentdelim'] +"))," \
                + "retract(nbestmode(_))," \
                + "assert(nbestmode("+ str(options['nbestmode']) + "))," \
                + 'retractall(morphology(_)),' \
                + "assert(morphology("+ morphology_mapping[str(options['morphology'])] + "))," \
                + "retractall(lemmatisation(_))," \
                + "assert(lemmatisation("+ morphology_mapping[str(options['morphology'])] +"))," \
                + "retractall(extrainfo(_))," \
                + "assert(extrainfo("+ options['extrainfo'] + "))," \
                + "start_german."

        self.prolog_parser.sendline(parser_init)
        self.prolog_parser.expect('.*true.*')

        # launch graphical conversion script
        self.conll_to_svg = pexpect.spawn('perl',
                                          ['-I', os.path.join(root_directory,'postprocessor','DepSVG'), os.path.join(root_directory,'postprocessor','DepSVG','conll_to_svg.perl'), '--stdout'],
                                          cwd=os.path.join(root_directory,'postprocessor','DepSVG'),
                                          encoding='utf-8',
                                          echo=False)
        self.conll_to_svg.delaybeforesend = 0

        self.lock_tokenize = threading.Lock()
        self.lock_tag = threading.Lock()
        self.lock_preprocess = threading.Lock()
        self.lock_parse = threading.Lock()
        self.lock_svg = threading.Lock()

        self.options = options

    def __del__(self):
        self.tokenizer.close()
        self.morph.close()
        self.prolog_preprocess.close()
        self.prolog_parser.close()
        self.conll_to_svg.close()

    def main(self, text, inputformat=None, outputformat=None):

        if inputformat is None:
            inputformat = self.options['inputformat']

        if outputformat is None:
            outputformat = self.options['outputformat']


        if inputformat in ['plain', 'tokenized_lines']:
            text = text.strip()
            with self.lock_tokenize:
                sentences = self.tokenize(text, inputformat)
        else:
            sentences = text.split('\n\n')

        # strip empty sentences
        # TODO: we may want to retain empty sentences for alignment purposes in parallel data
        # this is currently not supported in parzu_class; use the batch processing mode of ParZu for this
        sentences = [s.strip() for s in sentences if s.strip()]

        if not sentences:
            if outputformat == 'graphical':
                return [EMPTY_SVG]
            else:
                return []

        if outputformat == 'tokenized':
            return sentences

        if inputformat in ['plain', 'tokenized_lines', 'tokenized']:
            with self.lock_tag:
                sentences = self.tag(sentences)
        else:
            sentences = text.split('\n\n')
        if outputformat == 'tagged':
            return sentences

        if inputformat in ['plain', 'tokenized_lines', 'tokenized', 'tagged']:
            with self.lock_preprocess:
                preprocessed_path = self.preprocess(sentences)
        else:
            preprocessed_path = tempfile.NamedTemporaryFile(prefix='ParZu-preprocessed.pl', dir=os.path.join(self.options['tempdir']), delete=False)
            preprocessed_path.close()
            codecs.open(preprocessed_path.name, 'w', encoding='UTF-8').write(text)

        if outputformat == 'preprocessed':
            text = codecs.open(preprocessed_path, encoding='UTF-8').read()
            os.remove(preprocessed_path)
            return text

        with self.lock_parse:
            parsed_path = self.parse(preprocessed_path, outputformat)
        os.remove(preprocessed_path)

        if outputformat == 'raw':
            text = codecs.open(parsed_path, encoding='UTF-8').read()
            os.remove(parsed_path)
            return text

        sentences = self.postprocess(parsed_path, outputformat)
        os.remove(parsed_path)

        if outputformat in outputformats:
            return sentences

        with self.lock_svg:
            sentences = self.generate_graphics(sentences)

        return sentences


    #sentence splitting and tokenization
    #input: plain text
    #output: one token per line; empty lines mark sentence boundaries
    def tokenize(self, text, inputformat):

        if self.options['verbose']:
            self.options['senderror'].write("Starting tokenizer\n")

        if not text:
            return []

        if inputformat == 'tokenized_lines':
            return ['\n'.join(line.split()) for line in text.splitlines()]

        elif self.options['linewise']:
            sentences = text.splitlines()
        else:
            sentences = self.punkt_tokenizer.tokenize(text)
            # remove line breaks
            sentences = [sentence.replace('\n', ' ').replace('\r', '') for sentence in sentences]

        sentences = process_by_sentence(self.tokenizer, sentences)

        return sentences


    #pos tagging
    #input: list of sentences; each sentence is one token per line
    #output: list of sentences; each sentence is one token per line (token \t tag \n)
    def tag(self, sentences):

        if self.options['verbose']:
            self.options['senderror'].write("Starting POS-tagger\n")

        sentences = self.tagger.tag(sentences)

        return sentences


    #convert to prolog-readable format
    #do morphological analysis
    #identify verb complexes
    #input: list of sentences
    #output: path to file in which preprocessed text is written
    def preprocess(self, sentences):

        if self.options['verbose']:
            self.options['senderror'].write("Starting preprocessor\n")

        # convert to prolog format and get vocabulary
        sentences_out = []
        vocab = set()
        for sentence in sentences:
            sentence_out = []
            for line in sentence.splitlines():
                word, line = treetagger2prolog.format_conversion(line)
                sentence_out.append(line)

                #expand word forms for query (to also include spelling variants)
                for variant in treetagger2prolog.spelling_variations(word):
                    vocab.add(variant)

            sentence_out.append("w('ENDOFSENTENCE','{0}',['._{0}'],'ENDOFSENTENCE').".format(self.options['sentdelim']))

            sentences_out.append('\n'.join(sentence_out))

        sentences_out.append("\nw('ENDOFDOC','{0}',['._{0}'],'ENDOFDOC').".format(self.options['sentdelim']))


        analyses = []
        # split vocab into batches to avoid filling buffer
        batch_size = 100
        vocab = list(vocab)
        for i in range(0, len(vocab), batch_size):
            subvocab = '\n'.join(vocab[i:i+batch_size]) + '\n\n'

            # do morphological analysis
            self.morph.send(subvocab)

            while True:
                ret = self.morph.readline().strip()
                if ret == 'no result for':
                    break
                else:
                    analyses.append(ret)

        # convert morphological analysis to prolog format
        analyses = morphisto2prolog.main(analyses)

        #having at least one entry makes sure that the preprocessing script doesn't crash
        analyses.append('gertwol(\'<unknown>\',\'<unknown>\',_,_,_).')

        # communication with swipl scripts is via temporary files
        morphfile = tempfile.NamedTemporaryFile(prefix='ParZu-morph.pl', dir=os.path.join(self.options['tempdir']), delete=False)
        morphfile.close()
        codecs.open(morphfile.name, 'w', encoding='UTF-8').write('\n'.join(analyses))

        tagfile = tempfile.NamedTemporaryFile(prefix='ParZu-tag.pl', dir=os.path.join(self.options['tempdir']), delete=False)
        tagfile.close()
        codecs.open(tagfile.name, 'w', encoding='UTF-8').write('\n'.join(sentences_out))

        preprocessedfile = tempfile.NamedTemporaryFile(prefix='ParZu-preprocessed.pl', dir=os.path.join(self.options['tempdir']), delete=False)
        preprocessedfile.close()

        # start preprocessing script and wait for it to finish
        self.prolog_preprocess.sendline('retractall(gertwol(_,_,_,_,_)),retractall(lemmatisation(_)),retractall(morphology(_)),assert(lemmatisation(smor)),assert(morphology(smor)),retract(sentdelim(_)),assert(sentdelim(\''+ self.options['sentdelim'] +"')),start('" + morphfile.name + "','" + tagfile.name + "','" + preprocessedfile.name + "').")

        while True:
            line = self.prolog_preprocess.readline()
            if self.options['verbose']:
                self.options['senderror'].write(line)
            line = re.sub(r'\x1B\[[0-?]*[ -/]*[@-~]', '', line)
            if re.match('(\?-\s)?true\.\r\n', line):
                break

        # clean up temporary files
        os.remove(morphfile.name)
        os.remove(tagfile.name)

        return preprocessedfile.name


    #main parsing step
    #input: path to input file (produced by preprocess())
    #output: path to output file (temporary file created by function)
    def parse(self, inpath, outputformat):

        if self.options['verbose']:
            self.options['senderror'].write("Starting parser\n")

        parsedfile = tempfile.NamedTemporaryFile(prefix='ParZu-parsed.pl', dir=os.path.join(self.options['tempdir']), delete=False)
        parsedfile.close()

        if outputformat == 'graphical':
            outputformat = 'conll'

        cmd = "retract(outputformat(_))," \
            + "assert(outputformat("+ outputformat +"))," \
            + "go_textual(\'" + inpath + "','" + parsedfile.name + "').\n"

        self.prolog_parser.sendline(cmd)

        while True:
            line = self.prolog_parser.readline()
            if self.options['verbose']:
                self.options['senderror'].write(line)
            line = re.sub(r'\x1B\[[0-?]*[ -/]*[@-~]', '', line)     # remove styling tokens
            if re.match('(\?-)?(\s+\|\s+)?true\.\r\n', line):
                break

        return parsedfile.name


    #de-projectivization and removal of debugging output
    #input: path to input file (produced by parse)
    #output:
    def postprocess(self, inpath, outputformat):

        if self.options['verbose']:
            self.options['senderror'].write("Starting postprocessor\n")

        infile = codecs.open(inpath, encoding='UTF-8')

        if outputformat == 'prolog':
            sentences = list(cleanup_output.cleanup_prolog(infile))
        else:
            sentences = list(cleanup_output.cleanup_conll(infile))

        return sentences


    #use DepSVG for graphical output
    def generate_graphics(self, sentences):

        if self.options['verbose']:
            self.options['senderror'].write("Generating SVG output\n")

        out_sentences = []
        svg = []

        for sentence in sentences:
            self.conll_to_svg.send(sentence)
            while True:
                line = self.conll_to_svg.readline()
                svg.append(line.strip())
                if line.strip() == '</svg>':
                    out_sentences.append('\n'.join(svg))
                    svg = []
                    break
                elif not line:
                    break

        return out_sentences

def process_by_sentence(processor, sentences):
    sentences_out = []
    for sentence in sentences:
        words = []
        processor.send(sentence + '\n')
        while True:
            word = processor.readline().strip()
            if word:
                words.append(word)
            else:
                break
        sentences_out.append('\n'.join(words))

    return sentences_out
