ParZu - The Zurich Dependency Parser for German
===============================================

A project of the Computational Linguistics Group at the University of Zurich (http://www.cl.uzh.ch).

Project Homepage: http://github.com/rsennrich/parzu

online demo: https://pub.cl.uzh.ch/demo/parzu/

ABOUT
-----

ParZu is a dependency parser for German.
This means that it analyzes the linguistic structure of sentences and, among other things, identifies the subject and object(s) of a verb.

It is a fork of Gerold Schneider's English Pro3Gres parser. 
Its architecture is hybrid and consists of both a hand-written grammar and a statistics module that returns the most likely analysis of a sentence.
The primary difference to the English parser is the German grammar and statistics module.
Architecturally, it is different in that it supports the use of morphological information, and does not use a chunker.
ParZu also has a python wrapper that supports various input/output formats and multiprocessing.

REQUIREMENTS (DOCKER)
---------------------

ParZu can be installed via Docker on any platform that supports Docker, including many Linux variants, Mac (OS X 10.11 or newer), and Windows 10.

REQUIREMENTS (LOCAL INSTALLATION)
---------------------------------

This software was developed and tested using

    Linux (32 and 64 bit)
    SWI-Prolog 5.6 (or YAP 6.2 - check the top of the script `parzu` to switch between the two)
    Python 3.7
    Perl 5.10

Additionally, the parser requires the following software and licenses:

- A POS-Tagger using the STTS tagset: (for instance clevertagger: https://github.com/rsennrich/clevertagger)
- A tool for morphological analysis (not required, but strongly recommended). Supported:
   - Zmorge: https://pub.cl.uzh.ch/users/sennrich/zmorge/ (open source; recommended)
   - Morphisto: http://code.google.com/p/morphisto/ (open source; non-commercial)
   - Gertwol: http://www2.lingsoft.fi/cgi-bin/gertwol (proprietary; might be deprecated in the future)

For development and testing, we recommend Tüba-D/Z or the Hamburg Dependency Treebank HDT (free download for academic use).
See the section below on how to extract and integrate statistical information from Tüba-D/Z or the HDT into the system.

LOCAL INSTALLATION
------------------

1. Install all requirements. In Ubuntu Linux, all are available in the repositories:
    sudo apt-get install swi-prolog sfst

2. unpack (or git clone) the directory to your target directory.

3. adjust file paths in the config.ini file. Also define which lemmatisation/morphology you use (if any), and make sure they work. Default models can be installed/configured by executing `install.sh`

4. (OPTIONAL): If you have the Tüba-D/Z corpus in the right format (dependencies, CONLL format, UTF-8), you can generate improved statistic files by executing `statistics/create_statistics.sh`. 
    The same script will also work with the Hamburg Dependency Treebank in the CONLL format.
    The files advstats* freq* konjstats* ppstats* and vstats* are created in a temporary directory - move them into the `statistics/` folder to make the system use them.
    With evaluation/create_devsets.py , you can create a development from a CONLL format file, and perform quality/regression tests with python evaluation/do_evaluation.py.

5. (OPTIONAL): to speed up the parser initialization (with SWI-Prolog), run `statistics/compile.sh`. Repeat this step if you modify the statistics files.

INSTALLATION (DOCKER)
---------------------

Alternatively to the local installation, ParZu can also be installed through Docker:

    docker pull rsennrich/parzu

You can then execute ParZu via `docker run`:

    docker run -p 5003:5003 rsennrich/parzu

This will launch a web server on http://localhost:5003/ (open this in a web browser, or see below how to interact with it)

You can also start the Docker container so that it reads from stdin and writes to stdout:

    echo "Ich bin ein Berliner." | docker run -i rsennrich/parzu /ParZu/parzu


EXAMPLE COMMANDS AND USAGE INFORMATION
--------------------------------------

you can use ParZu as a stand-alone script:

    echo "Das ist ein Test." | ./parzu
runs preprocessing, the main parsing step and postprocessing.

    ./parzu -i tagged < sample_input
parse pre-tagged text file

    ./parzu -h
show available command line parameters

alternatively, you can call ParZu from a Python application:

    import parzu_class as parzu
    options = parzu.process_arguments()
    ParZu = parzu.Parser(options)
    sentences = ParZu.main('Das ist ein Test. Das auch.')
    for sentence in sentences:
        print sentence


lastly, you can run ParZu as a server with a simple web API:

    ./parzu_server.py

by default, this will launch a server which serves http://localhost:5003. Check this location in a browser, or check [here](doc/API.md) to see the API.

here is a sample command using curl:

    curl -H "Content-Type: application/json" -X POST -d '{"text": "Ich bin ein Berliner."}' "http://localhost:5003/parse/"

The dependency labels implemented in ParZu are described in:

Killian A. Foth. 2005. Eine umfassende Contraint-Dependenz-Grammatik des Deutschen. University of Hamburg.

A short overview is given [here](doc/LABELS.md).

ON QUALITY AND TESTING
----------------------

The default statistics are trained on automatic parses of Europarl.
This means they are slightly worse than statistics extracted from a hand-created treebank, but license-free.
See step 4 in the installation instractions on how to extract statistics from other treebanks.

If you have a Tüba-D/Z license, you can use the script `evaluation/create_devsets.py` to generate a development set and do your own performance tests.
Given that both an input and a gold file exist, start the evaluation as follows:
    python evaluation/do_evaluation.py

This will parse the first 1000 sentences* of TüBa-D/Z and report the results.
*Sentences 1-1000 were used during development, sentences 1001-4000 for the evaluation in (Sennrich et al. 2009), and sentences 4001-end for training).
You can use the online demo at https://pub.cl.uzh.ch/demo/parzu/ to check if there are any major regressions in your local install.

TECHNICAL FAQ
-------------

- What is the architecture of the parser?

  The core of ParZu is the syntactic parser itself, which relies on tokenized and POS-tagged input, and morphological annotation.
  To allow parsing of raw text, ParZu includes a pipeline of text processing steps.
  A short overview is given [here](doc/PIPELINE.md).

- What do the fields in the output mean?

  By default, ParZu uses the [CoNLL dependency format](doc/CONLL.md).
  The parse information is in columns 7 and 8 (ID of head and dependency relation);
  columns 9 and 10 either provide information about secondary edges (option --secedges) or projective heads (option --projective).
  The relation labels are described [here](doc/LABELS.md), the STTS tagset for the POS tags at http://www.ims.uni-stuttgart.de/forschung/ressourcen/lexika/TagSets/stts-table.html .

- Why doesn't the parser immediately start generating output? (especially when parsing large texts)

  For technical reasons, the parser finishes the morphological analysis of the entire texts before starting the actual parsing.
  To avoid this (i.e. if you want to do interactive parsing), re-use an old morphology file (they are generated in the temporary directory),
  and hard-code it in into parzu. You can do this by replacing the value of steps[30] with preprocess2 (original value: preprocess),
  and overriding options['morphpath'] in process_arguments(). 
  IMPORTANT: also set "deltemp" in your config.ini to 0, otherwise the file at options['morphpath'] will be deleted after parsing.

  Compiling the statistical files also helps to speed up parser initialization. See the installation instructions.

- Why do things go wrong / Can you fix this error?

  There are a number of possible reasons for wrong analyses, and often, there's no easy fix:

    - Tagging errors (the grammar is unlexicalised and based on PoS tags; errors in tagging may propagate)
    - Errors in morphological analysis (the grammar enforces agreement constraints; morphological misanalysis may prevent the correct parse)
    - Gaps/errors in the grammar (sometimes possible to fix, but allowing a rare construction might generate false positives in other places)
    - Statistics (the correct analysis is not the most probable one; this is often a sign of actual ambiguities that we could only solve with better statistical information)
    - Pruning (even if the correct analysis were the best globally, a subtree might have been discarded early on because it was locally less likely than others)

  Finding the error source can take quite a bit of time (some errors can be seen from the output (e.g. tagging errors); errors with the grammar/statistics are best investigated at runtime).
  We welcome any improvements to the grammar, but please use a development set to ensure that fixing the analysis of a particular sentence doesn't generate new errors for others.

- What if I want to use a PoS-tagger that uses a different input/output format?

  So long as your PoS-tagger uses the STTS tagset, you can use whatever sentence splitting/tokenization/tagging you want, and then call the parser with `--input tagged`.
  Make sure to convert your tagged text to the following format, with empty lines signifying sentence boundaries:
    word [tab] tag [newline]
    
- What character encoding does the parser use?

  ParZu uses UTF-8 encoding.

KNOWN ISSUES
------------

Internally, ParZu uses the following format as input/output for tagging: one token per line, blank lines for sentence boundaries.
Most POS taggers (e.g. TnT, Stanford POS tagger, hunpos, clevertagger) support this format; the TreeTagger, however, uses SGML tags instead.
To use the TreeTagger for POS tagging, use `preprocessing/treetagger-wrapper.py`, which converts between the different formats.
If you want to use the TreeTagger, set the paths to the TreeTagger binary and the German UTF-8 model in `preprocessing/treetagger-wrapper.py`.


LICENSE
-------

ParZu is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License (see LICENSE).

The files in `postprocessing/DepSVG` are from Kaarel Kaljurand's DepSVG library and are licensed under the LGPL (https://github.com/Kaljurand/DepSVG)

`preprocessing/tokenizer.perl` and `preprocessing/nonbreaking_prefix.de` are from the Moses toolkit and licensed under the LGPL (http://www.statmt.org/moses/)

`preprocessing/punkt_tokenizer.py` is from the NLTK and licensed under the Apache License 2.0 (https://github.com/nltk/nltk)

PUBLICATIONS
------------

The parser is described in:

Rico Sennrich, Gerold Schneider, Martin Volk and Martin Warin (2009): 
   A New Hybrid Dependency Parser for German. In: Proceedings of GSCL Conference, Potsdam.

Rico Sennrich, Martin Volk and Gerold Schneider (2013):
   Exploiting Synergies Between Open Resources for German Dependency Parsing, POS-tagging, and Morphological Analysis.
   In: Proceedings of the International Conference Recent Advances in Natural Language Processing 2013, Hissar, Bulgaria.

ACKNOWLEDGMENTS
---------------

This project has received funding from GRADUAL.CONSULTING.

CONTACT
-------

For questions and feeback, please contact sennrich@cl.uzh.ch or use the GitHub repository.
