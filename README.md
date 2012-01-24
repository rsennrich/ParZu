ParZu - The Zurich Dependency Parser for German (formerly known as Pro3GresDE)
=============================================================================

A project of the Computational Linguistics Group at the University of Zurich (http://www.cl.uzh.ch).

Project Homepage: http://github.com/rsennrich/parzu

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation

ABOUT
-----

ParZu is a dependency parser for German.
This means that it analyzes the linguistic structure of sentences and, among other things, identifies the subject and object(s) of a verb.

It is a fork of Gerold Schneider's English Pro3Gres parser. 
Its architecture is hybrid and consists of both a hand-written grammar and a statistics module that returns the most likely analysis of a sentence.
The primary difference to the English parser is the German grammar and statistics module.
Architecturally, it is different in that it supports the use of morphological information, and does not use a chunker.
ParZu also has a python wrapper that supports various input/output formats and multiprocessing.

REQUIREMENTS
------------

This software was developed and tested using

Linux (32 and 64 bit)
SWI-Prolog 5.6 (or YAP 6.2 - check the top of the script `parzu` to switch between the two)
Python 2.6 (also tested on Python 3.1)
Perl 5.10
NLTK 2.0b8 (for sentence splitting)

It may work in different environments, but there is no guarantee. Any contributions to improve cross-plattform compatibility are welcome.

Additionally, the parser requires the following software and licenses:

- A POS-Tagger using the STTS tagset: (for instance TreeTagger: http://www.ims.uni-stuttgart.de/projekte/corplex/TreeTagger/)
- A tool for morphological analysis (not required, but strongly recommended). Supported:
   - Morphisto: http://code.google.com/p/morphisto/ (open source; recommended)
   - Gertwol: http://www2.lingsoft.fi/cgi-bin/gertwol (proprietary; might be deprecated in the future)

For development and testing, we recommend Tüba-D/Z, which you can obtain at http://www.sfs.uni-tuebingen.de/tuebadz.shtml (free download for academic use).
See the section below on how to extract and integrate statistical information from Tüba-D/Z into the system.

INSTALLATION
------------

1. Install all requirements. In Ubuntu Linux, all are available in the repositories:
    sudo apt-get install python-nltk swi-prolog sfst

2. unpack (or git clone) the directory to your target directory.

3. adjust file paths in the config.ini file. Also define which lemmatisation/morphology you use (if any), and make sure they work.

4. (OPTIONAL): If you have the Tüba-D/Z corpus in the right format (dependencies, CONLL format, UTF-8), you can generate improved statistic files by executing `statistics/create_statistics.sh`. 
    The files advstats* freq* konjstats* ppstats* and vstats* are created in a temporary directory - move them into the `statistics/` folder to make the system use them.
    With evaluation/create_devsets.py , you can create a development from the same Tüba-D/Z file, and perform quality/regression tests with python evaluation/do_evaluation.py.

5. (OPTIONAL): to speed up the parser initialization (with SWI-Prolog), run `statistics/compile.sh`. Repeat this step if you modify the statistics files.

EXAMPLE COMMANDS AND USAGE INFORMATION
--------------------------------------

    echo "Das ist ein Test." | ./parzu
runs preprocessing, the main parsing step and postprocessing.

    ./parzu -i tagged < sample_input
parse pre-tagged text file

    ./parzu -h
show available command line parameters

The output formats are defined in 'postprocessor/postprocessing_module.pl' and can be chosen in config.ini or through command line parameters.
All postprocessed formats are deprojectivized. If you are interested in projective parse trees, use "raw" output and do your own cleanup/format conversion.

The dependency labels implemented in ParZu are described in:

Killian A. Foth. 2005. Eine umfassende Contraint-Dependenz-Grammatik des Deutschen. University of Hamburg.

ON QUALITY AND TESTING
----------------------

Some ballpark figures on what performance to expect (calculated on 1000 Tüba-D/Z sentences):

out-of-the-box:             83%   precision; 81%   recall
with morphisto:             86.5% precision; 83.5% recall
with Tüba-D/Z statistics:   85%   precision; 83%   recall
with both :                 87.5% precision; 84.5% recall

In short: if you use morphisto for morphological analysis, you get 10%-20% fewer errors.
The default statistics are trained on automatic parses of Europarl. This means they are slightly worse than statistics extracted from a hand-created treebank, but license-free.
Using Tüba-D/Z statistics instead of the default ones decreases error rates by another 5-10%.
The quality difference is more extreme for linguistically interesting phenomena (e.g. the functional disambiguation of noun phrases),
since trivial analyses (determiners and attributes) are hardly affected by the lack of morphological or lexical information.

If you have a Tüba-D/Z license, you can use the script `evaluation/create_devsets.py` to generate a development set and do your own performance tests.
Given that both an input and a gold file exist, start the evaluation as follows:
    python evaluation/do_evaluation.py

This will parse the first 1000 sentences* of TüBa-D/Z and report the results.
*Sentences 1-1000 were used during development, sentences 1001-4000 for the evaluation in (Sennrich et al. 2009), and sentences 4001-end for training).
The numbers reported above were obtained with an older Tüba version and may differ slightly.
You can use the online demo at http://www.cl.uzh.ch/kitt/parzu/ to check if there are any major regressions in your local install.

TECHNICAL FAQ
-------------

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

The TreeTagger does not work correctly with tokenized input or the --linewise option.
The reason is that sentence boundaries are internally representend through blank lines (in the tokenized format), which the TreeTagger ignores.
Most other POS taggers (e.g. TnT, Stanford POS tagger, hunpos, CRF++) support the POS tagger input/output format expected by ParZu: one token per line, blank lines for sentence boundaries.


LICENSE
-------

ParZu is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License (see LICENSE).

The files in `postprocessing/DepSVG` are from Kaarel Kaljurand's DepSVG library and are licensed under the LGPL (https://github.com/Kaljurand/DepSVG)

`preprocessing/tokenizer.perl` and `preprocessing/nonbreaking_prefix.de` are from the Moses toolkit and licensed under the LGPL (http://www.statmt.org/moses/)

PUBLICATIONS
------------

The parser is described in:

Rico Sennrich, Gerold Schneider, Martin Volk and Martin Warin (2009): 
   A New Hybrid Dependency Parser for German. In: Proceedings of GSCL Conference, Potsdam.

CONTACT
-------

For questions and feeback, please contact sennrich@cl.uzh.ch or use the GitHub repository.