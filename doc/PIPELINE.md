THE PARZU PIPELINE
------------------

ParZu consists of a pipeline of processing steps. The individual components are managed by `parzu.py` (for the standalone script that reads from stdin and writes to stdout), and `parzu_class.py` (for the class that can be imported into Python scripts, and is used for the server-client architecture).
Here is a short description of the individual steps:

- sentence segmentation. This uses the punkt_tokenizer from NLTK. To process input that has already been segmented, use the argument `--linewise` (ParZu then expects each input line to be a sentence), or set the input format to be at a later step.

- tokenization. This segments a sentence into words, using the Moses tokenizer. To process input that has already been tokenized, set the input format to `--tokenized` or `--tokenized_lines`. (the former is one word per line, the latter one sentence per line).

- POS tagging. This assigns a POS tag from the [STTS tag set](http://www.ims.uni-stuttgart.de/forschung/ressourcen/lexika/TagSets/stts-table.html) to each word, using [clevertagger](https://github.com/rsennrich/clevertagger) by default. To process input that has already been tagged, set the input format to `--tagged`.

  ParZu relies on the parts-of-speech from POS tagging for most of its syntactic rules. To reduce the effect of POS errors, ParZu also supports n-best-tagging, where the n-best tag sequences are all parsed, and the outputs are re-ranked based on features such as the number of root elements.

- morphologial analysis. This uses an SMOR or [Zmorge](https://pub.cl.uzh.ch/users/sennrich/zmorge/) transducer by default.

  ParZu relies on morphological analysis to identify verb complexes, and for some syntactic rules, most importantly for the functional disambiguation of noun phrases (SUBJ vs. OBJA vs. OBJD).

  A second function of morphological analysis is lemmatisation. Internally, lemmatisation reduces the sparsity of some statistics in the core parser.

- preprocessing. In this Prolog script, verb complexes (e.g. "hat sehen wollen") are identified so statistics can be based on the full verb.

- ParZu core. ParZu is a CYK-parser implemented in Prolog. It uses a backbone of rule-based constraints based on POS tags, morphology, position, and other features, and uses Treebank statistics for disambiguation. Morphological constraints are tested via unification.

  Treebank statistics can be obtained from the TüBa-D/Z treebank, or the Hamburg dependency treebank. The pre-built statistics distributed with ParZu are based on automatically parsed text.

  ParZu core also calls a Prolog postprocessing script, in which it performs deprojectivization, identification of secondary edges and some minor mappings of label names.

- postprocessing. In a cleanup script, debugging output of the core parser is removed. If n-best-tagging is active, the parses are reranked and the best one is selected.

- generate_graphics. If the desired output format is graphical, the SVG image is generated with DepSVG.
