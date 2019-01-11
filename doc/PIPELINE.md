THE PARZU PIPELINE
------------------

- sentence segmentation. This uses the punkt_tokenizer from NLTK. To process input that has already been segmented, use the argument `--linewise` (ParZu then expects each input line to be a sentence), or set the input format to be at a later step.

- tokenization. This uses the Moses tokenizer. To process input that has already been tokenized, set the input format to `--tokenized` or `--tokenized_lines`. (the former is one word per line, the latter one sentence per line).

- POS tagging. This uses [clevertagger](https://github.com/rsennrich/clevertagger) by default. To process input that has already been tagged, set the input format to `--tagged`.

  ParZu relies on the parts-of-speech from POS tagging for most of its syntactic rules. To reduce the effect of POS errors, ParZu also supports n-best-tagging, where the n-best tag sequences are all parsed, and the outputs are re-ranked based on features such as the number of root elements.

- morphologial analysis. This uses an SMOR or [Zmorge](https://pub.cl.uzh.ch/users/sennrich/zmorge/) transducer by default.

  ParZu relies on morphological analysis to identify verb complexes, and for some syntactic rules, most importantly for the functional disambiguation of noun phrases (SUBJ vs. OBJA vs. OBJD).

  A second function of morphological analysis is lemmatisation. Internally, lemmatisation reduces the sparsity of some statistics in the core parser.

- preprocessing. In this Prolog script, verb complexes (e.g. "hat sehen wollen") are identified so statistics can be based on the full verb.

- ParZu core. ParZu is a CYK-parser implemented in Prolog. It uses a backbone of rule-based constraints based on POS tags, morphology, position, and other features, and uses Treebank statistics for disambiguation. Morphological constraints are tested via unification.

  Treebank statistics can be obtained from the TüBa-D/Z treebank, or the Hamburg dependency treebank. The pre-built statistics distributed with ParZu are based on automatically parsed text.

- postprocessing. In this Prolog script, we perform deprojectivization, identification of secondary edges and other modifications.
