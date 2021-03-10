The CONLL-X data format (originally from http://ilk.uvt.nl/conll/#dataformat ):


* Data files contain sentences separated by a blank line.
* A sentence consists of one or tokens, each one starting on a new line.
* A token consists of ten fields described in the table below. Fields are separated by a single tab character. Space/blank characters are not allowed in within fields
* All data files will contains these ten fields, although only the ID, FORM, CPOSTAG, POSTAG, HEAD and DEPREL columns are guaranteed to contain non-dummy (i.e. non-underscore) values for all languages.
* Data files are UTF-8 encoded (Unicode).


| Field number: | Field name: | Description: |
| ------------- | ----------- | ------------ |
| 1             | ID          | Token counter, starting at 1 for each new sentence. |
| 2             | FORM        | Word form or punctuation symbol. |
| 3             | LEMMA       | Lemma or stem (depending on particular data set) of word form, or an underscore if not available. |
| 4             | CPOSTAG     | Coarse-grained part-of-speech tag, where tagset depends on the language. |
| 5             | POSTAG      | Fine-grained part-of-speech tag, where the tagset depends on the language, or identical to the coarse-grained part-of-speech tag if not available. |
| 6             | FEATS       | Unordered set of syntactic and/or morphological features (depending on the particular language), separated by a vertical bar (|), or an underscore if not available. |
| 7             | HEAD        | Head of the current token, which is either a value of ID or zero ('0'). Note that depending on the original treebank annotation, there may be multiple tokens with an ID of zero. |
| 8             | DEPREL      | Dependency relation to the HEAD. The set of dependency relations depends on the particular language. Note that depending on the original treebank annotation, the dependency relation may be meaningfull or simply 'ROOT'. |
| 9             | PHEAD       | Projective head of current token, which is either a value of ID or zero ('0'), or an underscore if not available. Note that depending on the original treebank annotation, there may be multiple tokens an with ID of zero. The dependency structure resulting from the PHEAD column is guaranteed to be projective (but is not available for all languages), whereas the structures resulting from the HEAD column will be non-projective for some sentences of some languages (but is always available). |
| 10            | PDEPREL     | Dependency relation to the PHEAD, or an underscore if not available. The set of dependency relations depends on the particular language. Note that depending on the original treebank annotation, the dependency relation may be meaningfull or simply 'ROOT'. |
