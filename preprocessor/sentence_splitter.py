#!/usr/bin/python
# -*- coding: utf-8 -*-
import sys
import nltk

try:
    tokenizer = nltk.data.load('tokenizers/punkt/german.pickle')
except:
    nltk.download('punkt')
    tokenizer = nltk.data.load('tokenizers/punkt/german.pickle')


#modified version of _slices_from_text in nltk/tokenize/punkt.py to process stdin line-by-line instead of requiring string
def tokenize():
    last_break = 0
    buf = ''
    for line in sys.stdin:
        buf += line.strip() + ' '
        for match in tokenizer._lang_vars.period_context_re().finditer(buf):
            context = match.group() + match.group('after_tok')
            if tokenizer.text_contains_sentbreak(context):
                yield buf[0:match.end()]
                if match.group('next_tok'):
                    # next sentence starts after whitespace
                    buf = buf[match.start('next_tok'):]
                else:
                    # next sentence starts at following punctuation
                    buf = buf[match.end():]
    yield buf

for sent in tokenize():
    print sent.replace('\n','').strip()