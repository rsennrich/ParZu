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
    buf = ''
    for line in sys.stdin:
        buf += line.strip() + ' '
        last_break = 0
        for match in tokenizer._lang_vars.period_context_re().finditer(buf):
            context = match.group() + match.group('after_tok')
            if tokenizer.text_contains_sentbreak(context):
                yield buf[last_break:match.end()]
                if match.group('next_tok'):
                    # next sentence starts after whitespace
                    last_break = match.start('next_tok')
                else:
                    # next sentence starts at following punctuation
                    last_break = match.end()
        buf = buf[last_break:]
    yield buf

for sent in tokenize():
    print sent.replace('\n','').strip()