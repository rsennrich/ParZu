#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Very simple web server for ParZu
"""

from __future__ import unicode_literals

import sys
import logging
import traceback
from flask import Flask, request, Response, abort
from pexpect.exceptions import TIMEOUT

from parzu_class import Parser, process_arguments

valid_inputformats = ['plain', 'tokenized', 'tokenized_lines', 'tagged']
suggested_outputformats = ['conll', 'prolog', 'graphical']
other_outputformats = ['tokenized', 'tagged', 'preprocessed', 'moses', 'raw']
valid_outputformats  = suggested_outputformats + other_outputformats

outputformat_docstring = ""
for outputformat in suggested_outputformats:
  outputformat_docstring += """      <li><a href="/parse?text=Ich bin ein Berliner.&format={0}">{0}</a></li>""".format(outputformat)

inputformat_docstring = ""
inputformat_docstring += """      <li><a href="/parse?text=Ich bin ein Berliner. Er ist ein Hamburger.&inputformat=plain">plain</a></li>"""
inputformat_docstring += """      <li><a href="/parse?text=Ich%0Abin%0Aein%0ABerliner%0A.%0A%0AEr%0Aist%0Aein%0AHamburger%0A.&inputformat=tokenized">tokenized</a> (one line per word; empty line marks end of sentence)</li>"""
inputformat_docstring += """      <li><a href="/parse?text=Ich bin ein Berliner .%0AEr ist ein Hamburger .&inputformat=tokenized_lines">tokenized_lines</a> (one line per sentence; tokens separated by whitespace)</li>"""
inputformat_docstring += """      <li><a href="/parse?text=Ich PPER%0Abin VAFIN%0Aein ART%0ABerliner NN%0A. $.%0A%0AEr PPER%0Aist VAFIN%0Aein ART%0AHamburger NN%0A. $.&inputformat=tagged">tagged</a> (one line per word; empty line marks end of sentence; POS-tag for each word separated by whitespace/tab)</li>"""

index_str = """<!doctype html>
<html lang="en">
<title>ParZu API</title>
<body>
Simple web API for ParZu, supporting both GET and POST requests.
<br/>
<br/>
Arguments:
<ul>
  <li>text: the raw text that you want to parse</li>
  <li>format: the desired output format. Suggested choices:
  <ul>
  {0}
  </ul>
  </li>
  <li>inputformat: the format of the input string. Valid choices:
  <ul>
  {1}
  </ul>
  </li>
</ul>
<br/>
For more information, see <a href="http://github.com/rsennrich/ParZu">http://github.com/rsennrich/ParZu</a>.
</body></html>
""".format(outputformat_docstring, inputformat_docstring)

class Server(object):

    def __init__(self, timeout=10, options=None):
        if options is None:
            options = process_arguments(commandline=False)
            options['extrainfo'] = 'secedges'
        self.parser = Parser(options, timeout=timeout)
        self.app = Flask('ParZuServer')

        @self.app.route('/', methods=['GET'])
        def index():
            return Response(index_str, 200, mimetype='text/html')

        @self.app.route('/parse/', methods=['GET', 'POST'])
        def parse():
            if request.method == "GET":
                text = request.args.get('text', None)
                outputformat = request.args.get('format', 'conll')
                inputformat = request.args.get('inputformat', 'plain')
            else:
                input = request.get_json(force=True)
                text = input.get('text')
                outputformat = input.get('format', 'conll')
                inputformat = input.get('inputformat', 'plain')
            if not text:
                return "Please provide text as POST data or GET text= parameter\n", 400

            if outputformat not in valid_outputformats:
                return "Please provide valid output format as POST data or GET format= parameter\n</br>Valid output formats are: <ul><li>{0}</li></ul>".format('</li>\n<li>'.join(valid_outputformats)), 400

            if inputformat not in valid_inputformats:
                return "Please provide valid input format as POST data or GET inputformat= parameter\n</br>Valid inputformats are: <ul><li>{0}</li></ul>".format('</li>\n<li>'.join(valid_inputformats)), 400

            try:
                parses = self.parser.main(text, inputformat=inputformat, outputformat=outputformat)
            except TIMEOUT as e:
                sys.stderr.write(traceback.format_exc())
                del self.parser
                self.parser = Parser(options)
                return abort(408)

            if outputformat in ['tokenized', 'tagged', 'conll', 'prolog', 'moses']:
                result = '\n'.join(parses)
            elif outputformat in ['preprocessed', 'raw']:
                result = parses
            elif outputformat == 'graphical':
                return Response(parses[0], mimetype='image/svg+xml')

            return Response(result, mimetype='text/plain')

if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("--port", "-p", type=int, default=5003,
                        help="Port number to listen to (default: 5003)")
    parser.add_argument("--host", "-H", help="Host address to listen on (default: localhost)")
    parser.add_argument("--debug", "-d", help="Set debug mode", action="store_true")
    parser.add_argument("--timeout", type=int, default=10,
                        help="Timeout for each step in pipeline (total processing time may be longer).")
    args = parser.parse_args()

    logging.basicConfig(level=logging.DEBUG if args.debug else logging.INFO,
                        format='[%(asctime)s %(name)-12s %(levelname)-5s] %(message)s')

    server = Server(timeout=args.timeout)

    server.app.run(port=args.port, host=args.host, debug=args.debug, threaded=True)
