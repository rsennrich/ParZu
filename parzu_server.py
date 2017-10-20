"""
Very simple web server for ParZu
See: http://github.com/rsennrich/ParZu
"""
import csv
import logging

from io import StringIO, BytesIO

import sys
from flask import Flask, request, Response
import subprocess
import os

PYTHON2 = (sys.version_info[0] == 2)

app = Flask('ParZuServer')

@app.route('/', methods=['GET'])
def index():
    return 'Simple web API for ParZu, see  this <a href="/parse?text=Ich bin ein Berliner">example</a>) and see <a href="http://github.com/rsennrich/ParZu">http://github.com/rsennrich/ParZu</a> for more information.', 200


@app.route('/parse/', methods=['GET', 'POST'])
def parse():
    if request.method == "GET":
        text = request.args.get('text', None)
    else:
        input = request.json
        text = input.get('text')
    if not text:
        return "Please provide text as POST data or GET text= parameter\n", 400
    result = convert_output(do_parse(text))
    return Response(result, mimetype='text/csv')

def convert_output(output):
    '1\tIch\tich\tPRO\tPPER\t1|Sg|_|Nom\t2\tsubj\t_\t_ \n2\tbin\tsein\tV\tVAFIN\t1|Sg|Pres|Ind\t0\troot\t_\t_ \n3\tein\teine\tART\tART\tIndef|_|_|Sg\t4\tdet\t_\t_ \n4\tBerliner\tBerliner\tADJA\tADJA\t_|_|Sg\t2\tpred\t_\t_ \n\n'
    out = BytesIO() if PYTHON2 else StringIO()
    w = csv.writer(out)
    for line in csv.reader(StringIO(output), delimiter="\t"):
        w.writerow(line)
    return out.getvalue().strip() + "\n"

def do_parse(text):
    path = os.path.dirname(os.path.realpath(__file__))
    cmd = [os.path.join(path, "parzu")]
    logging.debug("Calling {cmd} with {} bytes of input".format(len(text), **locals()))
    p = subprocess.Popen(cmd, shell=False, stdin=subprocess.PIPE, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
    out, err = p.communicate(text.encode("utf-8"))
    if p.returncode != 0:
        raise Exception(err)
    return out.decode("utf-8")


if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("--port", "-p", type=int, default=5003,
                        help="Port number to listen to (default: 5003)")
    parser.add_argument("--host", "-H", help="Host address to listen on (default: localhost)")
    parser.add_argument("--debug", "-d", help="Set debug mode", action="store_true")
    args = parser.parse_args()

    logging.basicConfig(level=logging.DEBUG if args.debug else logging.INFO,
                        format='[%(asctime)s %(name)-12s %(levelname)-5s] %(message)s')

    app.run(port=args.port, host=args.host, debug=args.debug, threaded=True)
