# -*- coding: utf-8 -*-

import sys
import shlex
from subprocess import Popen, PIPE

gertwolcmd = shlex.split(sys.argv[1])
gertwolscorecmd = shlex.split(sys.argv[2])

#utf8 to latin1 (for gertwol)
input_latin1 = Popen(["iconv", "-c", "-f", "utf-8", "-t", "latin1"], stdin=sys.stdin, stdout=PIPE, stderr=PIPE)

#call gertwol
gertwol = Popen(gertwolcmd, stdin=input_latin1.stdout, stdout=PIPE, stderr=PIPE) 

#format conversion / gertwol access
gertwol_score = Popen(gertwolscorecmd, stdin=gertwol.stdout, stdout=PIPE, stderr=PIPE)

gertwol2prolog = Popen(["perl", "gertwol2prolog.perl", "-nomorph"], stdin=gertwol_score.stdout, stdout=PIPE, stderr=PIPE)

#latin1 to utf8 (for rest of program)
gertwol_utf8 = Popen(["iconv", "-c", "-f", "latin1", "-t", "utf-8"], stdin=gertwol2prolog.stdout, stdout=PIPE)

gertwol2prolog2 = Popen(['swipl', '-q', '-s', 'gertwol2prolog.pl', '-t', 'asserta(option(var)),go.'], stdin=gertwol_utf8.stdout, stdout=PIPE)

lines = gertwol2prolog2.communicate()[0]

lines = lines.replace("|: ","")
print(lines)