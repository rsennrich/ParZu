# -*- coding: utf-8 -*-

import sys

gertwolinpath = sys.argv[1]
sentdelim = sys.argv[2]

#used for Gertwol
uniquedict = {}
gertwol = open(gertwolinpath,'w')

pos = ''
#format conversion
for line in sys.stdin:
    linelist = line.split()
    if not linelist: # skip empty lines
        if sentdelim == '$newline':
            print("w('ENDOFLINE','"+sentdelim+"',['._"+sentdelim+"'],'ENDOFLINE').")
        continue
    if len(linelist) == 1:
      word = linelist[0].replace("\\","\\\\").replace("'","\\'")
      pos = 'XY'
      sys.stderr.write('Error: No tag provided: {0}'.format(line))
    else:
      word = linelist[0].replace("\\","\\\\").replace("'","\\'")
      pos = linelist[1].replace("\\","\\\\").replace("'","\\'")
    newline = "w('" + word + "','" + pos + "',['" + word + '_' + pos + "'],'" + word + "').\n"
    print(newline),

#prepare Gertwol Input
    uniquedict[linelist[0]]=0
    
    #deal with spelling variations that Gertwol doesn't know
    if linelist[0].startswith('Ae'):
       uniquedict["Ä" + linelist[0][2:]]=0
    elif linelist[0].startswith('Oe'):
       uniquedict["Ö" + linelist[0][2:]]=0
    elif linelist[0].startswith('Ue'):
       uniquedict["Ü" + linelist[0][2:]]=0
    if "ss" in linelist[0]:
       sharplist = linelist[0].split('ss')
       for i in range(len(sharplist)-1):
         uniquedict[sharplist[i]+'ß'+sharplist[i+1]]=0


if pos != sentdelim:
   print("w('ENDOFDOC','"+sentdelim+"',['._"+sentdelim+"'],'ENDOFDOC').")

for item in uniquedict.keys():
    gertwol.write(item + '\n')

gertwol.close()