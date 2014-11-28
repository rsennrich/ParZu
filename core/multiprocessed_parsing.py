#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright © 2010-2011 University of Zürich
# Author: Rico Sennrich <sennrich@cl.uzh.ch>
# Implements multiprocessed parsing

from __future__ import unicode_literals
import sys
import os
import time
import multiprocessing
import codecs
from subprocess import Popen, PIPE


class Parser(multiprocessing.Process):
    
    def __init__(self, task_queue, result_dict,commandlist,commandpath,error_signal):
        multiprocessing.Process.__init__(self)
        self.task_queue = task_queue
        self.result_dict = result_dict
        self.parsing = Popen(commandlist,cwd=commandpath, stdin=PIPE, stdout=PIPE)
        self.error_signal = error_signal

    def run(self):
        while True:
            index,next_task = self.task_queue.get()
            
            if next_task is None:
                # Poison pill in task queue
                self.parsing.terminate()
                break
                
            self.parsing.stdin.write(next_task)
            self.parsing.stdin.flush()
            answer = ''

            while True:
                outline = self.parsing.stdout.readline().decode("UTF-8")
                #if we ever reach the end of parsing.stdout, this indicates that the parser process has crashed
                if not outline:
                    sys.stderr.write("Parser process {0} has crashed on sentence {1}. If you don't see swipl error message, try parsing sentence in single-processed mode (option -p 1).\n".format(self.parsing.pid,index+1))
                    self.error_signal.value = 1
                    self.parsing.terminate()
                    return
                
                answer += outline
                #This signals that parser has finished with the sentence and can begin with the next one
                if outline == '%%END_OF_SENTENCE\n':
                    break
                      
            self.result_dict[index] = answer
        return



#Segments input into sentences and sends them to tasks queue
def segment_sent(inpipe,tasks,num_parsers,sentdelim,todo):
    
      i = 0
      msg = b''
      
      for line in inpipe:
          msg += line
          #sentdelim signals end of sentence; sentence is sent to queue as single job
          if b'_' + sentdelim + b"']," in line:
              todo.value = i+2 # this ensures that number of finished task never equals todo
              tasks.put((i,msg + b'\n'))
              i += 1
              msg = b''
              
      # Add a poison pill for each parser
      for i in range(num_parsers):
          tasks.put((None,None))
          
      # After this point, allow process to finish if all tasks are done
      todo.value -= 1


#Results from all processes are written to shared dictionary
#this function writes the results and clears memory as soon as possible
def generate_output(results,todo,outpipe,error_signal):
    
    i = 0
    #This is only False if all sentences have been parsed or swipl crashed
    while i < todo.value and not error_signal.value:
        try:
            result = results[i]
            outpipe.write(result)
            del(results[i])
            i += 1
        except KeyError:
            time.sleep(0.1)
            if error_signal.value:
                break
                
    outpipe.close

#Segments sentences, parses them using num_parsers parallel processes, and combines output of all parser processes
def main(inpipe,outpipe,num_parsers,sentdelim,commandlist,commandpath):

      # Establish process communication protocols
      tasks = multiprocessing.Queue(num_parsers+1) #sends tasks from segment_sent to parser
      
      manager = multiprocessing.Manager()
      results = manager.dict()
      todo = manager.Value('i',1)
      error_signal = manager.Value('i',0)

      # Start parsers
      parsers = [ Parser(tasks, results,commandlist,commandpath,error_signal)
                    for i in range(num_parsers) ]
      for w in parsers:
          w.start()

      #enqueue sentences to parse
      p = multiprocessing.Process(target=segment_sent, args=(inpipe,tasks,num_parsers,sentdelim,todo))
      p.start()

      generate_output(results,todo,outpipe,error_signal)
      
      #prevent hangup when all parser processes crash (which hopefully never happens)
      for parser in parsers:
          parser.terminate()
      p.terminate() 



if __name__ == "__main__":

    if sys.version_info < (3, 0):
        sys.stdout = codecs.getwriter('UTF-8')(sys.stdout)
        sys.stderr = codecs.getwriter('UTF-8')(sys.stderr)
        sys.stdin = codecs.getreader('UTF-8')(sys.stdin)

    #hack to relay stdin to new multiprocessing.Process
    init_pipe = Popen(["cat"], stdin=sys.stdin,stdout=PIPE)

    try:
        num_parsers = int(sys.argv[1])
    except: 
        num_parsers = 2
        
    args = sys.argv[2]
    if sys.version_info < (3, 0):
        sentdelim = bytes(sys.argv[3])
    else:
        sentdelim = bytes(sys.argv[3], encoding="UTF-8")
    path = sys.argv[4]
    prolog = sys.argv[5]
    prolog_load = sys.argv[6]
    
    runCMD = [prolog, '-q', prolog_load, 'ParZu-parser.pl', '-g', args]
    
    if prolog.endswith('swipl'):
        runCMD += ['-t', 'halt.','-G248M', '-L248M']

    main(init_pipe.stdout,sys.stdout,num_parsers,sentdelim,runCMD,path)