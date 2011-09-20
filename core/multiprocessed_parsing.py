#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright © 2010-2011 University of Zürich
# Author: Rico Sennrich <sennrich@cl.uzh.ch>
# Implements multiprocessed parsing

import sys
import os
import time
import multiprocessing
from subprocess import Popen, PIPE


class Parser(multiprocessing.Process):
    
    def __init__(self, task_queue, result_dict,commandlist,commandpath):
        multiprocessing.Process.__init__(self)
        self.task_queue = task_queue
        self.result_dict = result_dict
        self.parsing = Popen(commandlist,cwd=commandpath, stdin=PIPE, stdout=PIPE)

    def run(self):
        while True:
            self.index,self.next_task = self.task_queue.get()
            
            if self.next_task is None:
                # Poison pill in task queue
                self.parsing.terminate()
                break

            self.parsing.stdin.write(self.next_task)
            self.islocked = 1
            self.answer = ''
            while self.islocked:
                  self.outline = self.parsing.stdout.readline()
                  self.answer += self.outline
                  #This signals that parser has finished with the sentence and can begin with the next one
                  if self.outline == '%%END_OF_SENTENCE\n':
                      self.islocked = 0
                      
            self.result_dict[self.index] = self.answer
        return


#Segments input into sentences and sends them to tasks queue
def segment_sent(inpipe,tasks,num_parsers,sentdelim,todo):
    
      i = 0
      msg = ''
      
      for line in inpipe:
          msg += line
          #sentdelim signals end of sentence; sentence is sent to queue as single job
          if '_' + sentdelim + "']," in line:
              todo.value = i+2 # this ensures that number of finished task never equals todo
              tasks.put((i,msg + '\n'))
              i += 1
              msg = ''
              
      # Add a poison pill for each parser
      for i in xrange(num_parsers):
          tasks.put((None,None))
          
      # After this point, allow process to finish if all tasks are done
      todo.value -= 1


#Results from all processes are written to shared dictionary
#this function writes the results and clears memory as soon as possible
def generate_output(results,todo,outpipe):
    
      i = 0
      #This is only False if all sentences have been parsed
      while i < todo.value:
          
            #wait till result #i is populated
            while True:
                try:
                    result = results[i]
                    break
                except KeyError:
                    time.sleep(0.1)

            outpipe.write(result)
            del(results[i])
            i += 1
      
      outpipe.close

#Segments sentences, parses them using num_parsers parallel processes, and combines output of all parser processes
def main(inpipe,outpipe,num_parsers,sentdelim,commandlist,commandpath):

      # Establish process communication protocols
      tasks = multiprocessing.Queue(num_parsers+1) #sends tasks from segment_sent to parser
      
      manager = multiprocessing.Manager()
      results = manager.dict()
      todo = manager.Value('i',1)

      # Start parsers
      parsers = [ Parser(tasks, results,commandlist,commandpath)
                    for i in xrange(num_parsers) ]
      for w in parsers:
          w.start()

      #enqueue sentences to parse
      p = multiprocessing.Process(target=segment_sent, args=(inpipe,tasks,num_parsers,sentdelim,todo))
      p.start()

      generate_output(results,todo,outpipe)



if __name__ == "__main__":
    import sys

    #hack to relay stdin to new multiprocessing.Process
    init_pipe = Popen(["cat"], stdin=sys.stdin,stdout=PIPE)

    try:
        num_parsers = int(sys.argv[1])
    except: 
        num_parsers = 2
        
    args = sys.argv[2]
    sentdelim = sys.argv[3]
    path = sys.argv[4]
    prolog = sys.argv[5]
    prolog_load = sys.argv[6]
    
    runCMD = [prolog, '-q', '-G128M', '-L128M', prolog_load, 'ParZu-parser.pl', '-t', args]


    main(init_pipe.stdout,sys.stdout,num_parsers,sentdelim,runCMD,path)