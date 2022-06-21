#!/usr/bin/env python
import os
import shutil
import sys
from datetime import datetime, timedelta

ARTIFACT_ROOT = sys.path[0] # sys.path[0] is the directory containing this script
DOOP_HOME = os.path.join(ARTIFACT_ROOT, 'doop')
DOOP_CACHE = os.path.join(DOOP_HOME, 'cache')


ANALYSIS={
    '1callSL':'1-call-site-sensitive+heap+SL',
    'ci':'context-insensitive',
    'graph_ci':'context-insensitive',
    '2obj':'2-object-sensitive+heap',
    'graphick':'2-object-sensitive+heap+Data',
}


ALLOW_PHANTOM=[]

UNSCALABLE={
}

MAIN={
    'pgm.jar':'pgm.Main'
}



TAMIFLEX={
    'pgm.jar':'pgm-refl.log',
}





RESET  = '\033[0m'
CYAN   = '\033[36m'
BOLD = '\033[1m'

def getCP(app):
    if CP.has_key(app):
        return CP[app]
    elif app in DACAPO:
        return 'jars/dacapo/%s.jar' % app
    else:
        raise Exception('Unknown application: %s' % app)

def getPTACommand(app, analysis, main):
    cmd = './run -jre1.6 --cache --color '
    if analysis == 'graph_ci':
        cmd += '-graph '
    cmd += '--allow-phantom '
    cmd += '-main %s ' % main 
    cmd += '%s %s ' % (ANALYSIS[analysis], app)
    #cmd += '| tee results/%s-%s.output' % (app, analysis)
    print 'cmd : {}'.format(cmd)
    return cmd

 

def runPTA(main, pta, app):
  if pta == 'graphick':
    cmd = getPTACommand(app, 'graph_ci', main)
    os.system(cmd) 
    print 'cmd : {}'.format(cmd)
    cmd = './query.sh'
    os.system(cmd) 

  cmd = getPTACommand(app, pta, main)
  os.system(cmd)



def clean():
  cmd = 'rm -r cache/*'
  os.system(cmd)
 


def run(args):
  start = datetime.now()
  pta = args[0]
  if args[0] == '-clean':
    clean()
  elif args[1] == '-main':
    main = args[2]
    app = args[3]
    runPTA(main,pta,app)
  #else:
  #  runPTANoMain(pta,args[1])
  end = datetime.now()
  print 'taken time: {}'.format(end-start)
#run.py graphick -main Main pgm.jar
if __name__ == '__main__':
    run(sys.argv[1:])
