#!/usr/bin/python
# -*- coding: utf-8 -*-

import optparse, sys, logging, csv
log = logging.getLogger("ParseSpeedTestResult")

"""
PROG=./pymoltest
STRUCTURE=1CRN.pdb
real    0m0.303s
user    0m0.096s
sys     0m0.048s
"""

def crop(s):
  s = s.strip()
  if s.startswith("0m"):
    s = s[2:]
    if s.endswith("s"):
      s = s[:-1]
  elif s.startswith("./"):
    s = s[2:]
  elif s.startswith("../"):
    s = s[3:]
  if s.endswith(".pdb"):
    s = s[:-4]
  return s

mem_label      = 'Allocated memory [MB]'
cputime_label  = 'CPU time'
realtime_label = 'Completion time'

labels = ['Program', 'PDB entry', cputime_label, realtime_label, mem_label]

def get_labels(records):
  """Gets a superset of all sets of record
     labels (as dictionary keys) in a list.
  """
  labelsets = [set(rec.keys()) - set(labels)
               for rec in records]
  return labels + list(reduce(set.union, labelsets))

def parse(input):
  "Parses speedtest.sh output."
  current = {}
  result = []
  _biopython_mem_string = "Memory use after BioPython parsing: used Mem("
  _bioruby_mem_string   = "Memory used in megabytes: "
  _biojava_mem_string   = "Used memory is megabytes: "
  for line in input:
    if line.startswith("PROG="):                       # from speedtest.sh
      if current != {}:
        result.append(current)
        current = { 'Program' : crop(line[5:]) }
    elif line.startswith("STRUCTURE="):                # from speedtest.sh
      current['PDB entry'] = crop(line[10:])
    elif line.startswith("user"):                      # from shell `time` command
      current[cputime_label]  = crop(line[5:])
    elif line.startswith("real"):                      # from shell `time` command
      current[realtime_label]  = crop(line[5:])
    elif line.startswith(_biopython_mem_string):       # BioPython
      current[mem_label] = float(line[len(_biopython_mem_string):].strip().split(")")[0])/1024/1024
    elif line.startswith(_bioruby_mem_string):         # BioRuby
      current[mem_label] = float(line[len(_bioruby_mem_string):].strip().split(" ")[0])
    elif line.startswith(_biojava_mem_string): # BioJava
      current[mem_label] = int(line[len(_biojava_mem_string):].strip().split(" ")[0])
    elif "total memory in use" in line:                # Haskell RTS reports it in MB
      current[mem_label] = int(line.strip().split(" ")[0].replace(",", ""))# *1024*1024
  if current != {}:
    result.append(current)
  return result

def prettyprint(output, labels, records):
  "Writes a set of records with a given order of labels into CSV file."
  writer = csv.writer(output)
  writer.writerow(labels)
  for rec in records:
    row = [rec.get(label, "")
           for label
	   in labels]
    writer.writerow(row)

if __name__=="__main__":
  # Options
  optparser = optparse.OptionParser(usage="%prog [<options>] <input.txt> [<output.csv>]")
  # Logging
  optparser.add_option("-v", "--verbose",
                       action="callback",
                       callback=lambda _a,_b,_c,_d: logging.root.setLevel(max(logging.root.level-10, 0)),
                       help="print more run info, can be used multiple times")
  optparser.add_option("-q", "--quiet",
                       action="callback",
                       callback=lambda _a,_b,_c,_d: log.setLevel(logging.ERROR),
                       help="print hard errors only")
  logging.basicConfig(stream = sys.stderr, format='%(levelname)-8s %(message)s', level=logging.WARN)
  
  opts, args = optparser.parse_args() # Parse arguments
  
  # Verify arguments
  if len(args) not in (1, 2):
    print >> sys.stderr, optparser.format_help()
    sys.exit(1)
   
  with open(args[0], "r") as input:
    records = parse(input)
    labels = get_labels(records)
    if len(args) == 1:
      prettyprint(sys.stdout, labels, records)
    else:
      with open(args[1], "w") as output:
        prettyprint(output, labels, records)

