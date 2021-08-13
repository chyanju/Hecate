#!/usr/bin/env python3

import argparse
import re
import subprocess
import os
import sys

parser = argparse.ArgumentParser(description='Run experiments')
parser.add_argument('--seed', type=int, help='random seed', default=0)
parser.add_argument('--N', type=int, help='number of trials', default=10)
parser.add_argument('--compile', action="store_true", help='force recompilation', default=False)

NUM_WARMUP = 5

def exec(cmd):
  ret = subprocess.run(cmd, shell=True, capture_output=True)
  if not ret.returncode == 0:
    print(ret)
    assert(False)
  return ret
  

if __name__ == '__main__':
  args = parser.parse_args()
  flags = "-O2 -std=c++11 -Wattributes -Wno-write-strings"
  progs = {
    "prog_unfused" : ("UNFUSED", ""),
    "prog_fused" : ("FUSED", ""),
    "prog_hecate_l": ("UNFUSED", "-D HECATE"),
    "prog_hecate_v": ("UNFUSED", "-D HECATE -D RESOLVE -D VECTOR"),
    "prog_hecate_par": ("UNFUSED", "-D HECATE -D RESOLVE -D VECTOR -fopenmp")
  }

  if args.compile:
    for prog, (d, cpp_flags) in progs.items():
      cmd = f"g++-10 {d}/main.cpp {cpp_flags} {flags} -o {prog}"
      print(cmd)
      exec(cmd)

  params = [
    [10, 100, 1000, 10000, 100000, 200000],
    [1, 10, 20, 30, 40, 50],
    [50, 100, 125, 150, 175, 200]
  ]

  re_runtime = re.compile("Time:\s*([0-9]+)")
  re_visits = re.compile("Node Visits:\s*([0-9]+)")
  re_treesize = re.compile("Tree size:\s*([0-9]+)")

  worklist = [
    ("", "FUSED", "prog_fused"), 
    ("", "UNFUSED", "prog_unfused"), 
    ("-D HECATE", "UNFUSED", "prog_hecate"),
    ]

  data = list()
  with open("results.csv", "w") as csvfile:
    import csv
    writer = csv.writer(csvfile, delimiter=',', quotechar='|', quoting=csv.QUOTE_MINIMAL)
    for doc_i, param_list in enumerate(params):
      for param in param_list:
        for prog in progs:
          print("Program: {:<20} Doc: {:<10} Param: {:<10}".format(prog, doc_i, param))
        
          cmd = f"./{prog} {param} {doc_i+1} {args.seed}"

          total = 0
          for n in range(-NUM_WARMUP, args.N):
            ret = exec(cmd)
            res = ret.stderr.decode('utf-8')
            treesize = int(re_treesize.search(res).groups()[0])
            runtime = int(re_runtime.search(res).groups()[0])
            
            if n >= 0:
              total += runtime
              print("Trial: {:<10} Tree size: {:<20} Time : {:<20}".format(n, treesize, runtime))
              row = [doc_i, prog, param, treesize, n, runtime]
              data.append(row)
              writer.writerow(row)
              print(row)
          
          row = [doc_i, prog, param, treesize, -1, total / args.N]
          data.append(row)
          writer.writerow(row)
          print(row)
  
      


        
          

    # with open(filename, "w") as out:
      