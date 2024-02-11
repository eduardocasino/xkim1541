#!/usr/bin/env python

import re
import argparse

rx_dict = {
    'begin': re.compile(r'^Exports list by value:\n'),
    'definition': re.compile(r'(\S*)\s+00([0-9A-F]{4})  [A-Z][A-Z]'),
    'end': re.compile(r'^Imports list:\n')
}

start = 0

header = """
; This file is generated automatically by the includes.py script.
; *** DO NOT MODIFY ***
;
"""

parser = argparse.ArgumentParser(
                    prog='includes.py',
                    description='Generate includes from ld65 map files')
parser.add_argument('--mapfile', metavar='<file>', nargs=1, required=True)
parser.add_argument('--include', metavar='<file>', nargs='+')

args = parser.parse_args()

print(header)

for f in args.include:
    with open(f, 'r') as ifile:
        line = ifile.readline()

        while line:
            print(line, end='')
            line = ifile.readline()

print("\n; Symbols\n;")

with open(args.mapfile[0], 'r') as mapfile:
    line = mapfile.readline()

    while line:
        for key, rx in rx_dict.items():
            ismatch = rx.findall(line)
            if ismatch:

                match key:
                    case "begin":
                        start = 1
                    
                    case "end":
                        start = 0

                    case "definition":
                        if start == 1:
                            for symbol, value in ismatch:
                                lineout = symbol + "\t\t= $" + value 
                                print(lineout.expandtabs(8))
        
        line = mapfile.readline()

