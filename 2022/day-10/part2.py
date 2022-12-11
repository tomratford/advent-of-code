#!/usr/bin/env python3

import sys
import operator

if __name__=='__main__':
    if len(sys.argv) > 1:
        fname = sys.argv[1]
    else:
        fname = input("Enter filename: ")
    register=1
    cyc_n=0
    rvals = [] #register values
    #work out the register values
    with open(fname, 'r') as f:
        for line in f:
            rvals.append(register)
            if line.strip() != "noop":
                rvals.append(register)
                register += int(line.split(" ",1)[1])
    #draw the telly
    for i in range(6):
        line = ""
        for i in range(40):
            if abs(i - rvals.pop(0)) < 2:
                line += "#"
            else:
                line += "."
        print(line)
