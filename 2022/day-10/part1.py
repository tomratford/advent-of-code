#!/usr/bin/env python3

import sys
import operator

if __name__=='__main__':
    if len(sys.argv) > 1:
        fname = sys.argv[1]
    else:
        fname = input("Enter filename: ")
    # Set initial values
    register=1
    cyc_n=0
    ns_to_mult = [20,60,100,140,180,220]
    n_we_need = 20
    list_of_nums = []
    #work out the register value
    with open(fname, 'r') as f:
        for line in f:
            cyc_n += 1
            if cyc_n >= n_we_need:
                list_of_nums.append(register)
                n_we_need += 40
            if line.strip() != "noop":
                cyc_n += 1
                if cyc_n >= n_we_need:
                    list_of_nums.append(register)
                    n_we_need += 40
                register += int(line.split(" ",1)[1])
    #work out the actual value
    list_to_mult = list_of_nums[0:6]
    list_of_mults = map(operator.mul, ns_to_mult, list_to_mult)
    print(sum(list_of_mults))
