#!/usr/bin/env python3

import sys
import re
import copy

# A group of monkey is called a troop!
class Troop:
    '''
    Attributes:
      monkeys - A dictionary of integer (monkey id), Monkey() objects
    '''
    def __init__(self):
        self.monkeys={}

    def __str__(self):
        str = ""
        for x in self.monkeys.keys():
            str += "Monkey {}:\n".format(x) + self.monkeys[x].__str__() + "\n\n"
        return str

    def add(self, mid, monkey):
        '''Add a new monkey to the troop'''
        self.monkeys[mid] = monkey

    def throw(self, item, mid):
        '''Throw a new item to a monkey'''
        self.monkeys[mid].items.append(item)

    def round(self):
        '''Perform a `round` of all monkeys'''
        for x in self.monkeys.keys():
            moves = self.monkeys[x].turn()
            for m in moves:
                self.throw(m[0],m[1])

    def counts(self):
        '''return the list of counts for each monkey'''
        rtn = []
        for x in self.monkeys.keys():
            rtn.append(self.monkeys[x].count)
        return rtn

class Monkey:
    '''
    Attributes:
      items - a list of worry levels for each item the monkey is holding
      operation - a lambda returning an integer
      test - A lambda returning the new monkey to throw to
      count - the number of times a monkey has inspected an item
    '''
    def __init__(self, items, operation, test):
        self.items = items
        self.operation = operation
        self.test = test
        self.count = 0
        
    def __str__(self):
        return "Items: {}\nop: {}\ntest: {}\ncount: {}".format(self.items, self.operation, self.test, self.count)

    def turn(self):
        rtn = []
        while len(self.items) > 0:
            item = self.items.pop(0)
            new = int(self.operation(item)/3) if PART1 else self.operation(item)
            self.count += 1
            throwTo = self.test(new)
            rtn.append((new, throwTo))
        return rtn
        
class Parser:
    '''
    This takes a whole `monkey block` of the type:
    
      Monkey 0:
        Starting items: 79, 98
        Operation: new = old * 19
        Test: divisible by 23
          If true: throw to monkey 2
          If false: throw to monkey 3

    and the user can 
    '''
    def __init__(self, string):
        self.string = string

    def _monkeyId(self):
        try:
            regex = re.compile(r'^Monkey (\d+):$', re.M)
            return int(re.search(regex, self.string).group(1))
        except:
            print("Could not parse monkey Id")

    def _startingItems(self):
        try:
            regex = re.compile(r'^  Starting items: (.*)$', re.M)
            nums = re.search(regex, self.string).group()
            vals = list(map(int, re.findall(r'\d+', nums)))
            return vals
        except:
            print("Could not parse starting items")

    def _operation(self):
        try:
            vals = re.search(r'(?m)^  Operation: new = old (.) (.+)$', self.string)
            try:
                digit = int(vals.group(2))
                return {
                    '+': lambda x: x + digit,
                    '*': lambda x: x * digit
                }[vals.group(1)]
            except:
                return lambda x: x * x
        except:
            print("Could not parse operation")

    def _test(self):
        try:
            divisible = int(re.search(r'(?m)^  Test: divisible by (\d+)$', self.string).group(1))
            true_case = int(re.search(r'(?m)^    If true: throw to monkey (\d+)$', self.string).group(1))
            false_case = int(re.search(r'(?m)^    If false: throw to monkey (\d+)$',self.string).group(1))
            return lambda x: true_case if x % divisible == 0 else false_case
        except:
            print("Could not parse test")

    def parse(self):
        '''Returns a tuple of (id, Monkey object)'''
        id = self._monkeyId()
        monkey = Monkey(self._startingItems(), self._operation(), self._test())
        return (id, monkey)
            

def solve(troop):
    list = troop.counts()
    list.sort(reverse=True)
    return list[0] * list[1]
    
if __name__=='__main__':
    if len(sys.argv) > 1:
        fname = sys.argv[1]
    else:
        fname = input("Enter filename: ")

    with open(fname, "r") as f:
        raw = f.read()

    input = raw.split("\n\n")

    t = Troop()
    for m in input:
        m_p = Parser(m)
        vals = m_p.parse()
        t.add(vals[0], vals[1])

    t2 = copy.deepcopy(t)
        
    PART1 = True
    for i in range(20):
        t.round()

    print(solve(t))

    ##Can't be bothered rewriting all this
    PART1 = False
    for i in range(1000):
        t2.round()
    print(t2.counts())
