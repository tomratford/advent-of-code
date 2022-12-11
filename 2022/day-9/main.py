#!/usr/bin/env python3

import sys ## argument handling
from math import sqrt
from copy import deepcopy

def mv2zero(x):
    '''helper to avoid repition, iterates toward 0'''
    if x == 0:
        return x
    elif x > 0:
        return x - 1
    else:
        return x + 1

def add2(x,y):
    '''helper to add two coordinates together'''
    return [x[0] + y[0], x[1] + y[1]]

def minus2(x,y):
    '''helper to subtract two coordinates together'''
    return [x[0] - y[0], x[1] - y[1]]

class Tail:
    '''
    Attributes:
      parent - the `head` of the tail, this can be another tail
      pos - position relative to parent object
    '''
    def __init__(self, parent):
        self.parent = parent
        self.pos = [0,0] # list over tuple because lists can be reassigned at an index

    def getPos(self):
        '''
        get the actual position in the 2D space
        gets the parent position and returns back it's position as a tuple
        '''
        parent_coord = self.parent.getPos() # get parent positon
        my_pos = add2(self.pos, parent_coord) # work out new position
        return (my_pos[0], my_pos[1])

    def getDist(self):
        '''get the distance from the parent object'''
        return sqrt(self.pos[0]**2 + self.pos[1]**2)

    def move(self, direction):
        '''
        sends a move command up to the parent, and then checks if it should also move.
        
        returns back the coordinates of how much it moved to update the childs relative position'''
        moves = self.parent.move(direction)
        self.pos = add2(self.pos, moves)
        cur_pos = deepcopy(self.pos) #ensure we copy the value and not the reference

        if self.getDist() >= 2:
            self.pos[0] = mv2zero(self.pos[0])
            self.pos[1] = mv2zero(self.pos[1])
            return minus2(cur_pos, self.pos) # if we move then we must update our child
        else:
            return [0,0]
            

class Head:
    '''
    Attributes:
      pos: The actual position on the board
    '''
    def __init__(self):
        self.pos = [0,0]

    def getPos(self):
        '''get the actual position in the 2D space'''
        return self.pos

    def move(self, d):
        '''returns back how much it will have moved relative to child'''
        if d=='R':
            self.pos[0] += 1
            return [-1, 0]
        elif d=='U':
            self.pos[1] += 1
            return [0, -1]
        elif d=='L':
            self.pos[0] -= 1
            return [1, 0]
        elif d=='D':
            self.pos[1] -= 1
            return [0, 1]
        else:
            print(f'BAD INPUT: {f}')

if __name__=='__main__':
    if len(sys.argv) > 1:
        fname = sys.argv[1]
    else:
        fname = input("Enter filename: ")

    # part 1
    r = Tail(Head())

    tailPositions = set()
    with open(fname, 'r') as f:
        for line in f:
            k, v = line.strip().split(' ',1)
            for i in range(int(v)):
                r.move(k)
                tailPositions.add(r.getPos())
    print(len(tailPositions))

    # part 2
    r2 = Tail(Tail(Tail(Tail(Tail(Tail(Tail(Tail(Tail(Head())))))))))

    tailPositions2 = set()
    with open(fname, 'r') as f:
        for line in f:
            k, v = line.strip().split(' ',1)
            for i in range(int(v)):
                r2.move(k)
                tailPositions2.add(r2.getPos())
    print(len(tailPositions2))

