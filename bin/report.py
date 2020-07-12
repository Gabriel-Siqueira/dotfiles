#!/usr/bin/env python
# -*- coding: utf-8 -*-
import os
import sys
import re
from datetime import date

# Q,label,question
def long_question(str):
    print(str.rstrip(),"?",sep="")
    return input("> ")

# B,label,question
def bool_question(str):
    print(str.rstrip(),"? (y/n)",sep="")
    while True:
        ans = input("> ")
        if ans == "y" or ans == "Y":
            return True
        if ans == "n" or ans == "N":
            return False

# I,label,question
def int_question(str):
    print(str.rstrip(),"?",sep="")
    while True:
        ans = input("> ")
        if ans.isdigit():
            return int(ans)

# O,label,flag,default,...recursive
def optional(flag,default,c,*args):
    if eval(flag.rstrip()):
        return select(c,*args)
    else:
        return default

# R,label,str
def reminder(str):
    print(str.rstrip())
    input()

def read_next(fin,anss):
    line = fin.readline()
    if line == "" or line[0] == "%":
        return line
    return re.sub("\$\w+", lambda x: str(anss[x.group(0)[1:]]), line)


def select(c,*args):
    if c == "Q":
        return long_question(*args)
    elif c == "B":
        return bool_question(*args)
    elif c == "I":
        return int_question(*args)
    elif c == "O":
        return optional(*args)
    else:
        return None

def consolidate(anss,fin,path):
    with open(path, "a") as fout:
        while True:
            line = read_next(fin, anss)
            if line[0] == "@":
                return
            print(line.rstrip(),file=fout)

def main():
    anss = {}
    with open(sys.argv[1], "r") as fin:
        path = fin.readline().rstrip() + date.today().strftime("%Y-%m-%d") + ".md"
        print(path)
        while True:
            line = read_next(fin, anss)
            if line == "":
                break
            line = line.split(",")
            if line[0] == "C":
                consolidate(anss,fin,path)
            elif line[0] == "R":
                reminder(line[1])
            else:
                ans = select(line[0],*line[2:])
                if ans != None:
                    anss[line[1]] = ans
    os.system("vim " + path)

if __name__ == "__main__":
  main()
