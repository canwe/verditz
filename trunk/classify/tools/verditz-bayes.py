#!/usr/bin/python
import sys
import re
from collections import defaultdict

words = defaultdict(int)
for word in re.compile("[a-zA-Z]+").findall(sys.stdin.read()):
  words[word] += 1

keys = words.keys()
keys.sort(lambda a,b: cmp(words[a],words[b]))
keys.reverse()

for key in keys:
  print "%s,%s"%(key,words[key])
