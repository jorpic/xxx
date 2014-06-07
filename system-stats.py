#!/usr/bin/python

import sys
import numpy
import matplotlib.pyplot as plt


def parseEqs(fname):
  f = open(sys.argv[1], 'r')
  try:
    return [eval(l) for l in f.readlines()]
  finally:
    f.close()


def eqVars(eq):
  return set([x for xs in eq for x in xs])

def hist(msg, xs, rng):
  ys, bins = numpy.histogram(xs, bins=range(1,rng))
  print msg, list(ys)
  plt.bar((bins[:-1] + bins[1:]) / 2, ys, align='center')
  plt.yscale('log', nonposy='clip')
  plt.title(msg)


def main():
  eqs = parseEqs(sys.argv[1])

  lenEqs = len(eqs)
  print 'number of equations:', lenEqs

  plt.subplot(131)
  hist('monoms per eq', map(len, eqs), 10)
  plt.subplot(132)
  hist('vars per eq', map(lambda eq: len(eqVars(eq)), eqs), 30)
  plt.subplot(133)
  hist('eq degree', map(lambda eq: max(map(len,eq)), eqs), 30)
  plt.show()

  varUsage = {}
  for i,eq in enumerate(eqs):
    for v in eqVars(eq):
      varUsage[v] = varUsage.setdefault(v,[]) + [i]
  print 'number of variables:', len(varUsage)

  print filter(lambda (i,v): len(v) == 1, varUsage.iteritems())
  hist('var usage', map(len, varUsage.itervalues()), 30)
  plt.show()








main()
