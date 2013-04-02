#!/usr/bin/python3

timings = []

with open('speedup.data') as datafile:
    for line in datafile:
        cores, t = [float(s) for s in line.split()]
        timings.append((int(cores),t,))
        
times = [t for c, t in timings]

speedups = [x / y for x, y in zip(times[:-1], times[1:])]

for tm, su in zip(timings, [None] + speedups):
    c,t = tm
    print(c, t, su)
