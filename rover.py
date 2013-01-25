#!/usr/bin/env ipython
from __future__ import division
import random, numpy as np

class Rover:
	# The rover moves around on an X by Y grid
	# It can always move in any of 4 directions:
	# Up, Down, Left, Right

	def __init__(self, gridX, gridY):
		self.gridX, self.gridY = gridX, gridY
		self.x = 0
		self.y = 0
		self.visited = set((0,0))

	def randomMove(self):
		direction = random.choice(['U','D','L','R'])
		self.moveDir(direction)

	def moveDir(self, direction):
		if direction == 'U':
			self.y += 1
		elif direction == 'D':
			self.y -= 1
		elif direction == 'L':
			self.x -= 1
		elif direction=='R':
			self.x += 1

		self.x %= self.gridX
		self.y %= self.gridY
		self.visited.add( (self.x,self.y) )

	def nVisited(self):
		return len(self.visited)

def runRover(gridX, gridY):
	steps = 0
	myRover = Rover(gridX, gridY)
	target = gridX * gridY

	while myRover.nVisited() < target:
		myRover.randomMove()
		steps += 1

	return steps

def avgRun(size, nRepeats=100):
	total = 0
	for i in xrange(nRepeats):
		total += runRover(size,size)

	return total / nRepeats

def runDistribution(size, nRepeats=1000):
	# runs the rover 
	results = np.zeros(nRepeats)
	for i in xrange(nRepeats):
		results[i] = runRover(size,size)

	fname = 'distribution_' + str(size) + '_'+ str(nRepeats) + '.csv'
	np.savetxt(fname, results, delimiter=',')

	bins = min(100,steps//10)
	hist = np.histogram(results, bins)
	np.savetxt('hist_'+fname, hist, delimiter=',')

	return results, hist

def buildCurve(minn=2, maxx=50):
	# build the data that shows # of steps vs grid size
	diff = maxx-minn+1
	results = np.zeros(diff)
	for i in xrange(diff):
		results[i] = avgRun(i+minn)

	np.savetxt('steps.csv',results,delimiter=',')
	return results

def sampleProbabilityDistribution(data,samplePts=1000):
	#Work in progress - incomplete
	data = sorted(data)
	length = len(data)
	
	lowBound = data[0]
	highBound = data[-1]
	interval = highBound - lowBound
	stepSize = interval // samplePts 
	# an integer - beware rounding issues

	ptcount = np.zeros(samplePts)
	pdist = np.matlib.zeros(samplePts,2)
	totalPts = 0

	for i in xrange(samplePts):
		lb = stepSize*i + lowBound
		# use mult to avoid rounding issues
		hb = lb + stepSize
		inStep = [d in data if (lb =< d =< hb)]
		pdist[i,1] = inStep
		totalPts += inStep

	for i in xrange(samplePts):
		pdist[i,0] = i / samplePts
		pdist[i,1] = float(ptcount[i]) / totalPts

	return pdist

def makeCumulativeDistribution(data):
	# untested
	data = sorted(data)
	length = len(data)
	pd = np.zeros(length,2)
	for i in xrange(length):
		pd[i,0] = float(i) / length
		# how far into the distribution are we
		pd[i,1] = data[i]

	return data

def main():
	print runDistribution(size=50)


if __name__ == '__main__':
	main()

