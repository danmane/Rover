#!/usr/bin/env ipython
# Rover v-1 for speed testing purposes
# v1 (o(n) algo): 135s
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

	#fname = 'distribution_' + str(size) + '_'+ str(nRepeats) + '.csv'
	#np.savetxt(fname, results, delimiter=',')

	return results

def buildCurve(minn=2, maxx=50):
	# build the data that shows # of steps vs grid size
	diff = maxx-minn+1
	results = np.zeros(diff)
	for i in xrange(diff):
		results[i] = avgRun(i+minn)

	np.savetxt('steps.csv',results,delimiter=',')
	return results

def stdtest():
	return runDistribution(size=20, nRepeats=10000)

def main():
	print stdtest()

if __name__ == '__main__':
	main()

