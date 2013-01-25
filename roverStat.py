import numpy as np
import numpy.matlib as matlib

def sampleProbabilityDistribution(data,samplePts=1000, overlap=5):
	#Work in progress - incomplete
	data = sorted(data)
	length = len(data)
	
	lowBound = data[0]
	highBound = data[-1]
	dataRange = highBound - lowBound
	stepSize = dataRange // samplePts
	intervalWidth = stepSize * overlap
	# an integer - beware rounding issues
	# We set the interval width to be (o) times winder than step size
	# As a consequence, each point is counted (o) times - keep this in mind

	ptcount = np.zeros(samplePts)
	pdist = matlib.zeros((samplePts,2))
	totalPts = 0

	for i in xrange(samplePts):
		lb = stepSize*i + lowBound
		# use mult to avoid rounding issues
		hb = lb + intervalWidth
		inInterval = [d for d in data if lb <= d <= hb]
		nInInterval = len(inInterval)
		ptcount[i] = nInInterval
		totalPts  += nInInterval

	for i in xrange(samplePts):
		pdist[i,0] = i / samplePts
		pdist[i,1] = float(ptcount[i]) / totalPts

	assert totalPts == length * overlap
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