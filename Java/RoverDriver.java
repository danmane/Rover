class RoverDriver {
	public static int traverse(int size){
		Rover newRover;
		int steps, target;

		newRover = new Rover(size, size);
		steps = 0;
		target = size * size;

		while (newRover.getNumVisited() < target) {
			newRover.randomMove();
			steps++;
		}
		return steps;
	}

	public static int[] traversals(int size, int nTraversals){
		int[] traversalRecord;
		traversalRecord = new int[nTraversals];
		for (int i=0; i<nTraversals; i++){
			traversalRecord[i] = traverse(size);
		}

		return traversalRecord;
	}
	public static void main(String[] args) {
		int[] results;
		int gridSize, nRepeats;
		int sum=0;

		if (args.length >= 2){
			gridSize = Integer.parseInt(args[0]);
			nRepeats = Integer.parseInt(args[1]);
		}
		else {
			gridSize=20;
			nRepeats=10000;
		}

		System.out.println("Using size=" + gridSize + ", repeats = " + nRepeats);

		results = traversals(gridSize,nRepeats);
		for (int res : results) {
			sum += res;
		}
		System.out.println(sum);
	}
}