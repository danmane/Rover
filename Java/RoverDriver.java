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
		results = traversals(100,10000);
		for (int res : results) {
			//System.out.println(res);
		}
	}
}