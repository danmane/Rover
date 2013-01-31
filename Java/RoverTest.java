class RoverTest{
	public static boolean runTests(){
		boolean[] t;
		t = new boolean[3];
		t[0] = Helper.report(testRoverAttributes(), "RoverAttributes");
		t[1] = Helper.report(testRoverMovement()  , "RoverMovement"  );
		t[2] = Helper.report(testNumVisited()     , "NumVisited"     );
		return Helper.boolAnd(t);
	}
	private static boolean testRoverAttributes(){
		// Currently only tests that it maintains grid & location coordinates
		// Doesn't include visited-counts
		Rover testRover;
		boolean[] t;
		boolean passed;
		t = new boolean[4];

		testRover = new Rover(3,4);
		t[0] = testRover.getX()     == 0;
		t[1] = testRover.getY()     == 0;
		t[2] = testRover.getGridX() == 3;
		t[3] = testRover.getGridY() == 4;

		passed = Helper.boolAnd(t);
		return passed;
	}

	private static boolean testRoverMovement(){
		Rover testRover;
		int gx, gy;
		boolean t[];
		t = new boolean[8];

		testRover = new Rover(3,4);

		// Test basic movement
		testRover.moveRover(Direction.UP);
		t[0] = testRover.getX() == 0;
		t[1] = testRover.getY() == 1;
		assert t[0] && t[1];

		testRover.moveRover(Direction.RIGHT);
		t[2] = testRover.getX() == 1;
		t[3] = testRover.getY() == 1;
		assert t[2] && t[3];

		testRover.moveRover(Direction.DOWN);
		t[4] = testRover.getX() == 1;
		t[5] = testRover.getY() == 0;
		assert t[4] && t[5];

		// Test wrap-around
		testRover.moveRover(Direction.DOWN);
		t[6] = testRover.getY() == 3;
		assert t[6];

		testRover.moveRover(Direction.RIGHT);
		testRover.moveRover(Direction.RIGHT);
		testRover.moveRover(Direction.RIGHT);
		t[7] = testRover.getX() == 1;
		assert t[7]; 
		return Helper.boolAnd(t);
	}

	private static boolean testNumVisited(){
		Rover testRover;
		boolean[] t;
		boolean passed;
		t = new boolean[5];

		testRover = new Rover(3,3); //0,0
		t[0] = testRover.getNumVisited() == 1;
		assert t[0];

		testRover.moveRover(Direction.UP); //0,1
		t[1] = testRover.getNumVisited() == 2;
		testRover.moveRover(Direction.DOWN); //0,0
		t[2] = testRover.getNumVisited() == 2;
		assert t[1] && t[2];

		testRover.moveRover(Direction.RIGHT); //1,0
		testRover.moveRover(Direction.UP); //1,1
		testRover.moveRover(Direction.UP); //1,2
		testRover.moveRover(Direction.UP); //1,0
		t[3] = testRover.getNumVisited() == 5;
		assert t[3];
		testRover.moveRover(Direction.LEFT);  // 0,0
		testRover.moveRover(Direction.LEFT);  // 2,0
		testRover.moveRover(Direction.DOWN); // 2,2
		testRover.moveRover(Direction.DOWN); // 2,1
		testRover.moveRover(Direction.DOWN); // 2,0
		testRover.moveRover(Direction.RIGHT); //0,0
		testRover.moveRover(Direction.DOWN); //1,0
		testRover.moveRover(Direction.RIGHT); //1,1
		t[4] = testRover.getNumVisited() == 9;
		assert t[4] : testRover.getNumVisited();
		passed = Helper.boolAnd(t);
		return passed;
	}

	// public static boolean boolAnd(boolean[] arr){
	// 	boolean rval;
	// 	rval = True;
	// 	for (boolean it : arr){
	// 		rval = rval && it;
	// 	}
	// }

	public static void main(String[] args){
		boolean testPassed;
		testPassed = RoverTest.runTests();
		if (testPassed){
			System.out.println("Congrats, the tests passed!");
		}
		else {
			System.out.println("Oh no, the tests failed!");
		}
		System.out.println(Direction.getRandomDirection());
	}

}