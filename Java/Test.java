class Test{
	public static boolean runTests(){
		return testRoverAttributes() && testRoverMovement();
	}
	private static boolean testRoverAttributes(){
		// Currently only tests that it maintains grid & location coordinates
		// Doesn't include visited-counts
		Rover testRover;
		int gx, gy;
		gx = 3;
		gy = 4;
		testRover = new Rover(gx,gy);
		assert testRover.getX()     == 0;
		assert testRover.getY()     == 0;
		assert testRover.getGridX() == 3;
		assert testRover.getGridY() == 4;
		return true;
	}

	private static boolean testRoverMovement(){
		Rover testRover;
		int gx, gy;
		boolean t1, t2, t3, t4, t5, t6, t7, t8;
		gx = 3; gy = 4;
		testRover = new Rover(gx,gy);

		// Test basic movement
		testRover.moveRover(Direction.UP);
		t1 = testRover.getX() == 0;
		t2 = testRover.getY() == 1;
		assert t1 && t2;

		testRover.moveRover(Direction.RIGHT);
		t3 = testRover.getX() == 1;
		t4 = testRover.getY() == 1;
		assert t3 && t4;

		testRover.moveRover(Direction.DOWN);
		t5 = testRover.getX() == 1;
		t6 = testRover.getY() == 0;
		assert t5 && t6;

		// Test wrap-around
		testRover.moveRover(Direction.DOWN);
		t7 = testRover.getY() == 3;
		assert t7;

		testRover.moveRover(Direction.RIGHT);
		testRover.moveRover(Direction.RIGHT);
		testRover.moveRover(Direction.RIGHT);
		t8 = testRover.getX() == 1;
		assert t8; 
		return (t1 && t2 && t3 && t4 && t5 && t6 && t7 && t8);
	}

	private static boolean testNumVisited(){
		Rover testRover;
		boolean t1, t2, t3, t4;
		testRover = new Rover(3,3);
		t1 = testRover.getNumVisited() == 1;
		assert t1;

		return t1;
	}

	public static void main(String[] args){
		boolean testPassed;
		testPassed = Test.runTests();
		if (testPassed){
			System.out.println("Congrats, the tests passed!");
		}
		else {
			System.out.println("Oh no, the tests failed!");
		}
	}

}