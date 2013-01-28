class Helper{
	public static boolean boolAnd(boolean[] boolArr){
		boolean rval;
		rval = true;
		for (boolean it : boolArr){
			rval = rval && it;
		}
		return rval;
	}

	public static boolean report(boolean val, String testName){
		if (val){
			System.out.println(testName + " test passed!");
		}
		else {
			System.out.println(testName + " test failed! Oh no!");
		}
		return val;
	}

	public static boolean testBoolAnd(){
		boolean[] testArr1, testArr2, testArr3;
		boolean t1, t2, t3;
		testArr1 = new boolean[1000];
		testArr2 = new boolean[1000];
		testArr3 = new boolean[1000];
		for (int i=0; i<1000; i++){
			testArr1[i] = true;
			testArr2[i] = true;
			testArr3[i] = false;
		}
		testArr2[0] = false;
		testArr3[999] = true;

		t1 = boolAnd(testArr1) == true;
		t2 = boolAnd(testArr2) == false;
		t3 = boolAnd(testArr3) == false;

		return t1 && t2 && t3;

	}

	public static boolean runHelperTests(){
		boolean t1;
		t1 = testBoolAnd();
		return t1;
	}

	public static void main(String[] args) {
		if (runHelperTests()){
			System.out.println("Helper tests passed!");

		}
		else {
			System.out.println("Helper tests failed!");
		}
	}
}