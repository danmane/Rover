public enum Direction {
	UP,DOWN,LEFT,RIGHT;
	public static Direction getRandomDirection(){
		return values()[(int) (Math.random() * values().length)]; //randomly chosen
	}
	public static Direction int2Direction(int i){
		return values()[i];
	}
};
