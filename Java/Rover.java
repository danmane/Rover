class Rover {
	private int x;
	private int y;
	private int gridX;
	private int gridY;
	private int nVisited;
	private boolean[] hasVisited;

	public Rover (int gx, int gy){ // Constructor
		x = 0;
		y = 0;
		gridX = gx;
		gridY = gy;
		nVisited = 1;
		hasVisited = new boolean[gx*gy];
		hasVisited[0] = true;
		for (int i=1; i<(gx*gy); i++){
			hasVisited[i] = false;
		}

	}

	public void randomMove(){
		Direction d;
		d = Direction.getRandomDirection();
		moveRover(d);
	}

	public void moveRover (Direction d) {
		switch(d){
			case UP:
				y += 1;
				break;
			case DOWN:
				y -= 1;
				break;
			case LEFT:
				x -= 1;
				break;
			case RIGHT:
				x += 1;
				break;
		}
		x = (x + gridX) % gridX;
		y = (y + gridY) % gridY;
		updateVisited();
	}

	private void updateVisited (){
		int currentCoordinate;
		currentCoordinate = coord2idx(x, y);
		if (! hasVisited[currentCoordinate]){
			hasVisited[currentCoordinate] = true;
			nVisited++;
			assert nVisited <= gridX*gridY;
		}
	}

	public int getNumVisited(){
		return nVisited;
	}

	public int getX(){
		return x;
	}
	public int getY(){
		return y;
	}
	public int getGridX(){
		return gridX;
	}
	public int getGridY(){
		return gridY;
	}
	public int coord2idx(int xx, int yy){
		return xx + (yy * gridX);
	}

}



