import java.io.*;

class RandomMoveGen {
	private static int[] getRandomArray(int size){
		int[] array;
		array = new int[size];
		for (int i=0; i<size; i++){
			array[i] = (int) (Math.random() * 4);
		}
		return array;
	}

	public static void main(String[] args){
		int randomArraySize;
		int[] randomArray;
		PrintWriter result;
		randomArraySize = 100007;
		randomArray = getRandomArray(randomArraySize);
		try {
			result = new PrintWriter(new FileWriter("../Data/random.dat"));
			for (int i=0; i<randomArraySize; i++){			
				result.println(randomArray[i]);
			}
			result.close();
		}
		catch (IOException e){
			System.out.println("File IO Error! Error: " + e);
			return;
		}

	}
}