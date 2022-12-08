import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class FileIO {
	public String findLongestWord() {
		String longestWord = "";
		Scanner inputScanner = null;
		try {
			inputScanner = new Scanner(new File("example.txt"));
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}

		while (inputScanner.hasNext()) {
			String currentWord = inputScanner.next();
			if (currentWord.length() > longestWord.length()) {
				longestWord = currentWord;
				System.out.println(longestWord);
			}
			return longestWord;
		}
		return null;
	}
	
	public String[] fileToArray() {
		List<String> textList = new ArrayList<String>();

		FileReader fileReader = null;
		try {
			fileReader = new FileReader("example.txt");
		} catch (FileNotFoundException e2) {
			e2.printStackTrace();
		}
		BufferedReader bufferedReader = new BufferedReader(fileReader);
		boolean endOfFile = false;
		while(!endOfFile)
		{
			String textLine = null;
			try {
				textLine = bufferedReader.readLine();
			} catch (IOException e1) {
				e1.printStackTrace();
			}

			while (textLine != null) {
				textList.add(textLine);
				try {
					textLine = bufferedReader.readLine();
				} catch (IOException e) {
					e.printStackTrace();
					endOfFile = true;
				}
			}

			try {
				bufferedReader.close();
				fileReader.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		String[] textArray = textList.toArray(new String[0]);
		System.out.print("Text Array: " + textArray);
		return textArray;
	}
}
