/*
 * 1. readDictionary
- I have the correct method definition [Mark out of 5: 5]
- Comment: I have the correct method definition
- My method reads the words from the "words.txt" file. [Mark out of 5: 5]
- Comment: My method reads the words from the "words.txt" file
- It returns the contents from "words.txt" in a String array or an ArrayList. [Mark out of 5: 5]
- Comment: It returns the contents from "words.txt" in a String array or an ArrayList. 

2. readWordList
- I have the correct method definition [Mark out of 5: 5]
- Comment: I have the correct method definition
- My method reads the words provided (which are separated by commas, saves them to an array or ArrayList of String references and returns it. [Mark out of 5:5]
- Comment: My method reads the words provided (which are separated by commas, saves them to an array or ArrayList of String references and returns it

3. isUniqueList
- I have the correct method definition [Mark out of 5: 5]
- Comment: I have the correct method definition
- My method compares each word in the array with the rest of the words in the list. [Mark out of 5:5]
- Comment: My method compares each word in the array with the rest of the words in the list.
- Exits the loop when a non-unique word is found. [Mark out of 5: 5]
- Comment: Exits the loop when a non-unique word is found.
- Returns true is all the words are unique and false otherwise. [Mark out of 5: 5]
- Comment: Returns true is all the words are unique and false otherwise. 

4. isEnglishWord
- I have the correct method definition [Mark out of 5: 5]
- Comment:  I have the correct method definition
- My method uses the binarySearch method in Arrays library class. [Mark out of 3: 3]
- Comment: I used an ArrayList so I had to use the collections library
- Returns true if the binarySearch method return a value >= 0, otherwise false is returned. [Mark out of 2: 2]
- Comment: Returns true if the binarySearch method return a value >= 0, otherwise false is returned.

5. isDifferentByOne
- I have the correct method definition [Mark out of 5: 5]
- Comment: I have the correct method definition
- My method loops through the length of a words comparing characters at the same position in both words searching for one difference. [Mark out of 10:10]
- Comment: My method loops through the length of a words comparing characters at the same position in both words searching for one difference.

6. isWordChain
- I have the correct method definition [Mark out of 5:5]
- Comment:  I have the correct method definition
- My method calls isUniqueList, isEnglishWord and isDifferentByOne methods and prints the appropriate message [Mark out of 10: 10]
- Comment: My method calls isUniqueList, isEnglishWord and isDifferentByOne methods and prints the appropriate message

7. main
- Reads all the words from file words.txt into an array or an ArrayList using the any of teh Java.IO classes covered in lectures [Mark out of 10: 1-]
- Comment: Reads all the words from file words.txt into an array or an ArrayList using the any of teh Java.IO classes covered in lectures 
- Asks the user for input and calls isWordChain [Mark out of 5: 5]
- Comment: Asks the user for input and calls isWordChain

 Total Mark out of 100 (Add all the previous marks): 100
 */

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.InputMismatchException;
import java.util.Scanner;

public class WordLinks {

	public static String[] readDictionary()
	{
		ArrayList<String> dictionary = new ArrayList<String>();
		FileReader file = null;
		try {
			file = new FileReader("C:\\Users\\faith\\Downloads\\words.txt");
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		BufferedReader reader = new BufferedReader(file);
		String word = null;
		try {
			word = reader.readLine();
		} catch (IOException e) {
			e.printStackTrace();
		}
		while(word != null)
		{
			dictionary.add(word);
			try {
				word = reader.readLine();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return dictionary.toArray(new String[0]);
	}

	public static String[] readWordList(String words)
	{
		ArrayList<String> seperatedWords = new ArrayList<String>();
		String[] vals = words.split(", ");
		Collections.addAll(seperatedWords, vals);	
		return seperatedWords.toArray(new String[0]);
	}

	public static boolean isUnqiueList(String[] wordChain)
	{
		boolean unique = true;
		for(int i = 0; i < wordChain.length; i++)
		{
			String tmp = wordChain[i];
			for(int j = i + 1; j < wordChain.length; j++)
			{
				if(tmp == wordChain[j])
				{
					unique = false;
					return unique;
				}
			}
		}
		return unique;
	}

	public static boolean isEnglishWord(String word)
	{
		return Arrays.binarySearch(dictionary, word) >= 0;
	}	

	public static boolean isDifferentByOne(String one, String two)
	{
		int diff = 1;
		boolean isDiff = false;
		if(one.length() == two.length())
		{
			for(int i = 0; i< one.length(); i++)
			{
				if(one.charAt(i)!= two.charAt(i))
				{
					diff--;
					if(diff < 0)
					{
						isDiff = false;
					}
					else
					{
						isDiff = true;
					}
				}
			}
		}
		return isDiff;
	}

	public static boolean isWordChain(String[] wordChain)
	{
		if(!isUnqiueList(wordChain))
		{
			return false;
		}

		for(final String word: wordChain)
		{
			if(!isEnglishWord(word))
			{
				return false;
			}
		}

		String tmp = wordChain[0];
		for(int i = 1; i < wordChain.length; i++)
		{
			if(!isDifferentByOne(tmp, wordChain[i]))
			{
				return false;
			}
		}

		return true;
	}

	public static String[] dictionary;

	public static void main(String[] args) {

		System.out.println("WELCOME");

		dictionary = readDictionary();
		boolean quit = false;
		try {
			while(!quit)
			{
				System.out.print("Enter a comma separated list of words (or an empty list to quit): ");
				Scanner inputScanner = new Scanner(System.in);
				String words = inputScanner.nextLine();
				if(words.isEmpty())
				{
					quit = true;
					System.out.println("YOU HAVE QUIT, GOODBYE");
				}
				else if(words != null)
				{
					String[] inputWords = readWordList(words);
					System.out.println(isWordChain(inputWords) ? "Valid chain of words from Lewis Carroll's word-links game" 
							:  "Not a valid chain of words from Lewis Carroll's word-links game" );
				}
			}
		} catch (InputMismatchException exception) {
			System.out.println("ERROR: INVALID INPUT");
		}

	}
}

