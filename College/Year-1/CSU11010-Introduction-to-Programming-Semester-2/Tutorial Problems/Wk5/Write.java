/*
 * Write a method, toDocument, that repeatedly prompts the user for a word from the console, and writes those words to a file, with a space separating each word, until null/EOF is reached.
The method should just take in a FileWriter which will write to a file. Hint: You can use readLine(). You are required to makes use of BufferedReader and BufferedredWriter classes for improved performance.
Write a main method which takes in an argument denoting which file to write to. Then, it should create an appropriate writer (which writes to the file that was specified) and pass that in to toDocument. Hint: Take a quick look at the Java InputStreamReader API.
 */
import java.io.*;
import java.util.Scanner;

public class Write
{
    public static void toDocument(FileWriter writer) throws Exception
    {

        BufferedWriter buffer = new BufferedWriter(writer);  	
	InputStreamReader r = new InputStreamReader(System.in);    
        BufferedReader br = new BufferedReader(r);   

	System.out.print("Enter next word: ");
	
	String word;
	while ((word = br.readLine()) != null && !(word.equals("done")))
	{ 
	    buffer.write(word);
	    buffer.write(" ");
	    System.out.print("Enter next word: ");
	}
	buffer.close();
    }

    public static void main(String[] args) throws Exception
    {
	FileWriter writer = new FileWriter(args[0]);  
        toDocument(writer);
    }
}