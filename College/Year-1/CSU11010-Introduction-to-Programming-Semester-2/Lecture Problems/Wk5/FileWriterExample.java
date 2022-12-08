/*
 * Create a FileWriterExample class using the Java  FileWriter library class that writes a string to a file  without the need to convert it into a byte array.​

You can make use of the FileOutputStream class  that we created earlier.​
 */

import java.io.FileWriter;

public class FileWriterExample {

	public static void main(String[] args) {
		try
		{
			FileWriter fw = new FileWriter("out.txt");
			fw.write("Welcome to Java");
			fw.close();
		} catch(Exception e){System.err.println(e);}
			System.out.println("Success...");
		}

	}