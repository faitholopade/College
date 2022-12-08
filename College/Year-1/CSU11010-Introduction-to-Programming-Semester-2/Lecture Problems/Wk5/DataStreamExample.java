/*
 * read all  bytes in the file. Assume the file out.txt contains  the string “Welcome to Java”.​
 */
import java.io.FileInputStream;
 
public class DataStreamExample
{  
    public static void main(String args[])
    {    
        try
        {    
            FileInputStream fin = new  FileInputStream("out.txt");    
            int i =0;
            while((i = fin.read()) != -1) // as long as i dont get to end of file (-1) it will read in each byte from file
            	{
            		System.out.print((char)i); 
            	}   
            fin.close();    
         }catch(Exception e){System.err.println(e);}    
    }    
} 