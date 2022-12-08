/*
 * Use the BufferedReader class to read the contents  of a file (out.txt) and print the output to the  standard output. You are required to make use of the  FileReader class.
 */
import java.io.*;  
public class BufferedReaderExample
{  
    public static void main(String args[]) throws Exception
    {    
        FileReader fr = new FileReader("out.txt");    
        BufferedReader br = new BufferedReader(fr);    
  
        int i;    
        while((i=br.read())!=-1)
        {  
            System.out.print((char)i);  
        }

        br.close();    
        fr.close();    
    }    
} 