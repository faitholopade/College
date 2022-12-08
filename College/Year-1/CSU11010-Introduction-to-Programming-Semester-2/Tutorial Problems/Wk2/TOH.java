/*
 * The Tower of Hanoi is a mathematical puzzle where we have three rods and n disks. The objective of the puzzle is to move the entire stack to another rod, obeying the following simple rules:
1.Only one disk can be moved at a time
2.Each move consists of taking the upper disk from one of the stacks and placing it on top of another stack i.e. a disk can only be moved if it is the uppermost disk on a stack
3.No disk may be placed on top of a smaller disk
Write a Java program that asks the user to input the number of disks and implements a recursive function to implement your solution.
 */
import java.util.Scanner;

class TOH 
{ 
    static void towerOfHanoi(int n, char from_rod, char to_rod, char aux_rod) 
    { 
        if (n == 1) 
        { 
            System.out.println("Move disk 1 from rod " +  from_rod + " to rod " + to_rod); 
            return; 
        } 

        towerOfHanoi(n-1, from_rod, aux_rod, to_rod); 
        System.out.println("Move disk " + n + " from rod " +  from_rod + " to rod " + to_rod); 
        towerOfHanoi(n-1, aux_rod, to_rod, from_rod); 
    } 
      
    public static void main(String args[]) 
    { 
        Scanner input = new Scanner(System.in);
        System.out.print("Input the number of disks to move: ");
        int n = input.nextInt();

        towerOfHanoi(n, 'A', 'C', 'B');  // A, B and C are names of rods 
    } 
} 