import java.util.Scanner;

public class StackTest
{
	public static void main(String[] args)
	{
		int element, pos;
		Boolean success;

		// Create a stack object
		Stack myStack = new Stack();

		// Initialize the stack
		System.out.print("Please enter an element and terminate by entering 0: ");
		Scanner input = new Scanner(System.in);
		element = input.nextInt();
		while (element !=0)
		{
			myStack.push(element);
			System.out.print("Please enter an element and terminate by entering 0: ");
			element = input.nextInt();
		}

		// Print the elements in the stack
		myStack.printStack();

		// Pop: Remove an element from the top of the stack
		myStack.pop();

		// Peek: Print the last element in the stack
		element = myStack.peek();
		if (element != -1)
			System.out.println("Peek: " + element);

		// Search: Search for a specific element in the stack
		System.out.print("Please enter the element to search in the stack: ");
		input = new Scanner(System.in);
		element = input.nextInt();
		pos = myStack.search(element);
		if (pos == -1)
			System.out.println("Search: Element does not exist in the stack");
		else
			System.out.println("Search: Element at position " + pos);

		// Move: Move an element to the top of the stack without removing it from the stack
		System.out.print("Please enter the element to move to the top of the stack: ");
		input = new Scanner(System.in);
		element = input.nextInt();
		success = myStack.move(element);
		if(success)
		{
			System.out.printf("Move: ");
			myStack.printStack();
		}
	}
}