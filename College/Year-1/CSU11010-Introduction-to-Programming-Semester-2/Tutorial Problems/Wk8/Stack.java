/*
 * A Stack is a last in, first out (LIFO) linear data structure and can have any object as an element. It is characterized by three fundamental operations namely: push, pop and peek.
The push operation adds a new item (taken as a parameter) to the top of the stack. It extends the stack to accommodate the new item.
The pop operation removes an item from the top of the stack. A pop reveals previously concealed items or results in an empty stack. If the stack is empty when a pop operation is attempted, then an error condition is raised.
The peek operation returns the last item in a stack without removing it. If the stack is empty, then the return value is -1.
Create a Java class (Stack.java) that implements a LIFO stack to store integers with the appropriate accessor and mutator methods. Your code should store the stack elements in an ArrayList and should not make use of a stack class from a class library. Stack.java should implement the functions push(), pop() and peek().
Additionally, implement a method search (Object element) which determines whether an object exists in the stack. If the element is found, it returns the position of the element from the top of the stack. Else, it returns -1.
Also implement a method move (Object element) to move an element to the top of the stack without removing it from the stack. It should make use of the search() method to determine if the element exists in the stack.
Finally, implement a test application (StackTest.java) that demonstrates the correct operation of the stack functions. StackTest.java creates a Stack object, inserts the numbers from 1 to 10 into the stack using push(), and calls pop(), peek(), search() & move() passing in elements of your choice where applicable and printing the output. User input in not required in this program.
 */
import java.util.ArrayList;

public class Stack
{
	private ArrayList<Integer> stack;

	Stack()
	{
		stack = new ArrayList<Integer>();
	}

	public void push(int element)
	{
		stack.add(element);
	}

	public void pop()
	{
		if (stack.isEmpty())
			System.out.println("Stack is empty");

		stack.remove(0);
		System.out.printf("Pop: ");
		printStack();
	}

	public int peek()
	{
		int sizeOfStack;

		sizeOfStack = stack.size();

		if (sizeOfStack>=1)
			return(stack.get(sizeOfStack-1));

		return(-1);

	}

	public int search(int element)
	{
		return(stack.indexOf(element));
	}

	public Boolean move(int element)
	{
		int pos, tmp1, tmp2;
		pos = search(element);

		if (pos == -1)
		{
			System.out.println("Move: Element does not exist in the stack");
			return(false);
		}

		while (pos>0)
		{
			tmp1 = stack.get(pos);
			tmp2 = stack.get(pos-1);
			stack.set(pos-1, tmp1);
			stack.set(pos, tmp2);
			pos--;
		}

		return(true);
	}

	public void printStack()
	{
		for (Integer item: stack)
			System.out.printf("%d ", item);
		System.out.println();
	}
}
