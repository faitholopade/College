/*
 * A Singly Linked List is the most basic types of linked list where every node holds some data and reference to next node. 
Your task is to implement a singly linked list with the following functionality:
The add operation appends a new node to the end of the list
The insert operation inserts a new node at the specified position in the list
The get operation returns the node at the specified position in the list
The remove operation deletes the node at the specified position in the list
The size operation returns the current size of the list
You have been provided with a Java class (Node.java) that implements a basic node structure and a skeleton of the Java class (LinkedList.java) that implements a singly linked list to store integers with the appropriate constructor. You have also been provided with the add() function
Extend the LinkedList class to implement the methods insert(), get(), remove(), and size(). Also create a class LLTest to test the functionality of your code.
 */
public class LLTest
{

	public static void main(String[] args)
	{
		LinkedList lnklist = new LinkedList();
		lnklist.add(1);
		lnklist.add(2);
		lnklist.add(3);
		lnklist.add(4);
		lnklist.add(5);

		System.out.println("All the Nodes available in the List : ");
		DisplayList(lnklist);

		lnklist.insert(10, 3);
		System.out.println("All the Nodes available in the List : ");
		DisplayList(lnklist);

		lnklist.remove(1);
		lnklist.remove(4);
		System.out.println("All the Nodes available in the List : ");
		DisplayList(lnklist);

		Node tmp = lnklist.get(4);
		System.out.println("Contents of node are "+ tmp.getData());

		System.out.println("Number Of Nodes in the List: " + lnklist.size());
	}

	public static void DisplayList(LinkedList ll)
	{
		for (int i = 1; i <= ll.size(); i++) {
			System.out.println(i + " : " + ll.get(i).getData());
		}
	}
}