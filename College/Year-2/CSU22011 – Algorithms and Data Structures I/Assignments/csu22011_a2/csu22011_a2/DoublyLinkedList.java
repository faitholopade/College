package csu22011_a2;

import java.util.Iterator;
import java.util.ListIterator;
import java.util.NoSuchElementException;

// -------------------------------------------------------------------------
/**
 *  This class contains the methods of Doubly Linked List.
 *
 *  @author  
 *  @version 11/10/22
 */


/**
 * Class DoublyLinkedList: implements a *generic* Doubly Linked List.
 * @param <T> This is a type parameter. T is used as a class name in the
 * definition of this class.
 *
 * When creating a new DoublyLinkedList, T should be instantiated with an
 * actual class name that extends the class Comparable.
 * Such classes include String and Integer.
 *
 * For example to create a new DoublyLinkedList class containing String data: 
 *    DoublyLinkedList<String> myStringList = new DoublyLinkedList<String>();
 *
 * The class offers a toString() method which returns a comma-separated sting of
 * all elements in the data structure.
 * 
 * This is a bare minimum class you would need to completely implement.
 * You can add additional methods to support your code. Each method will need
 * to be tested by your jUnit tests -- for simplicity in jUnit testing
 * introduce only public methods.
 */
class DoublyLinkedList<T extends Comparable<T>>
{

	/**
	 * private class DLLNode: implements a *generic* Doubly Linked List node.
	 */
	private class DLLNode
	{
		public final T data; // this field should never be updated. It gets its
		// value once from the constructor DLLNode.
		public DLLNode next;
		public DLLNode prev;

		/**
		 * Constructor
		 * @param theData : data of type T, to be stored in the node
		 * @param prevNode : the previous Node in the Doubly Linked List
		 * @param nextNode : the next Node in the Doubly Linked List
		 * @return DLLNode
		 */
		public DLLNode(T theData, DLLNode prevNode, DLLNode nextNode) 
		{
			data = theData;
			prev = prevNode;
			next = nextNode;
		}
	}

	// Fields head and tail point to the first and last nodes of the list.
	private DLLNode head, tail;

	/**
	 * Constructor of an empty DLL
	 * @return DoublyLinkedList
	 */
	public DoublyLinkedList() 
	{
		head = null;
		tail = null;
	}

	/**
	 * Tests if the doubly linked list is empty
	 * @return true if list is empty, and false otherwise
	 *
	 * Worst-case asymptotic running time cost: Theta(1)
	 *
	 * Justification: 
	 * - Code consists of one statement
	 * - This statement is a constant time operation and a single return statement 
	 * 	 hence Theta(1) running time cost
	 *  
	 */
	public boolean isEmpty()
	{
		return (head == null);
	}

	/**
	 * Inserts an element in the doubly linked list
	 * @param pos : The integer location at which the new data should be
	 *      inserted in the list. We assume that the first position in the list
	 *      is 0 (zero). If pos is less than 0 then add to the head of the list.
	 *      If pos is greater or equal to the size of the list then add the
	 *      element at the end of the list.
	 * @param data : The new data of class T that needs to be added to the list
	 * @return none
	 *
	 * Worst-case asymptotic running time cost: Theta(N)
	 *
	 * Justification:
	 * - Code consists of lines within the if, else if and else statement plus a return statement 
	 * 
	 * Lines in if statement
	 * - A call to isEmpty() which is Theta(1) as we calculated above 
	 * - Creating new node for doubly linked list and initializing head and tail are all constant time operations hence Theta(1) 
	 * running time cost for if statement 
	 * - In total this section of the method will run in Theta(1)
	 * 
	 * Lines in else if statement 
	 * - Inserting node at front of doubly linked list includes creating new node and moving pointers to point to new node and next
	 * node. These are are all constant time operations not dependent on length hence running cost is Theta(1)
	 * 
	 * Lines in else statement 
	 * - Here we have a for loop which iterates from the head of our list to the tail hence this operation will take N * Theta(1) as we
	 * must go through all nodes in our doubly linked list so in total we have a Theta(N) running time cost
	 * - In each iteration of the for loop the if condition will be checked, should if condition evaluate as true all operations 
	 * performed within this statement will take Theta(1) running time as they are all constant time operations 
	 * - Once the for loop ends we have Theta(1) instructions creating a new tail node 
	 * 
	 * Therefore this method will run in Theta(1) + Theta(1) + Theta(N) + Theta (1) = Theta(N) time in the worst case
	 * 	 
	 * 
	 * */
	public void insertBefore( int pos, T data ) 
	{
		if(isEmpty()) {
			DLLNode newNode = new DLLNode(data, null, null);
			head = newNode;
			tail = newNode;
		}
		else if(pos < 1){ //insert at front of dll
			DLLNode oldFirst = head;
			DLLNode firstNode = new DLLNode(data, null, oldFirst);
			oldFirst.prev = firstNode;
			head = firstNode;
		} 
		else {  // insert anywhere else in dll
			int i = 0;
			for (DLLNode currNode = head; currNode != null; currNode = currNode.next) {
				if (i == pos) {
					DLLNode newNode = new DLLNode(data, currNode.prev, currNode);
					currNode.prev.next = newNode;
					currNode.prev = newNode;
					return;
				}
				i++;
			}

			DLLNode oldTail = tail;
			DLLNode lastNode = new DLLNode(data, oldTail, null);
			oldTail.next = lastNode;
			tail = lastNode;
		}
		return;
	}

    /**
     * Returns the data stored at a particular position
     * @param pos : the position
     * @return the data at pos, if pos is within the bounds of the list, and null otherwise.
     *
     * Worst-case asymptotic running time cost: Theta(N)
     *
     * Justification:
     * Code consists of lines within if statement and else statement 
     * 
     * If statement 
     * - Here we make a call to the isEmpty() function which we have calculated to run in Theta(1)
     * we are also checking our pos variable which is another Theta(1) operation.
     * - We then have a single return statement which is Theta(1) 
     * - Hence in total by addition all lines within if statement run in Theta(1) running time 
     * 
     * Else statement
     * - First we have a single assignment statement which will take Theta(1) 
     * - We then have a for loop which will iterate over all elements of the DLL. Thus it will perform 
     * Theta(N) iterations 
     * - Each iteration  will involve the if condition which will run which is a constant time operation
     * thus the statements within this if statement cost Theta(1)
     * 
     * Therefore this method will run in Theta(1) + Theta(1) + Theta(N) + Theta(1) = Theta(N) time in the worst case 
     * 
     *
     */
	public T get(int pos) 
	{
		if(isEmpty() || pos < 0) {
			return null;
		}
		else {
			int i = 0;
			for (DLLNode currNode = head; currNode != null; currNode = currNode.next) {
				if (i == pos)
					return currNode.data;
				i++;
			}
		}
		return null;
	}

    /**
     * reference: https://www.geeksforgeeks.org/delete-a-node-in-a-doubly-linked-list/
     * Deletes the element of the list at position pos.
     * First element in the list has position 0. If pos points outside the
     * elements of the list then no modification happens to the list.
     * @param pos : the position to delete in the list.
     * @return true : on successful deletion, false : list has not been modified. 
     *
     * Worst-case asymptotic running time cost: Theta(N)
     *
     * Justification:
     * Code consists of lines within if statement and else statement 
     * 
     * If statement 
     * - Here we make a call to the isEmpty() function which we have calculated to run in Theta(1)
     * we are also checking our pos variable which is another Theta(1) operation.
     * - We then have a single return statement which is Theta(1) 
     * - Hence in total by addition all lines within if statement run in Theta(1) running time 
     * 
     * Else statement
     * - First we have a single assignment statement which will take Theta(1) 
     * - We then have a for loop which will iterate over all elements of the DLL. Thus it will perform 
     * Theta(N) iterations 
     * - Each iteration  will involve the if condition the if-else if-else if-if-else-else conditions
     * and branching which are constant time operations Thus these cost Theta(1) time
     * 
     * Therefore this method will run in Theta(1) + Theta(1) + Theta(N) + Theta(1) = Theta(N) time in the worst case 
     * 
     *
     */
	//currNode.next == null && currNode.prev == null
	public boolean deleteAt(int pos) 
	{
		if(isEmpty() || pos < 0) {
			return false;
		}
		else {
			int i = 0;
			for (DLLNode currNode = head; currNode != null; currNode = currNode.next) {
				if (pos == 0 && head == tail) { //only one node
					head = null;
					tail = null;
					return true;
				} else if (pos == 0) {  //head
					head = head.next;
					head.prev = null;
					return true;
				} else if (i == pos) {  
					if (currNode.next != null) { 
						currNode.next.prev = currNode.prev;
						currNode.prev.next = currNode.next;
						return true;
					} else { //tail
						tail.prev.next = null;
						tail = tail.prev;
						return true;
					}
				} else {
					i++;
				}
			}
		}
		return false;
	}

    /**
     * Reverses the list.
     * If the list contains "A", "B", "C", "D" before the method is called
     * Then it should contain "D", "C", "B", "A" after it returns.
     *
     * Worst-case asymptotic running time cost: Theta(N)
     *
     * Justification:
     * Code consists of lines within if statement 
     * 
     * If statement 
     * - Here we make a call to the isEmpty() function which we have calculated to run in Theta(1)
     * we are also checking our head node which is another Theta(1) operation.
     * - We then perform assignment operations before our while loop which run in constant time Theta(1)
     * - We then have a while loop which will iterate from our current node to the end of our list meaning 
     * it depends on the length of the list and so will run in Theta(N)
     * - Finally we have a simple if statement to place our head node this is a constant time operation Theta(1)
     * 
     * Therefore this method will run in Theta(1) + Theta(1) + Theta(1) + Theta(N) + Theta(1) = Theta(N) time in the worst case 
     * 
     *
     */
    public void reverse()
    {
      if(!isEmpty() && head.next != null) {
    	  DLLNode currNode = head;
    	 // tail = currNode;
    	  DLLNode tmpNode = null;
    	  while(currNode != null) {
    		  tmpNode = currNode.prev;
    		  currNode.prev = currNode.next;
    		  currNode.next = tmpNode;
    		  currNode = currNode.prev;
    	  }
    	  
//    	  if(tmpNode != null) {
    		  head = tmpNode.prev;
    	//  }  
      }
      
    }

    /**
     * Removes all duplicate elements from the list.
     * The method should remove the _least_number_ of elements to make all elements uniqueue.
     * If the list contains "A", "B", "C", "B", "D", "A" before the method is called
     * Then it should contain "A", "B", "C", "D" after it returns.
     * The relative order of elements in the resulting list should be the same as the starting list.
     *
     * Worst-case asymptotic running time cost: Theta(N^2)
     *
     * Justification:
     * Code consists of lines within if statement
     * 
     * If statement 
     * - Here we make a call to the isEmpty() function which we have calculated to run in Theta(1)
     * we are also checking our head node  which is another Theta(1) operation.
     * - We have a single assignment statement which is a constant time operation Theta(1)
     * - We then have an outer for loop and for every one iteration of the outer for loop the inner for loop will iterate N - 1 times
     * as our loop starts from our head node + 1. The outer for loop will iterate all N elements in the list
     * - Hence cost of inner for loop will be (N - 1) * Theta(1) = Theta (N - 1) = Theta(N) and the outer for loop will cost N * Theta(N)
     * 
     * 
     * Therefore this method will run in Theta(1) + Theta(1) + Theta(1) + (Theta(N) * Theta(N)) = Theta(N^2) time in the worst case 
     * 
     *
     */
    public void makeUnique()
    {
    	if(!isEmpty() && head.next != null) {
    		int i = 0;
    		for (DLLNode node1 = head; node1 != null; node1 = node1.next) {
    			int j = i + 1;
    			for (DLLNode node2 = node1.next; node2 != null; node2 = node2.next) {
    				if (node1.data == node2.data) {
    					deleteAt(j);
    					j = i;
    					node2 = node1;
    				}
    				j++;
    			}
    			i++;
    		}
    	}
    }


    /*----------------------- STACK API 
     * If only the push and pop methods are called the data structure should behave like a stack.
     */

    /**
     * This method adds an element to the data structure.
     * How exactly this will be represented in the Doubly Linked List is up to the programmer.
     * @param item : the item to push on the stack
     *
     * Worst-case asymptotic running time cost: Theta (N)
     *
     * Justification:
     *  This method consists of the insertBefore method call as we have calculated the WC running time cost of 
     *  insertBefore which is Theta(N). This operation will also run in Theta(N) running time 
     *  
     *  Therefore this method will run in Theta(N) = Theta(N) time in the worst case.
     */
    public void push(T item) 
    {
      insertBefore(0, item);
    }

    /**
     * This method returns and removes the element that was most recently added by the push method.
     * @return the last item inserted with a push; or null when the list is empty.
     *
     * Worst-case asymptotic running time cost: Theta (N)
     *
     * Justification: 
     *  - Code consists of 3 operations a conditional expression, function call and return 
     *  - Call to deleteAt is Theta(N) as we have calculated earlier and all other statements are constant time operations with cost 
     *  Theta(1)
     *  
     *  Therefore this method will run in Theta(1) + Theta(N) = Theta(N) time in the worst case.
     */
    public T pop() 
    {
    	T poppedElem = isEmpty() ? null : head.data;
    	deleteAt(0);
    	return poppedElem;
    }

    /*----------------------- QUEUE API
     * If only the enqueue and dequeue methods are called the data structure should behave like a FIFO queue.
     */
 
    /**
     * reference: https://www.scss.tcd.ie/Vasileios.Koutavas/teaching/csu22011/content/data/uploads/lecture07.pdf
     * This method adds an element to the data structure.
     * How exactly this will be represented in the Doubly Linked List is up to the programmer.
     * @param item : the item to be enqueued to the stack
     *
     * Worst-case asymptotic running time cost: Theta(1)
     *
     * Justification:
     *  Code consist of lines before if statement, the if statement and the else statement 
     *  
     *  Lines before if statement 
     *  - These lines consist of two constant time operations which create a new node at tail hence total cost of theser 
     *  lines is Theta(1)
     *  
     *  If statement
     *  - This makes a call to function isEmpty() which we have calculated previously to run in Theta(1) time in the WC
     *  the single assignment statement is also a constant time operation so in total all lines within if statement run in Theta(1) time 
     *  
     *  Else statement 
     *  - This part of the code consists of a single assignment statement which again runs in Theta(1) time
     *  
     *  
     *  Therefore this method will run in Theta(1) + Theta(1) + Theta(1) = Theta(1) time in the worst case.
     *  
     *  
     */
    public void enqueue(T item) 
    {
    	DLLNode oldTail = tail;
    	tail = new DLLNode(item, null, null);
    	if (isEmpty()){
    		head = tail;
    	}
    	else {
    		oldTail.next = tail;
        }
    }

     /**
     * This method returns and removes the element that was least recently added by the enqueue method.
     * @return the earliest item inserted with an enqueue; or null when the list is empty.
     *
     * Worst-case asymptotic running time cost: Theta(N)
     *
     * Justification:
     *  - Code consists of 3 operations a conditional expression, function call and return 
     *  - Call to deleteAt is Theta(N) as we have calculated earlier and all other statements are constant time operations with cost 
     *  Theta(1)
     *  
     *  Therefore this method will run in Theta(1) + Theta(N) = Theta(N) time in the worst case.
     
     */
    public T dequeue() 
    {
    	T dequeuedElem = isEmpty() ? null : head.data;
    	deleteAt(0);
    	return dequeuedElem;
    }
 

    /**
     * @return a string with the elements of the list as a comma-separated
     * list, from beginning to end
     *
     * Worst-case asymptotic running time cost:   Theta(n)
     *
     * Justification:
     *  The code consists of the lines before the for-loop, the for-loop itself and the lines after the for-loop.
     *
     *  The lines before the for loop involve 
     *  - the creation of a StringBuilder object which does not depend on the length of the list. Therefore it takes Theta(1) time.
     *  - the allocation and assignment of two variables which are constant time operations.
     *  Thus the code before the for-loop will take Theta(1) time to run.
     *
     *  The lines after the for-loop involve a single return instruction and thus take Theta(1) time.
     *
     *  The for-loop itself will iterate over all elements of the DLL. Thus it will perform Theta(N) iterations.
     *  Each iteration will involve:
     *   * The if-conditional will run:
     *       - the if-then-else conditions and branching, which are constant time operations. Thus these cost Theta(1) time.
     *       - The then-branch of the conditional calls StringBuilder's append() method, which (from the Java documentation) we know it runs in Theta(1) time.
     *       - the else-branch of the conditional involves a single assignment, thus runs in Theta(1) time.
     *     Therefore the if-then-else conditions will cost Theta(1) in the worst case.
     *   * The final call to StringBuilder's append will cost again Theta(1)
     *   * the nested call to toString() will cost time proportional to the length of the strings (but not the length of the list). 
     *     Assuming strings are relatively small compared to the size of the list, this cost will be Theta(1).
     *  Therefore each iteration of the loop will cost Theta(1).
     *  Thus the lines involving the for-loop will run in Theta(N)*Theta(1) = Theta(N).
     *
     * Therefore this method will run in Theta(1) + Theta(1) + Theta(N) = Theta(N) time in the worst case.
     *
     */
    public String toString() 
    {
      StringBuilder s = new StringBuilder();
      boolean isFirst = true; 

      // iterate over the list, starting from the head
      for (DLLNode iter = head; iter != null; iter = iter.next)
      {
        if (!isFirst)
        {
          s.append(",");
        } else {
          isFirst = false;
        }
        s.append(iter.data.toString());
      }

      return s.toString();
    }


}
