package csu22011_a2;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.junit.Ignore;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

//-------------------------------------------------------------------------
/**
 *  Test class for Doubly Linked List
 *
 *  @author Faith Olopade 
 *  @version 13/10/16 18:15
 */
@RunWith(JUnit4.class)
public class DoublyLinkedListTest
{
	//~ Constructor ........................................................
	@Test
	public void testConstructor()
	{
		new DoublyLinkedList<Integer>();
	}

	//~ Public Methods ........................................................

	// ----------------------------------------------------------
	/**
	 * Check if the insertBefore works
	 */
	@Test
	public void testInsertBefore()
	{
		// test non-empty list
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(0,1);
		testDLL.insertBefore(1,2);
		testDLL.insertBefore(2,3);

		testDLL.insertBefore(0,4);
		assertEquals( "Checking insertBefore to a list containing 3 elements at position 0", "4,1,2,3", testDLL.toString() );
		testDLL.insertBefore(1,5);
		assertEquals( "Checking insertBefore to a list containing 4 elements at position 1", "4,5,1,2,3", testDLL.toString() );
		testDLL.insertBefore(2,6);       
		assertEquals( "Checking insertBefore to a list containing 5 elements at position 2", "4,5,6,1,2,3", testDLL.toString() );


		// test empty list
		testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(0,1);        
		assertEquals( "Checking insertBefore to an empty list at position 0 - expected the element at the head of the list", "1", testDLL.toString() );
		testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(10,1);        
		assertEquals( "Checking insertBefore to an empty list at position 10 - expected the element at the head of the list", "1", testDLL.toString() );
		testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(-10,1);        
		assertEquals( "Checking insertBefore to an empty list at position -10 - expected the element at the head of the list", "1", testDLL.toString() );
	}

	/**
	 * Check if the isEmpty works
	 */
	@Test
	public void testsIsEmpty()
	{
		// test non-empty list
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
		assertTrue("Checking isEmpty()", testDLL.isEmpty());
		testDLL.insertBefore(0, 1);
		assertFalse("Checking isEmpty()", testDLL.isEmpty());

	}

	/**
	 * Check if the get works
	 */
	@Test
	public void testGet() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
		assertNull("Checking empty DLL", testDLL.get(0));

		// assertNull("Checking empty DLL", testDLL.get(-1));

		//        testDLL.insertBefore(-9,1);
		//        assertNull("Checking empty DLL", testDLL.get(-1));
		//        
		testDLL.insertBefore(0,1);
		testDLL.insertBefore(1,2);
		testDLL.insertBefore(2,3);
		assertEquals( "Checking get to a list containing 5 elements at position 0 - expected the element at position 0", "1", testDLL.get(0).toString());
		assertEquals( "Checking get to a list containing 5 elements at position 1 - expected the element at position 1", "2", testDLL.get(1).toString());
		assertEquals( "Checking get to a list containing 5 elements at position 2 - expected the element at position 2", "3", testDLL.get(2).toString());


		assertNull("Checking index out of bounds DLL", testDLL.get(24));
		testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(0,1);        
		assertEquals( "Checking get to a list containing 1 elements at position 0 - expected the element at position 0", "1", testDLL.get(0).toString());
		testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(10,1);        
		assertEquals( "Checking get to a list containing 1 elements at position 10 - expected the element at position 10", null, testDLL.get(10));
		testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(-10,1);        
		assertEquals( "Checking get to a list containing 1 elements at position -10 - expected the element at position -10", null, testDLL.get(-10));

		DoublyLinkedList<Integer> testEmptyDLL = new DoublyLinkedList<Integer>();
		assertEquals( "Checking get to an empty list at position 1 - expected null", null, testEmptyDLL.get(1));
	}

	/**
	 * Check if the deleteAt works
	 */
	@Test
	public void testDeleteAt() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
		//   assertFalse("Delete Empty", testDLL.deleteAt(0));
		assertEquals( "Checking deleteAt from an empty list at position 1 - expected false", false, testDLL.deleteAt(1));
		assertEquals( "Checking deleteAt from an empty list at position 0 - expected false", false, testDLL.deleteAt(0));

		//test non-empty list
		testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(0,1);
		testDLL.insertBefore(1,2);
		testDLL.insertBefore(2,3);
		testDLL.insertBefore(3,4);
		testDLL.insertBefore(4,5);

		// pos head
		testDLL.deleteAt(0);
		assertEquals( "Checking deleteAt from a list containing 5 elements at position 0 - expected the deleted element at the head of the list", "2,3,4,5", testDLL.toString());
		// pos tail
		testDLL.deleteAt(3);
		assertEquals( "Checking deleteAt from a list containing 4 elements at position 3 - expected the deleted element at the tail of the list", "2,3,4", testDLL.toString());
		// middle
		testDLL.deleteAt(1);
		assertEquals( "Checking deleteAt from a list containing 3 elements at position 1", "2,4", testDLL.toString());

		// out of bounds
		testDLL = new DoublyLinkedList<Integer>();
//		testDLL.insertBefore(0,1);
//		assertTrue("Delete within bounds", testDLL.deleteAt(0));
//		assertFalse("Delete outside bounds", testDLL.deleteAt(10));
		testDLL.insertBefore(0,1);
		testDLL.insertBefore(1,2);
		testDLL.insertBefore(2,3);
		assertEquals( "Checking deleteAt from a list containing 3 elements at position -10 - expected false", false, testDLL.deleteAt(-10));
		assertEquals( "Checking deleteAt from a list containing 3 elements at position -10", "1,2,3", testDLL.toString());
		assertEquals( "Checking deleteAt from a list containing 3 elements at position 700 - expected false", false, testDLL.deleteAt(700));
		assertEquals( "Checking deleteAt from a list containing 3 elements at position 700", "1,2,3", testDLL.toString());
		
		testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(0,1);
		//testDLL.deleteAt(0);
		assertEquals( "Checking deleteAt from a list containing 1 element at position 0 - expected true", true, testDLL.deleteAt(0));
		
	}

	/**
	 * Check if the reverse works
	 */
	@Test
	public void testReverse() {
		// test empty list
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
		testDLL.reverse();
		assertEquals( "Checking reverse with an empty list", "", testDLL.toString());

		// test single-element list
		testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(0,1);
		testDLL.reverse();
		assertEquals( "Checking reverse with a list containing 1 element", "1", testDLL.toString());

		// test multiple-element list
		testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(0,1);
		testDLL.insertBefore(1,2);
		testDLL.insertBefore(2,3);
		testDLL.insertBefore(3,4);
		testDLL.insertBefore(4,5);
		testDLL.reverse();
		assertEquals( "Checking reverse with a list containing 5 elements", "5,4,3,2,1", testDLL.toString());
			
	}

	/**
	 * Check if makeUnique works
	 */
	@Test
	public void testMakeUnique() {
		// test empty list
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
		testDLL.makeUnique();
		assertEquals( "Checking makeUnique with an empty list", "", testDLL.toString());

		// test single-element list
		testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(0,1);
		testDLL.makeUnique();
		assertEquals( "Checking makeUnique with a list containing 1 element", "1", testDLL.toString());

		// test multiple-element list
		testDLL = new DoublyLinkedList<Integer>();
		testDLL.insertBefore(0,2);
		testDLL.insertBefore(1,2);
		testDLL.insertBefore(2,3);
		testDLL.insertBefore(3,4);
		testDLL.insertBefore(4,3);
		testDLL.makeUnique();
		assertEquals( "Checking reverse with a 5-element list containing 2 duplicate elements", "2,3,4", testDLL.toString());
	}

	/**
	 * Check if push works
	 */
	@Test
	public void testPush() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
		testDLL.push(1);
		testDLL.push(2);
		testDLL.push(3);
		testDLL.push(4);
		testDLL.push(5);


		assertEquals( "Checking get to a list containing 5 elements", "5,4,3,2,1", testDLL.toString());
	}
	/**
	 * Check if pop works
	 */
	@Test
	public void testPop() {
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
		assertNull("Empty Stack", testDLL.pop());

		testDLL.push(1);
		testDLL.push(2);
		testDLL.push(3);

		assertEquals( "Checking pop from the stack", "3", testDLL.pop().toString() );

	}
	/**
	 * Check if enqueue works
	 */
	@Test
	public void testEnqueue(){
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
		testDLL.enqueue(1);
		testDLL.enqueue(2);
		testDLL.enqueue(3);
		testDLL.enqueue(4);
		testDLL.enqueue(5);

		assertEquals( "Checking get to a list containing 4 elements at position 0 - expected the element at position 0", "1,2,3,4,5", testDLL.toString());
	}
	/**
	 * Check if dequeue works
	 */
	@Test
	public void testDequeue(){
		DoublyLinkedList<Integer> testDLL = new DoublyLinkedList<Integer>();
		assertNull("Empty Queue",testDLL.dequeue());
		testDLL.enqueue(1);
		testDLL.dequeue();
		assertEquals("Dequeued", "", testDLL.toString());
		testDLL.enqueue(2);
		testDLL.enqueue(3);
		testDLL.dequeue();
		assertEquals("Dequeue One from list", "3", testDLL.toString());
	}

}

