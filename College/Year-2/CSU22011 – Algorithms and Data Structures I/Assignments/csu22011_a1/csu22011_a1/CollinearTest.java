package csu22011_a1;

import static org.junit.Assert.*;

import org.junit.Test;
//import org.junit.Ignore;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;


//-------------------------------------------------------------------------
/**
 *  Test class for Collinear.java
 *
 *  @author  
 *  @version 03/10/22 22:33:19
 */
@RunWith(JUnit4.class)
public class CollinearTest
{
	//~ Constructor ........................................................
	@Test
	public void testConstructor()
	{
		new Collinear();
	}

	//~ Public Methods ........................................................

	// ----------------------------------------------------------
	/**
	 * Check that the two methods work for empty arrays
	 */

	@Test
	public void testEmpty()
	{
		int expectedResult = 0;

		assertEquals("countCollinear with 3 empty arrays should return zero",     expectedResult, Collinear.countCollinear(new int[0], new int[0], new int[0]));
		assertEquals("countCollinearFast with 3 empty arrays should return zero", expectedResult, Collinear.countCollinearFast(new int[0], new int[0], new int[0]));
	}

	/*
	 * Reference: https://discord.com/channels/887676488655175730/1018810916986429471 (class discord server)
	 */


	/**
	 * Check countCollinear
	 */

	@Test 
	public void  testCountCo() {
		int [] a1 = {1, 0, 3};
		int [] a2 = {2, 0, 1};
		int [] a3 = {3, 0, 1};

		//		Collinear.countCollinear(a1, a2, a3);

		int expected = 4;

		assertEquals(expected, Collinear.countCollinear(a1, a2, a3));
	}

	/*
	 * Reference: https://discord.com/channels/887676488655175730/1018810916986429471 (class discord server)
	 */


	/**
	 * Check countCollinear
	 */

	@Test 
	public void  testCountCoFast() {
		int [] a1 = {1, 0, 3};
		int [] a2 = {2, 0, 1};
		int [] a3 = {3, 0, 1};

		//		Collinear.countCollinearFast(a1, a2, a3);

		int expected = 4;

		assertEquals(expected, Collinear.countCollinearFast(a1, a2, a3));
	}




	/**
	 * Check that sort method works 
	 */

	@Test 
	public void testSort()
	{
		int[] input = {9, 2, 4, 0, 6, 8};
		//		int[] inputCpy = {9, 2, 4, 0, 6, 8}
		int [] expectedOutput = {0, 2, 4, 6, 8, 9};
		Collinear.sort(input);
		assertArrayEquals(input, expectedOutput);

	}

	/**
	 * Check if binary search method works
	 */

	@Test
	public void testBS() {
		int [] input = {14, 2, 22, 94, 87, 46, 3};
		Collinear.sort(input);
		assertEquals(true, Collinear.binarySearch(input, 87));
		assertEquals(true, Collinear.binarySearch(input, 46));
		assertEquals(false, Collinear.binarySearch(input, 12));
		assertEquals(false, Collinear.binarySearch(input, 0));
	}

	// TODO: write more tests here to cover 100% of the instructions and the branches of Collinear.java

}