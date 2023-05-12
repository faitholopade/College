package csu22011_a3;

import static org.junit.Assert.*;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

//-------------------------------------------------------------------------
/**
 *  Test class for Doubly Linked List
 *
 *  @version 3.1 09/11/15 11:32:15
 *
 *  @author  Faith Olopade
 */

@RunWith(JUnit4.class)
public class BSTTest
{

	//TODO write more tests here.


	/** <p>Test {@link BST#prettyPrintKeys()}.</p> */

	@Test
	public void testPrettyPrint() {
		BST<Integer, Integer> bst = new BST<Integer, Integer>();
		assertEquals("Checking pretty printing of empty tree",
				"-null\n", bst.prettyPrintKeys());

		//  -7
		//   |-3
		//   | |-1
		//   | | |-null
		bst.put(7, 7);       //   | |  -2
		bst.put(8, 8);       //   | |   |-null
		bst.put(3, 3);       //   | |    -null
		bst.put(1, 1);       //   |  -6
		bst.put(2, 2);       //   |   |-4
		bst.put(6, 6);       //   |   | |-null
		bst.put(4, 4);       //   |   |  -5
		bst.put(5, 5);       //   |   |   |-null
		//   |   |    -null
		//   |    -null
		//    -8
		//     |-null
		//      -null

		String result = "-7\n" +
				" |-3\n" + 
				" | |-1\n" +
				" | | |-null\n" + 
				" | |  -2\n" +
				" | |   |-null\n" +
				" | |    -null\n" +
				" |  -6\n" +
				" |   |-4\n" +
				" |   | |-null\n" +
				" |   |  -5\n" +
				" |   |   |-null\n" +
				" |   |    -null\n" +
				" |    -null\n" +
				"  -8\n" +
				"   |-null\n" +
				"    -null\n";
		assertEquals("Checking pretty printing of non-empty tree", result, bst.prettyPrintKeys());
	}


	/** <p>Test {@link BST#delete(Comparable)}.</p> */
	@Test
	public void testDelete() {
		BST<Integer, Integer> bst = new BST<Integer, Integer>();
		bst.delete(1);
		assertEquals("Deleting from empty tree", "()", bst.printKeysInOrder());
		assertEquals("Deleting from empty tree", null, bst.get(null));

		bst.put(7, 7);   //        _7_
		bst.put(8, 8);   //      /     \
		bst.put(3, 3);   //    _3_      8
		bst.put(1, 1);   //  /     \
		bst.put(2, 2);   // 1       6
		bst.put(6, 6);   //  \     /
		bst.put(4, 4);   //   2   4
		bst.put(5, 5);   //        \
		//         5

		assertEquals("Checking order of constructed tree",
				"(((()1(()2()))3((()4(()5()))6()))7(()8()))", bst.printKeysInOrder());

		bst.delete(9);
		assertEquals("Deleting non-existent key",
				"(((()1(()2()))3((()4(()5()))6()))7(()8()))", bst.printKeysInOrder());

		bst.delete(8);
		assertEquals("Deleting leaf", "(((()1(()2()))3((()4(()5()))6()))7())", bst.printKeysInOrder());

		bst.delete(6);
		assertEquals("Deleting node with single child",
				"(((()1(()2()))3(()4(()5())))7())", bst.printKeysInOrder());

		bst.delete(3);
		assertEquals("Deleting node with two children",
				"(((()1())2(()4(()5())))7())", bst.printKeysInOrder());
	}

	@Test
	public void testHeight() {
		BST<String, String> bst = new BST<String, String>();
		assertEquals("Checking height of empty tree expect -1", -1,  bst.height());

		bst.put("C", "C");
		assertEquals("Checking height of tree that has root.left == null && root.right == null", 0, bst.height());


		bst.put("A", "A");
		bst.put("B", "B");
		bst.put("D", "D");
		bst.put("E", "E");

		assertEquals("Checking height of tree", 2, bst.height());
	}

	@Test
	public void testRank() {
		BST<String, String> bst = new BST<String, String>();
		bst.put("H", "H");
		bst.put("S", "S");
		bst.put("R", "R");
		bst.put("X", "X");
		bst.put("C", "C");
		bst.put("A", "A");
		bst.put("E", "E");


		assertEquals("Checking rank of S", 5, bst.rank("S"));
		assertEquals("Checking rank of R", 4, bst.rank("R"));
		assertEquals("Checking rank of Q", 4, bst.rank("Q"));

	}

	@Test
	public void testMedian() {
		BST<String, String> bst = new BST<String, String>();
		assertEquals("Checking median for an empty tree", null, bst.median());

		bst.put("H", "H");
		assertEquals("Checking median for a tree with 1 element",bst.get("H"), bst.median());

		bst.put("E", "E");
		bst.put("C", "C");
		assertEquals("Checking median for a tree with 3 element",bst.get("E"), bst.median());

		bst.put("A", "A");
		assertEquals("Checking median for a tree with 4 element",bst.get("C"), bst.median());

		bst.put("B", "B");
		assertEquals("Checking median for a tree with 5 element",bst.get("C"), bst.median());


		BST<String, String> bst1 = new BST<String, String>();
		assertEquals("Checking median for an empty tree", null, bst1.median());

		bst1.put("A", "A");
		assertEquals("Checking median for a tree with 1 element",bst1.get("A"), bst1.median());

		bst1.put("C", "C");
		bst1.put("U", "U");
		assertEquals("Checking median for a tree with 3 element",bst1.get("C"), bst1.median());

		bst1.put("W", "W");
		assertEquals("Checking median for a tree with 4 element",bst1.get("C"), bst1.median());


	}

	@Test
	public void testPrintKeysInOrder() {
		BST<String, String> bst = new BST<String, String>();
		assertEquals("Checking printing in order of empty tree", "()", bst.printKeysInOrder());

		bst.put("A", "A");
		assertEquals("Checking order of constructed tree", "(()A())", bst.printKeysInOrder());

		BST<String, String> bst1 = new BST<String, String>();
		assertEquals("Checking printing in order of empty tree", "()", bst1.printKeysInOrder());

		bst1.put("B", "B");
		bst1.put("A", "A");
		bst1.put("C", "C");
		bst1.put("D", "D");
		assertEquals("Checking order of constructed tree", "((()A())B(()C(()D())))", bst1.printKeysInOrder());

		BST<String, String> bst2 = new BST<String, String>();
		assertEquals("Checking printing in order of empty tree", "()", bst2.printKeysInOrder());

		bst2.put("S", "S");
		bst2.put("X", "X");
		bst2.put("E", "E");
		bst2.put("R", "R");
		bst2.put("H", "H");
		bst2.put("M", "M");
		bst2.put("A", "A");
		bst2.put("C", "C");
		assertEquals("Checking order of constructed tree", "(((()A(()C()))E((()H(()M()))R()))S(()X()))", bst2.printKeysInOrder());

	}

	@Test
	public void testDeleteMax() {
		BST<String, String> bst = new BST<String, String>();
		bst.put("B", "B");
		bst.put("A", "A");
		bst.put("C", "C");
		bst.put("D", "D");
		bst.put("S", "S");
		bst.put("X", "X");
		bst.put("E", "E");
		bst.put("R", "R");
		bst.put("H", "H");
		bst.put("M", "M");
		bst.put("A", "A");
		bst.put("C", "C");

		bst.deleteMax();
		assertFalse("Checking deletion of max node", bst.contains("X"));
	}

	@Test
	public void testPut() {
		BST<Integer, Integer> bst = new BST<Integer, Integer>();
		bst.put(1, 3);
		bst.put(2, null);
		bst.put(3, 12);
		assertEquals("Checking put with 3 keys and 2 values","3", bst.get(1).toString());
	}
}

