package csu22011_a1;


//-------------------------------------------------------------------------
/**
 * This class contains only two static methods that search for points on the
 * same line in three arrays of integers.
 *
 * @author
 * @version 03/10/22 22:22:52
 */
class Collinear {

	public static int Y1 = 1;
	public static int Y2 = 2;
	public static int Y3 = 3;

	// ----------------------------------------------------------
	/**
	 * Counts for the number of non-hoizontal lines that go through 3 points in
	 * arrays a1, a2, a3. This method is static, thus it can be called as
	 * Collinear.countCollinear(a1,a2,a3)
	 * 
	 * @param a1:
	 *            An UNSORTED array of integers. Each integer a1[i] represents
	 *            the point (a1[i], 1) on the plain.
	 * @param a2:
	 *            An UNSORTED array of integers. Each integer a2[i] represents
	 *            the point (a2[i], 2) on the plain.
	 * @param a3:
	 *            An UNSORTED array of integers. Each integer a3[i] represents
	 *            the point (a3[i], 3) on the plain.
	 * @return the number of points which are collinear and do not lie on a
	 *         horizontal line.
	 *
	 *         Array a1, a2 and a3 contain points on the horizontal line y=1,
	 *         y=2 and y=3, respectively. A non-horizontal line will have to
	 *         cross all three of these lines. Thus we are looking for 3 points,
	 *         each in a1, a2, a3 which lie on the same line.
	 *
	 *         Three points (x1, y1), (x2, y2), (x3, y3) are collinear (i.e.,
	 *         they are on the same line) if
	 * 
	 *         x1(y2-y3)+x2(y3-y1)+x3(y1-y2)=0
	 *
	 *         In our case y1=1, y2=2, y3=3.
	 *
	 *         You should implement this using a BRUTE FORCE approach (check all
	 *         possible combinations of numbers from a1, a2, a3) which should run
	 *         in the worst case in O(N^3).
	 *   
	 */
	static int countCollinear(int[] a1, int[] a2, int[] a3) {
		int countCollin = 0;

		for (int x1 : a1) {
			for (int x2: a2) {
				for (int x3 : a3) {
					if((x1 * (Y2 - Y3) + x2 * (Y3 - Y1) + x3 * (Y1 - Y2)) == 0) {
						countCollin++;
					}
				}
			}
		}
		return countCollin;
	}

	// ----------------------------------------------------------
	/**
	 * Counts for the number of non-hoizontal lines that go through 3 points in
	 * arrays a1, a2, a3. This method is static, thus it can be called as
	 * Collinear.countCollinearFast(a1,a2,a3)
	 * 
	 * @param a1:
	 *            An UNSORTED array of integers. Each integer a1[i] represents
	 *            the point (a1[i], 1) on the plain.
	 * @param a2:
	 *            An UNSORTED array of integers. Each integer a2[i] represents
	 *            the point (a2[i], 2) on the plain.
	 * @param a3:
	 *            An UNSORTED array of integers. Each integer a3[i] represents
	 *            the point (a3[i], 3) on the plain.
	 * @return the number of points which are collinear and do not lie on a
	 *         horizontal line.
	 *
	 *         In this implementation you should make non-trivial use of
	 *         InsertionSort and Binary Search. This method should run in the
	 *         worst case in O(N^2 lg(N)).
	 *
	 */
	static int countCollinearFast(int[] a1, int[] a2, int[] a3) {
		int countCollin = 0;
		Collinear.sort(a3);
		for (int x1 : a1) {
			for (int x2: a2) {
				//int x3 = (Y2 * x2) - x1;
				int x3 = ((-x1 * (Y2 - Y3) - x2 * (Y3 - Y1)) / (Y1 - Y2));
				if (Collinear.binarySearch(a3, x3)) {
					countCollin++;
				}
			}
		}
		return countCollin;

	}

	// ----------------------------------------------------------
	/**
	 * Sorts an array of integers according to InsertionSort. This method is
	 * static, thus it can be called as Collinear.sort(a)
	 * 
	 * @param a:
	 *            An UNSORTED array of integers.
	 * @return after the method returns, the array must be in ascending sorted
	 *         order.
	 *
	 * This method runs in the worst case in Theta(N^2) time.
	 */
	static void sort(int[] a) {
		for(int i = 0; i < a.length; i++){
			int j = i-1;
			while(j >= 0 && a[j]>a[j+1]){
				int temp = a[j];
				a[j] = a[j+1];
				a[j+1] = temp;
				j--;
			}				
		}
	}

	// ----------------------------------------------------------
	/**
	 * Searches for an integer inside an array of integers. This method is
	 * static, thus it can be called as Collinear.binarySearch(a,x)
	 * 
	 * @param a:
	 *            A array of integers SORTED in ascending order.
	 * @param x:
	 *            An integer.
	 * @return true if 'x' is contained in 'a'; false otherwise.
	 * 
	 * This method runs in the worst case in Theta(lg(N)) time.
	 *
	 */
	static boolean binarySearch(int[] a, int x) {
		int lo = 0, hi = a.length-1;
		while (lo <= hi)
		{
			int mid = lo + (hi - lo) / 2;
			if (x < a[mid]) hi = mid - 1;
			else if (x > a[mid]) lo = mid + 1;
			else return true;
		}
		return false;
	}
	
	public static void main(String[] args) {
		In FileThousand1 = new In("r01000-1.txt");
		In FileThousand2 = new In("r01000-2.txt");
		In FileThousand3 = new In("r01000-3.txt");


		int[] a10001 = FileThousand1.readAllInts();
		int[] a10002 = FileThousand2.readAllInts();
		int[] a10003 = FileThousand3.readAllInts();

		Stopwatch stopwatch = new Stopwatch();
		StdOut.println(Collinear.countCollinear(a10001,a10002,a10003));
		double time = stopwatch.elapsedTime();
		StdOut.println("1000 countCollinear elapsed time " + time);

		Stopwatch stopwatch2 = new Stopwatch();
		StdOut.println(Collinear.countCollinearFast(a10001,a10002,a10003));
		double time2 = stopwatch2.elapsedTime();
		StdOut.println("1000 CountCollinearFast elapsed time " + time2);

		In File2Thousand1 = new In("r02000-1.txt");
		In File2Thousand2 = new In("r02000-2.txt");
		In File2Thousand3 = new In("r02000-3.txt");


		int[] a20001 = File2Thousand1.readAllInts();
		int[] a20002 = File2Thousand2.readAllInts();
		int[] a20003 = File2Thousand3.readAllInts();

		Stopwatch stopwatch3 = new Stopwatch();
		StdOut.println(Collinear.countCollinear(a20001,a20002,a20003));
		//   	 StdOut.println(a);
		double time3 = stopwatch.elapsedTime();
		StdOut.println("2000 countCollinear elapsed time " + time3);

		Stopwatch stopwatch4 = new Stopwatch();
		StdOut.println(Collinear.countCollinearFast(a20001,a20002,a20003));
		//   	 StdOut.println(a);
		double time4 = stopwatch4.elapsedTime();
		StdOut.println("2000 CountCollinearFast elapsed time " + time4);

		In File4Thousand1 = new In("r04000-1.txt");
		In File4Thousand2 = new In("r04000-2.txt");
		In File4Thousand3 = new In("r04000-3.txt");


		int[] a40001 = File4Thousand1.readAllInts();
		int[] a40002 = File4Thousand2.readAllInts();
		int[] a40003 = File4Thousand3.readAllInts();

		Stopwatch stopwatch5 = new Stopwatch();
		StdOut.println(Collinear.countCollinear(a40001,a40002,a40003));
		//   	 StdOut.println(a);
		double time5 = stopwatch.elapsedTime();
		StdOut.println("4000 countCollinear elapsed time " + time5);

		Stopwatch stopwatch6 = new Stopwatch();
		StdOut.println(Collinear.countCollinearFast(a40001,a40002,a40003));
		double time6 = stopwatch6.elapsedTime();
		StdOut.println("4000 CountCollinearFast elapsed time " + time6);

		In File5Thousand1 = new In("r05000-1.txt");
		In File5Thousand2 = new In("r05000-2.txt");
		In File5Thousand3 = new In("r05000-3.txt");


		int[] a50001 = File5Thousand1.readAllInts();
		int[] a50002 = File5Thousand2.readAllInts();
		int[] a50003 = File5Thousand3.readAllInts();

		Stopwatch stopwatch7 = new Stopwatch();
		StdOut.println(Collinear.countCollinear(a50001,a50002,a50003));
		double time7 = stopwatch7.elapsedTime();
		StdOut.println("5000 countCollinear elapsed time " + time7);

		Stopwatch stopwatch8 = new Stopwatch();
		StdOut.println(Collinear.countCollinearFast(a50001,a50002,a50003));
		double time8 = stopwatch8.elapsedTime();
		StdOut.println("CountCollinearFast elapsed time " + time8);
	}

}
