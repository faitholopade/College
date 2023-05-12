public class MyClass {
    public static void main(String args[]) {
        int j = 86;
            for(int i = 86; i < 128; i++ ){
                System.out.println("--Address " +j+ "(Last 2 ID + " +j+")--\n"+
                                   "wait for 100ns;\n"+
                                   "Address_TB <= \""+String.format("%17s", Integer.toBinaryString(i)).replace(' ', '0')+ "\";\n");
                j++;
            }
        }
    }