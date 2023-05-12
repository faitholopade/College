public class MyClass {
    public static void main(String args[]) {
    int j = 92;
            for(int i = 159; i < 194; i++){
                j++;
                System.out.println("--Address "+j+"(Last 2 ID + "+j+")--\n"+
                                   "wait for 100ns;\n"+
                                   "Address_TB <= \"0\" & X\"00"+Integer.toHexString(i).toUpperCase()+"\";\n");
        }
    }
}