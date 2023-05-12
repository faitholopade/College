public class MyClass {
    public static void main(String args[]) {
        for(int i = 118; i < 128; i++){
            System.out.println("--Address "+i+"--\n"+
                                "wait until Clock_TB'event and Clock_TB = '1';\n"+
                                "WriteEnable_TB <= '0';\n"+
                                "Address_TB <= X\"000000"+Integer.toHexString(i)+"\";\n");
        }
    }
}