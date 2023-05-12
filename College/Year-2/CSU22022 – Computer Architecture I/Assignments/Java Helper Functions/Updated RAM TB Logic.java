public class MyClass {
    public static void main(String args[]) {
        for(int i = 32; i < 34; i++){
            System.out.println("--Address "+i+"--\n"+
                                "wait until Clock_TB'event and Clock_TB = '1';\n"+
                                "WriteEnable_TB <= '0';\n"+
                                "DataIn_TB <= X\"00000000\";\n"+
                                "Address_TB <= X\"000000"+Integer.toHexString(i).toUpperCase()+"\";\n");
        }
    }
}