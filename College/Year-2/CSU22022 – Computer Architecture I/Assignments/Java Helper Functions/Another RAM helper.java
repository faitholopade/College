public class MyClass {
    public static void main(String args[]) {
        int count = 0;
        for(int i = 6; i < 38; i++){
            System.out.println("--Address "+i+"--\n"+
                                "wait until Clock_TB'event and Clock_TB = '1';\n"+
                                "WriteEnable_TB <= '1';\n"+
                                "DataIn_TB <= X\"0000000\"\n"+
                                "Address_TB <= X\"000000"+Integer.toHexString(i)+"\";\n");
        }
    }
}