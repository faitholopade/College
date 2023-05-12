public class MyClass {
    public static void main(String args[]) {
        for(int i = 122; i < 128; i++){
            System.out.println("--Address " +i+ "--\nwait until Clock_TB'event and Clock_TB = '1';\n"+
                                "WriteEnable_TB <= '1';\n"+
                                "DataIn_TB <= ;\n"+
                                "Address_TB <= X;\n");
        }
    }
    }