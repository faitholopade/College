public class MyClass {
    public static void main(String args[]) {
        for(int i = 2; i < 32; i++){
            System.out.println("-- Instantiate Full Adder Bit "+i+"\nBIT"+i+": DP_FullAdder_21364066 port map (\n"
            +"  A => A("+i+"),\n"
            +"  B => B("+i+"),\n"
            +"  C_IN => C"+(i - 1)+"_to_C"+i+",\n"
            +"  SUM => SUM("+i+"),\n"
            +"  C_OUT => C"+i+ "_to_C"+(i + 1)+",\n"
        +"\n);");
        }
    }
}