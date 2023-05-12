public class MyClass {
    public static void main(String args[]) {
        int count = 0;
        for(int i = 66; i < 194; i++ ){
            System.out.println(String.format("%17s", Integer.toBinaryString(i)).replace(' ', '0'));
            count++;
            if(count == 16){
                System.out.println("\n");
                count = 0;
            }
        }
    }
}