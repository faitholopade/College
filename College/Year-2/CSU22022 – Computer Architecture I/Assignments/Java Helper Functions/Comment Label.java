public class MyClass {
    public static void main(String args[]) {
        int count = 0;
        for(int i = 0; i < 128; i++ ){
            System.out.println("Next Address = Last 2 ID + "+i);
            count++;
            if(count == 16){
                System.out.println("\n");
                count = 0;
            }
        }
    }
}