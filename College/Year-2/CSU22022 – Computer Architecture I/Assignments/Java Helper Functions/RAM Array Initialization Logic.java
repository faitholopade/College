public class MyClass {
    public static void main(String args[]) {
        int j = 0;
            for(int i = 66; i < 194; i++ ){
                System.out.println("X\"000000"+Integer.toHexString(i).toUpperCase()+ "\", -- " +i);
                j++;
                if(j == 16){
                    System.out.println("\n");
                    j = 0;
                }
        }
    }
}