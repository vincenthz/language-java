

public class Some {

   public void assignToArray() {
      array[1] = 0;

      array[1][2] = 0;
   }

   public void some() {
      Object[][] a = new Object[][] {null};
      Object[][] b = new Object[][] {new Object[] {}};
   }

   public void allIndexes() {
      Object[][] a = new Object[1][2];
   }

   public void firstIndex() {
      onarg(new Object[1][]);

      Object[][] a = new Object[1][] ;
   }

   public void onarg(Object[][] ignored) {}
}