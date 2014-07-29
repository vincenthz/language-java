
class Some {

   public void a() {
      int some = 0;

      // check that this is not broken
      int e = some << 1;
      int e = some >> 1;

      some >>>= 1;
      some >>= 1;

      boolean a = some > 1;
      boolean f = some < 1;

      boolean a1 = some >= 1;
      boolean f1 = some <= 1;

      Map<Integer, List<IHRCity>> mamap;

      Map<Integer, List<IHRCity>> mamap = new HashMap<Integer, List<IHRCity>>();

      final Map<Integer, List<String>> some = null;
   }


}