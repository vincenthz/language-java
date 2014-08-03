
class Some {
   <T> void some() {}
   public final <T> void somePub() {}
   public static <T> void someStatic() {}
}

class SomeOther extends Some {
   <T> void some() { super.<T>some(); }
}