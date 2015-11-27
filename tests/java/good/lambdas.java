import a;


final class E {
   void method() {
      final Runnable e = () -> {};
      call(() -> {});

      final Runnable c = () -> e.run();
      call(() -> e.run());

      final Runnable c = e::run;
      call(e::run);

      final Runnable c = Runnable::run;
      call(Runnable::run);

      final Receiver<R> r = (arg) -> {};
      call((arg) -> {});

      final Receiver<R> r = arg -> {};
      call(arg -> {});

      final Function<A,B> r = arg -> othervalue;
      call(arg -> othervalue);

      final Function<A,B> r = arg -> { return othervalue; };
      call(arg -> { return othervalue; });

      final TwoArgs r = (arg, other) -> { return other; };
      call((arg, other) -> { return other; });

   }
}

// these are lambdas by the github user markWot
final class LambdasByMarkWot {
    // no Parameters; result is void
    Type x = () -> {};
    // No Parameters, expression body
    Type x = () -> 42;
    // No Parameters; expression body
    Type x = () -> null;
    // No Parameters; block body with return
    Type x = () -> {return 42;};
    // No Parameters; void block body
    Type x = () -> {System.gc();};
    // Complex block body with returns
    Type x = () ->
        {
            if (true) return 12;
            else {
                int result = 15;
                for (int i = 1; i < 10; i++)
                    result *= i;
                return result;
            }
        };
    // Single declared-type parameter
    Type x = (int x) -> x + 1;
    // Single declared-type parameter
    Type x = (int x) -> {return x+1;};
    // Single inferred-type parameter
    Type x = (x) -> x+1;
    // Parentheses optional for single inferred type parameter
    Type x = x -> x + 1;
    // Single declared-type parameter
    Type x = (String s) -> s.length();
    // Single declared-type parameter
    Type x = (Thread t) -> { t.start();};
    // Single inferred-type parameter
    Type x = s -> s.length ();
    // Single inferred-type parameter
    Type x = t -> { t.start();};
    // Multiple declared-type parameters
    Type x = (int x, int y) -> x+y;
    // Multiple inferred-type parameters
    Type x = (x,y) -> x+y;
    // No distinction is made between the following lambda parameter lists:
    Type x = (int... x) -> {};
    Type x = (int[] x) -> {};
}


class E {

   interface EE {
      void call(int e);
   }

   interface II {
      int call(int e);
   }

   interface OO {
      II call(int e);
   }


   EE ee = (int x) -> System.out.println("x: " + x);

   II ii = (int y) -> y + 1;

   // lambda returning lambda, why not
   OO oo = (int x) -> (int y) -> y + x;
}