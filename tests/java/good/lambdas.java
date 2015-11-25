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