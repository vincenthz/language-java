class DiamondTestExtended {
   public void testGood() {
      final List<Type> list = new ArrayList();
      final List<Type> list = new ArrayList<>();
      final List<Type> list = new ArrayList<Test>();
      final List<Type> list = new Test.ArrayList();
      final List<Type> list = new Test.ArrayList<>();
      final List<Type> list = new Test.ArrayList<Test>();
      final List<Type> list = new Test<Foo>.ArrayList();
      final List<Type> list = new Test<Foo>.ArrayList<>();
      final List<Type> list = new Test<Foo>.ArrayList<Test>();
      final List<Type> list = new Test2.Test.ArrayList();
      final List<Type> list = new Test2.Test.ArrayList<>();
      final List<Type> list = new Test2.Test.ArrayList<Test>();
      final List<Type> list = new Test2.Test<Foo>.ArrayList();
      final List<Type> list = new Test2.Test<Foo>.ArrayList<>();
      final List<Type> list = new Test2.Test<Foo>.ArrayList<Test>();
      final List<Type> list = new Test2<Bar>.Test.ArrayList();
      final List<Type> list = new Test2<Bar>.Test.ArrayList<>();
      final List<Type> list = new Test2<Bar>.Test.ArrayList<Test>();
      final List<Type> list = new Test2<Bar>.Test<Foo>.ArrayList();
      final List<Type> list = new Test2<Bar>.Test<Foo>.ArrayList<>();
      final List<Type> list = new Test2<Bar>.Test<Foo>.ArrayList<Test>();
   }
}