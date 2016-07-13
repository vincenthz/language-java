package testPackage;

@interface TestAnn1 {}
@interface TestAnn2 {
  boolean value() default false;
}
@interface TestAnn3 {
  String first();
  String last();
}

class Other { 
  @TestAnn1
  void Test1() {}

  @TestAnn2(true)
  void Test2() {}

  @TestAnn3(first = "foo", last = "bar")
  void Test3() {}
}
