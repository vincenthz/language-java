 package TypeVarMembers;

         class C {
                 void mCDefault() {}
                 public void mCPublic() {}
                 private void mCPrivate() {}
                 protected void mCProtected() {}
         }
         class CT extends C implements I {}
         interface I {
                 void mI();
                 <T extends C & I> void test(T t) ;
        }