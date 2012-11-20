class A {
   public A foo() {
      return this;
   }
}

class B extends A {
   public B bar() {
      return this;
   }
}

class Main {
   public static void main(String[] args) {
      B b = new B();
      b.bar().foo().bar();
			//((B) b.bar().foo()).bar();
   }
}
