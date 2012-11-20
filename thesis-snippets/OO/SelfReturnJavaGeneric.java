abstract class A<T extends A<T>> {
   public T foo() {
      return (T) getThis();
   }
   public abstract T getThis();
}

class B extends A<B> {
   public B bar() {
      return this;
   }
   public B getThis() {
      return this;
   }
}

class Main {
   public static void main(String[] args) {
      B b = new B();
      b.bar().foo().bar();
   }
}
