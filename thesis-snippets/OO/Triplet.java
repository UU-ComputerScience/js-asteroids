class Pair<A,B> {
   private final A a;
   private final B b;
   
   public Pair(A a, B b) {
      this.a = a;
      this.b = b;
   }

   public A getFirst() {
      return a;
   }

   public B getSecond() {
      return b;
   }
/*
   public Pair<B,A> swap() {
      return new Pair<B,A>(b,a);
   }
*/
}

class Triplet<A,B,C> extends Pair<A,B> {
   private final C c;
   
   public Triplet(A a, B b, C c) {
      super(a,b);
      this.c = c;
   }

   public C getThird() {
      return c;
   }

   public Triplet<B,A,C> swap() {
      return new Triplet<B,A,C>(getSecond(),getFirst(),c);
   }
}

class Main {

   public static void main(String[] args) {
      Pair<Integer,Double> p = new Triplet<Integer,Double,String>(0,0.0,"hi");
   }
}

