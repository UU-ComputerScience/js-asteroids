// Copyright (c) 2005-2009  Tom Wu
// All Rights Reserved.
// See "LICENSE" for details.

// Extended JavaScript BN functions, required for RSA private ops.

// Version 1.1: new BigInteger("0", 10) returns "proper" zero

// (public)
function bnClone() { var r = nbi(); this.copyTo(r); return r; }

// (public) return value as integer
function bnIntValue() {
  if(this.s < 0) {
    if(this.t == 1) return this[0]-this.DV;
    else if(this.t == 0) return -1;
  }
  else if(this.t == 1) return this[0];
  else if(this.t == 0) return 0;
  // assumes 16 < DB < 32
  return ((this[1]&((1<<(32-this.DB))-1))<<this.DB)|this[0];
}

// (public) return value as double
function bnDoubleValue() {
  var x = this ;
  var sign = 1 ;
  if(x.s < 0) {
    x = x.negate() ;
    sign = -1 ;
  }
  var c = x.t - 1 ;
  var r = 0 ;
  while ( c >= 0 ) {
    r = r * x.DV + x[c];
    --c ;
  }
  return sign * r ;
}

// (public) return value as byte
function bnByteValue() { return (this.t==0)?this.s:(this[0]<<24)>>24; }

// (public) return value as short (assumes DB>=16)
function bnShortValue() { return (this.t==0)?this.s:(this[0]<<16)>>16; }

// (protected) return x s.t. r^x < DV
function bnpChunkSize(r) { return Math.floor(Math.LN2*this.DB/Math.log(r)); }

// (public) 0 if this == 0, 1 if this > 0
function bnSigNum() {
  if(this.s < 0) return -1;
  else if(this.t <= 0 || (this.t == 1 && this[0] <= 0)) return 0;
  else return 1;
}

// (protected) convert to radix string
function bnpToRadix(b) {
  if(b == null) b = 10;
  if(this.signum() == 0 || b < 2 || b > 36) return "0";
  var cs = this.chunkSize(b);
  var a = Math.pow(b,cs);
  var d = nbv(a), y = nbi(), z = nbi(), r = "";
  this.divRemTo(d,y,z);
  while(y.signum() > 0) {
    r = (a+z.intValue()).toString(b).substr(1) + r;
    y.divRemTo(d,y,z);
  }
  return z.intValue().toString(b) + r;
}

// (protected) convert from radix string
function bnpFromRadix(s,b) {
  this.fromInt(0);
  if(b == null) b = 10;
  var cs = this.chunkSize(b);
  var d = Math.pow(b,cs), mi = false, j = 0, w = 0;
  for(var i = 0; i < s.length; ++i) {
    var x = intAt(s,i);
    if(x < 0) {
      if(s.charAt(i) == "-" && this.signum() == 0) mi = true;
      continue;
    }
    w = b*w+x;
    if(++j >= cs) {
      this.dMultiply(d);
      this.dAddOffset(w,0);
      j = 0;
      w = 0;
    }
  }
  if(j > 0) {
    this.dMultiply(Math.pow(b,j));
    this.dAddOffset(w,0);
  }
  if(mi) BigInteger.ZERO.subTo(this,this);
}

// (protected) alternate constructor
function bnpFromDouble(x) {
  var sign = 1 ;
  var div = this.DV >> 1 ;
  if ( x < 0 ) {
    sign = -1 ;
    x = -x ;
  }
  var a = new Array() ;
  var c = 0 ;
  while( x > 0 ) {
    var d = Math.floor( x / div ) ;
    var r = x - d * div ;
    a[c] = r ;
    ++c ;
    // writeln("bnpFromDouble.L1 " + [x,d,r]) ;
    x = d ;
  }
  var n = nbv(0) ;
  for (c = a.length-1 ; c >= 0 ; --c ) {
    n.dMultiply(div) ;
    var x = nbv(a[c]) ;
    // writeln("bnpFromDouble.L2A " + [c,a[c],x,n]) ;
    n.addTo(x,n) ;
    // writeln("bnpFromDouble.L2B " + [c,a[c],x,r,n]) ;
  }
  if ( sign < 0 ) {
  	BigInteger.ZERO.subTo(n,this) ;
  } else {
    n.copyTo(this) ;
  }
}

// (protected) alternate constructor
function bnpFromNumber(a,b,c) {
  if("number" == typeof b) {
    // new BigInteger(int,int,RNG)
    if(a < 2) this.fromInt(1);
    else {
      this.fromNumber(a,c);
      if(!this.testBit(a-1))	// force MSB set
        this.bitwiseTo(BigInteger.ONE.shiftLeft(a-1),op_or,this);
      if(this.isEven()) this.dAddOffset(1,0); // force odd
      while(!this.isProbablePrime(b)) {
        this.dAddOffset(2,0);
        if(this.bitLength() > a) this.subTo(BigInteger.ONE.shiftLeft(a-1),this);
      }
    }
  }
  else {
    // new BigInteger(int,RNG)
    var x = new Array(), t = a&7;
    x.length = (a>>3)+1;
    b.nextBytes(x);
    if(t > 0) x[0] &= ((1<<t)-1); else x[0] = 0;
    this.fromString(x,256);
  }
}

// (public) convert to bigendian byte array
function bnToByteArray() {
  var i = this.t, r = new Array();
  r[0] = this.s;
  var p = this.DB-(i*this.DB)%8, d, k = 0;
  if(i-- > 0) {
    if(p < this.DB && (d = this[i]>>p) != (this.s&this.DM)>>p)
      r[k++] = d|(this.s<<(this.DB-p));
    while(i >= 0) {
      if(p < 8) {
        d = (this[i]&((1<<p)-1))<<(8-p);
        d |= this[--i]>>(p+=this.DB-8);
      }
      else {
        d = (this[i]>>(p-=8))&0xff;
        if(p <= 0) { p += this.DB; --i; }
      }
      if((d&0x80) != 0) d |= -256;
      if(k == 0 && (this.s&0x80) != (d&0x80)) ++k;
      if(k > 0 || d != this.s) r[k++] = d;
    }
  }
  return r;
}

function bnEquals(a) { return(this.compareTo(a)==0); }
function bnMin(a) { return(this.compareTo(a)<0)?this:a; }
function bnMax(a) { return(this.compareTo(a)>0)?this:a; }

// (protected) r = this op a (bitwise)
function bnpBitwiseTo(a,op,r) {
  var i, f, m = Math.min(a.t,this.t);
  for(i = 0; i < m; ++i) r[i] = op(this[i],a[i]);
  if(a.t < this.t) {
    f = a.s&this.DM;
    for(i = m; i < this.t; ++i) r[i] = op(this[i],f);
    r.t = this.t;
  }
  else {
    f = this.s&this.DM;
    for(i = m; i < a.t; ++i) r[i] = op(f,a[i]);
    r.t = a.t;
  }
  r.s = op(this.s,a.s);
  r.clamp();
}

// (public) this & a
function op_and(x,y) { return x&y; }
function bnAnd(a) { var r = nbi(); this.bitwiseTo(a,op_and,r); return r; }

// (public) this | a
function op_or(x,y) { return x|y; }
function bnOr(a) { var r = nbi(); this.bitwiseTo(a,op_or,r); return r; }

// (public) this ^ a
function op_xor(x,y) { return x^y; }
function bnXor(a) { var r = nbi(); this.bitwiseTo(a,op_xor,r); return r; }

// (public) this & ~a
function op_andnot(x,y) { return x&~y; }
function bnAndNot(a) { var r = nbi(); this.bitwiseTo(a,op_andnot,r); return r; }

// (public) ~this
function bnNot() {
  var r = nbi();
  for(var i = 0; i < this.t; ++i) r[i] = this.DM&~this[i];
  r.t = this.t;
  r.s = ~this.s;
  return r;
}

// (public) this << n
function bnShiftLeft(n) {
  var r = nbi();
  if(n < 0) this.rShiftTo(-n,r); else this.lShiftTo(n,r);
  return r;
}

// (public) this >> n
function bnShiftRight(n) {
  var r = nbi();
  if(n < 0) this.lShiftTo(-n,r); else this.rShiftTo(n,r);
  return r;
}

// return index of lowest 1-bit in x, x < 2^31
function lbit(x) {
  if(x == 0) return -1;
  var r = 0;
  if((x&0xffff) == 0) { x >>= 16; r += 16; }
  if((x&0xff) == 0) { x >>= 8; r += 8; }
  if((x&0xf) == 0) { x >>= 4; r += 4; }
  if((x&3) == 0) { x >>= 2; r += 2; }
  if((x&1) == 0) ++r;
  return r;
}

// (public) returns index of lowest 1-bit (or -1 if none)
function bnGetLowestSetBit() {
  for(var i = 0; i < this.t; ++i)
    if(this[i] != 0) return i*this.DB+lbit(this[i]);
  if(this.s < 0) return this.t*this.DB;
  return -1;
}

// return number of 1 bits in x
function cbit(x) {
  var r = 0;
  while(x != 0) { x &= x-1; ++r; }
  return r;
}

// (public) return number of set bits
function bnBitCount() {
  var r = 0, x = this.s&this.DM;
  for(var i = 0; i < this.t; ++i) r += cbit(this[i]^x);
  return r;
}

// (public) true iff nth bit is set
function bnTestBit(n) {
  var j = Math.floor(n/this.DB);
  if(j >= this.t) return(this.s!=0);
  return((this[j]&(1<<(n%this.DB)))!=0);
}

// (protected) this op (1<<n)
function bnpChangeBit(n,op) {
  var r = BigInteger.ONE.shiftLeft(n);
  this.bitwiseTo(r,op,r);
  return r;
}

// (public) this | (1<<n)
function bnSetBit(n) { return this.changeBit(n,op_or); }

// (public) this & ~(1<<n)
function bnClearBit(n) { return this.changeBit(n,op_andnot); }

// (public) this ^ (1<<n)
function bnFlipBit(n) { return this.changeBit(n,op_xor); }

// (protected) r = this + a
function bnpAddTo(a,r) {
  var i = 0, c = 0, m = Math.min(a.t,this.t);
  while(i < m) {
    c += this[i]+a[i];
    r[i++] = c&this.DM;
    c >>= this.DB;
  }
  if(a.t < this.t) {
    c += a.s;
    while(i < this.t) {
      c += this[i];
      r[i++] = c&this.DM;
      c >>= this.DB;
    }
    c += this.s;
  }
  else {
    c += this.s;
    while(i < a.t) {
      c += a[i];
      r[i++] = c&this.DM;
      c >>= this.DB;
    }
    c += a.s;
  }
  r.s = (c<0)?-1:0;
  if(c > 0) r[i++] = c;
  else if(c < -1) r[i++] = this.DV+c;
  r.t = i;
  r.clamp();
}

// (public) this + a
function bnAdd(a) { var r = nbi(); this.addTo(a,r); return r; }

// (public) this - a
function bnSubtract(a) { var r = nbi(); this.subTo(a,r); return r; }

// (public) this * a
function bnMultiply(a) { var r = nbi(); this.multiplyTo(a,r); return r; }

// (public) this / a
function bnDivide(a) { var r = nbi(); this.divRemTo(a,r,null); return r; }

// (public) this % a
function bnRemainder(a) { var r = nbi(); this.divRemTo(a,null,r); return r; }

// (public) [this/a,this%a]
function bnDivideAndRemainder(a) {
  var q = nbi(), r = nbi();
  this.divRemTo(a,q,r);
  return new Array(q,r);
}

// (protected) this *= n, this >= 0, 1 < n < DV
function bnpDMultiply(n) {
  this[this.t] = this.am(0,n-1,this,0,0,this.t);
  ++this.t;
  this.clamp();
}

// (protected) this += n << w words, this >= 0
function bnpDAddOffset(n,w) {
  if(n == 0) return;
  while(this.t <= w) this[this.t++] = 0;
  this[w] += n;
  while(this[w] >= this.DV) {
    this[w] -= this.DV;
    if(++w >= this.t) this[this.t++] = 0;
    ++this[w];
  }
}

// A "null" reducer
function NullExp() {}
function nNop(x) { return x; }
function nMulTo(x,y,r) { x.multiplyTo(y,r); }
function nSqrTo(x,r) { x.squareTo(r); }

NullExp.prototype.convert = nNop;
NullExp.prototype.revert = nNop;
NullExp.prototype.mulTo = nMulTo;
NullExp.prototype.sqrTo = nSqrTo;

// (public) this^e
function bnPow(e) { return this.exp(e,new NullExp()); }

// (protected) r = lower n words of "this * a", a.t <= n
// "this" should be the larger one if appropriate.
function bnpMultiplyLowerTo(a,n,r) {
  var i = Math.min(this.t+a.t,n);
  r.s = 0; // assumes a,this >= 0
  r.t = i;
  while(i > 0) r[--i] = 0;
  var j;
  for(j = r.t-this.t; i < j; ++i) r[i+this.t] = this.am(0,a[i],r,i,0,this.t);
  for(j = Math.min(a.t,n); i < j; ++i) this.am(0,a[i],r,i,0,n-i);
  r.clamp();
}

// (protected) r = "this * a" without lower n words, n > 0
// "this" should be the larger one if appropriate.
function bnpMultiplyUpperTo(a,n,r) {
  --n;
  var i = r.t = this.t+a.t-n;
  r.s = 0; // assumes a,this >= 0
  while(--i >= 0) r[i] = 0;
  for(i = Math.max(n-this.t,0); i < a.t; ++i)
    r[this.t+i-n] = this.am(n-i,a[i],r,0,0,this.t+i-n);
  r.clamp();
  r.drShiftTo(1,r);
}

// Barrett modular reduction
function Barrett(m) {
  // setup Barrett
  this.r2 = nbi();
  this.q3 = nbi();
  BigInteger.ONE.dlShiftTo(2*m.t,this.r2);
  this.mu = this.r2.divide(m);
  this.m = m;
}

function barrettConvert(x) {
  if(x.s < 0 || x.t > 2*this.m.t) return x.mod(this.m);
  else if(x.compareTo(this.m) < 0) return x;
  else { var r = nbi(); x.copyTo(r); this.reduce(r); return r; }
}

function barrettRevert(x) { return x; }

// x = x mod m (HAC 14.42)
function barrettReduce(x) {
  x.drShiftTo(this.m.t-1,this.r2);
  if(x.t > this.m.t+1) { x.t = this.m.t+1; x.clamp(); }
  this.mu.multiplyUpperTo(this.r2,this.m.t+1,this.q3);
  this.m.multiplyLowerTo(this.q3,this.m.t+1,this.r2);
  while(x.compareTo(this.r2) < 0) x.dAddOffset(1,this.m.t+1);
  x.subTo(this.r2,x);
  while(x.compareTo(this.m) >= 0) x.subTo(this.m,x);
}

// r = x^2 mod m; x != r
function barrettSqrTo(x,r) { x.squareTo(r); this.reduce(r); }

// r = x*y mod m; x,y != r
function barrettMulTo(x,y,r) { x.multiplyTo(y,r); this.reduce(r); }

Barrett.prototype.convert = barrettConvert;
Barrett.prototype.revert = barrettRevert;
Barrett.prototype.reduce = barrettReduce;
Barrett.prototype.mulTo = barrettMulTo;
Barrett.prototype.sqrTo = barrettSqrTo;

// (public) this^e % m (HAC 14.85)
function bnModPow(e,m) {
  var i = e.bitLength(), k, r = nbv(1), z;
  if(i <= 0) return r;
  else if(i < 18) k = 1;
  else if(i < 48) k = 3;
  else if(i < 144) k = 4;
  else if(i < 768) k = 5;
  else k = 6;
  if(i < 8)
    z = new Classic(m);
  else if(m.isEven())
    z = new Barrett(m);
  else
    z = new Montgomery(m);

  // precomputation
  var g = new Array(), n = 3, k1 = k-1, km = (1<<k)-1;
  g[1] = z.convert(this);
  if(k > 1) {
    var g2 = nbi();
    z.sqrTo(g[1],g2);
    while(n <= km) {
      g[n] = nbi();
      z.mulTo(g2,g[n-2],g[n]);
      n += 2;
    }
  }

  var j = e.t-1, w, is1 = true, r2 = nbi(), t;
  i = nbits(e[j])-1;
  while(j >= 0) {
    if(i >= k1) w = (e[j]>>(i-k1))&km;
    else {
      w = (e[j]&((1<<(i+1))-1))<<(k1-i);
      if(j > 0) w |= e[j-1]>>(this.DB+i-k1);
    }

    n = k;
    while((w&1) == 0) { w >>= 1; --n; }
    if((i -= n) < 0) { i += this.DB; --j; }
    if(is1) {	// ret == 1, don't bother squaring or multiplying it
      g[w].copyTo(r);
      is1 = false;
    }
    else {
      while(n > 1) { z.sqrTo(r,r2); z.sqrTo(r2,r); n -= 2; }
      if(n > 0) z.sqrTo(r,r2); else { t = r; r = r2; r2 = t; }
      z.mulTo(r2,g[w],r);
    }

    while(j >= 0 && (e[j]&(1<<i)) == 0) {
      z.sqrTo(r,r2); t = r; r = r2; r2 = t;
      if(--i < 0) { i = this.DB-1; --j; }
    }
  }
  return z.revert(r);
}

// (public) gcd(this,a) (HAC 14.54)
function bnGCD(a) {
  var x = (this.s<0)?this.negate():this.clone();
  var y = (a.s<0)?a.negate():a.clone();
  if(x.compareTo(y) < 0) { var t = x; x = y; y = t; }
  var i = x.getLowestSetBit(), g = y.getLowestSetBit();
  if(g < 0) return x;
  if(i < g) g = i;
  if(g > 0) {
    x.rShiftTo(g,x);
    y.rShiftTo(g,y);
  }
  while(x.signum() > 0) {
    if((i = x.getLowestSetBit()) > 0) x.rShiftTo(i,x);
    if((i = y.getLowestSetBit()) > 0) y.rShiftTo(i,y);
    if(x.compareTo(y) >= 0) {
      x.subTo(y,x);
      x.rShiftTo(1,x);
    }
    else {
      y.subTo(x,y);
      y.rShiftTo(1,y);
    }
  }
  if(g > 0) y.lShiftTo(g,y);
  return y;
}

// (protected) this % n, n < 2^26
function bnpModInt(n) {
  if(n <= 0) return 0;
  var d = this.DV%n, r = (this.s<0)?n-1:0;
  if(this.t > 0)
    if(d == 0) r = this[0]%n;
    else for(var i = this.t-1; i >= 0; --i) r = (d*r+this[i])%n;
  return r;
}

// (public) 1/this % m (HAC 14.61)
function bnModInverse(m) {
  var ac = m.isEven();
  if((this.isEven() && ac) || m.signum() == 0) return BigInteger.ZERO;
  var u = m.clone(), v = this.clone();
  var a = nbv(1), b = nbv(0), c = nbv(0), d = nbv(1);
  while(u.signum() != 0) {
    while(u.isEven()) {
      u.rShiftTo(1,u);
      if(ac) {
        if(!a.isEven() || !b.isEven()) { a.addTo(this,a); b.subTo(m,b); }
        a.rShiftTo(1,a);
      }
      else if(!b.isEven()) b.subTo(m,b);
      b.rShiftTo(1,b);
    }
    while(v.isEven()) {
      v.rShiftTo(1,v);
      if(ac) {
        if(!c.isEven() || !d.isEven()) { c.addTo(this,c); d.subTo(m,d); }
        c.rShiftTo(1,c);
      }
      else if(!d.isEven()) d.subTo(m,d);
      d.rShiftTo(1,d);
    }
    if(u.compareTo(v) >= 0) {
      u.subTo(v,u);
      if(ac) a.subTo(c,a);
      b.subTo(d,b);
    }
    else {
      v.subTo(u,v);
      if(ac) c.subTo(a,c);
      d.subTo(b,d);
    }
  }
  if(v.compareTo(BigInteger.ONE) != 0) return BigInteger.ZERO;
  if(d.compareTo(m) >= 0) return d.subtract(m);
  if(d.signum() < 0) d.addTo(m,d); else return d;
  if(d.signum() < 0) return d.add(m); else return d;
}

var lowprimes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509];
var lplim = (1<<26)/lowprimes[lowprimes.length-1];

// (public) test primality with certainty >= 1-.5^t
function bnIsProbablePrime(t) {
  var i, x = this.abs();
  if(x.t == 1 && x[0] <= lowprimes[lowprimes.length-1]) {
    for(i = 0; i < lowprimes.length; ++i)
      if(x[0] == lowprimes[i]) return true;
    return false;
  }
  if(x.isEven()) return false;
  i = 1;
  while(i < lowprimes.length) {
    var m = lowprimes[i], j = i+1;
    while(j < lowprimes.length && m < lplim) m *= lowprimes[j++];
    m = x.modInt(m);
    while(i < j) if(m%lowprimes[i++] == 0) return false;
  }
  return x.millerRabin(t);
}

// (protected) true if probably prime (HAC 4.24, Miller-Rabin)
function bnpMillerRabin(t) {
  var n1 = this.subtract(BigInteger.ONE);
  var k = n1.getLowestSetBit();
  if(k <= 0) return false;
  var r = n1.shiftRight(k);
  t = (t+1)>>1;
  if(t > lowprimes.length) t = lowprimes.length;
  var a = nbi();
  for(var i = 0; i < t; ++i) {
    a.fromInt(lowprimes[i]);
    var y = a.modPow(r,this);
    if(y.compareTo(BigInteger.ONE) != 0 && y.compareTo(n1) != 0) {
      var j = 1;
      while(j++ < k && y.compareTo(n1) != 0) {
        y = y.modPowInt(2,this);
        if(y.compareTo(BigInteger.ONE) == 0) return false;
      }
      if(y.compareTo(n1) != 0) return false;
    }
  }
  return true;
}

// protected
BigInteger.prototype.chunkSize = bnpChunkSize;
BigInteger.prototype.toRadix = bnpToRadix;
BigInteger.prototype.fromRadix = bnpFromRadix;
BigInteger.prototype.fromDouble = bnpFromDouble;
BigInteger.prototype.fromNumber = bnpFromNumber;
BigInteger.prototype.bitwiseTo = bnpBitwiseTo;
BigInteger.prototype.changeBit = bnpChangeBit;
BigInteger.prototype.addTo = bnpAddTo;
BigInteger.prototype.dMultiply = bnpDMultiply;
BigInteger.prototype.dAddOffset = bnpDAddOffset;
BigInteger.prototype.multiplyLowerTo = bnpMultiplyLowerTo;
BigInteger.prototype.multiplyUpperTo = bnpMultiplyUpperTo;
BigInteger.prototype.modInt = bnpModInt;
BigInteger.prototype.millerRabin = bnpMillerRabin;

// public
BigInteger.prototype.clone = bnClone;
BigInteger.prototype.intValue = bnIntValue;
BigInteger.prototype.byteValue = bnByteValue;
BigInteger.prototype.shortValue = bnShortValue;
BigInteger.prototype.doubleValue = bnDoubleValue;
BigInteger.prototype.signum = bnSigNum;
BigInteger.prototype.toByteArray = bnToByteArray;
BigInteger.prototype.equals = bnEquals;
BigInteger.prototype.min = bnMin;
BigInteger.prototype.max = bnMax;
BigInteger.prototype.and = bnAnd;
BigInteger.prototype.or = bnOr;
BigInteger.prototype.xor = bnXor;
BigInteger.prototype.andNot = bnAndNot;
BigInteger.prototype.not = bnNot;
BigInteger.prototype.shiftLeft = bnShiftLeft;
BigInteger.prototype.shiftRight = bnShiftRight;
BigInteger.prototype.getLowestSetBit = bnGetLowestSetBit;
BigInteger.prototype.bitCount = bnBitCount;
BigInteger.prototype.testBit = bnTestBit;
BigInteger.prototype.setBit = bnSetBit;
BigInteger.prototype.clearBit = bnClearBit;
BigInteger.prototype.flipBit = bnFlipBit;
BigInteger.prototype.add = bnAdd;
BigInteger.prototype.subtract = bnSubtract;
BigInteger.prototype.multiply = bnMultiply;
BigInteger.prototype.divide = bnDivide;
BigInteger.prototype.remainder = bnRemainder;
BigInteger.prototype.divideAndRemainder = bnDivideAndRemainder;
BigInteger.prototype.modPow = bnModPow;
BigInteger.prototype.modInverse = bnModInverse;
BigInteger.prototype.pow = bnPow;
BigInteger.prototype.gcd = bnGCD;
BigInteger.prototype.isProbablePrime = bnIsProbablePrime;

// BigInteger interfaces not implemented in jsbn:

// BigInteger(int signum, byte[] magnitude)
// double doubleValue()
// float floatValue()
// int hashCode()
// long longValue()
// static BigInteger valueOf(long val)
// Copyright (c) 2005  Tom Wu
// All Rights Reserved.
// See "LICENSE" for details.

// Basic JavaScript BN library - subset useful for RSA encryption.

// Bits per digit
var dbits;

// JavaScript engine analysis
var canary = 0xdeadbeefcafe;
var j_lm = ((canary&0xffffff)==0xefcafe);

// (public) Constructor
function BigInteger(a,b,c) {
  if(a != null)
    if("number" == typeof a) this.fromNumber(a,b,c);
    else if(b == null && "string" != typeof a) this.fromString(a,256);
    else this.fromString(a,b);
}

// return new, unset BigInteger
function nbi() { return new BigInteger(null); }

// am: Compute w_j += (x*this_i), propagate carries,
// c is initial carry, returns final carry.
// c < 3*dvalue, x < 2*dvalue, this_i < dvalue
// We need to select the fastest one that works in this environment.

// am1: use a single mult and divide to get the high bits,
// max digit bits should be 26 because
// max internal value = 2*dvalue^2-2*dvalue (< 2^53)
function am1(i,x,w,j,c,n) {
  while(--n >= 0) {
    var v = x*this[i++]+w[j]+c;
    c = Math.floor(v/0x4000000);
    w[j++] = v&0x3ffffff;
  }
  return c;
}
// am2 avoids a big mult-and-extract completely.
// Max digit bits should be <= 30 because we do bitwise ops
// on values up to 2*hdvalue^2-hdvalue-1 (< 2^31)
function am2(i,x,w,j,c,n) {
  var xl = x&0x7fff, xh = x>>15;
  while(--n >= 0) {
    var l = this[i]&0x7fff;
    var h = this[i++]>>15;
    var m = xh*l+h*xl;
    l = xl*l+((m&0x7fff)<<15)+w[j]+(c&0x3fffffff);
    c = (l>>>30)+(m>>>15)+xh*h+(c>>>30);
    w[j++] = l&0x3fffffff;
  }
  return c;
}
// Alternately, set max digit bits to 28 since some
// browsers slow down when dealing with 32-bit numbers.
function am3(i,x,w,j,c,n) {
  var xl = x&0x3fff, xh = x>>14;
  while(--n >= 0) {
    var l = this[i]&0x3fff;
    var h = this[i++]>>14;
    var m = xh*l+h*xl;
    l = xl*l+((m&0x3fff)<<14)+w[j]+c;
    c = (l>>28)+(m>>14)+xh*h;
    w[j++] = l&0xfffffff;
  }
  return c;
}
if ( typeof navigator == 'object' ) {
  if(j_lm && (navigator.appName == "Microsoft Internet Explorer")) {
    BigInteger.prototype.am = am2;
    dbits = 30;
  }
  else if(j_lm && (navigator.appName != "Netscape")) {
    BigInteger.prototype.am = am1;
    dbits = 26;
  }
  else { // Mozilla/Netscape seems to prefer am3
    BigInteger.prototype.am = am3;
    dbits = 28;
  }
} else {
  BigInteger.prototype.am = am1;
  dbits = 26;
}

BigInteger.prototype.DB = dbits;
BigInteger.prototype.DM = ((1<<dbits)-1);
BigInteger.prototype.DV = (1<<dbits);

var BI_FP = 52;
BigInteger.prototype.FV = Math.pow(2,BI_FP);
BigInteger.prototype.F1 = BI_FP-dbits;
BigInteger.prototype.F2 = 2*dbits-BI_FP;

// Digit conversions
var BI_RM = "0123456789abcdefghijklmnopqrstuvwxyz";
var BI_RC = new Array();
var rr,vv;
rr = "0".charCodeAt(0);
for(vv = 0; vv <= 9; ++vv) BI_RC[rr++] = vv;
rr = "a".charCodeAt(0);
for(vv = 10; vv < 36; ++vv) BI_RC[rr++] = vv;
rr = "A".charCodeAt(0);
for(vv = 10; vv < 36; ++vv) BI_RC[rr++] = vv;

function int2char(n) { return BI_RM.charAt(n); }
function intAt(s,i) {
  var c = BI_RC[s.charCodeAt(i)];
  return (c==null)?-1:c;
}

// (protected) copy this to r
function bnpCopyTo(r) {
  for(var i = this.t-1; i >= 0; --i) r[i] = this[i];
  r.t = this.t;
  r.s = this.s;
}

// (protected) set from integer value x, -DV <= x < DV
function bnpFromInt(x) {
  this.t = 1;
  this.s = (x<0)?-1:0;
  if(x > 0) this[0] = x;
  else if(x < -1) this[0] = x+this.DV;
  else this.t = 0;
}

// return bigint initialized to value
function nbv(i) { var r = nbi(); r.fromInt(i); return r; }

// (protected) set from string and radix
function bnpFromString(s,b) {
  var k;
  if(b == 16) k = 4;
  else if(b == 8) k = 3;
  else if(b == 256) k = 8; // byte array
  else if(b == 2) k = 1;
  else if(b == 32) k = 5;
  else if(b == 4) k = 2;
  else { this.fromRadix(s,b); return; }
  this.t = 0;
  this.s = 0;
  var i = s.length, mi = false, sh = 0;
  while(--i >= 0) {
    var x = (k==8)?s[i]&0xff:intAt(s,i);
    if(x < 0) {
      if(s.charAt(i) == "-") mi = true;
      continue;
    }
    mi = false;
    if(sh == 0)
      this[this.t++] = x;
    else if(sh+k > this.DB) {
      this[this.t-1] |= (x&((1<<(this.DB-sh))-1))<<sh;
      this[this.t++] = (x>>(this.DB-sh));
    }
    else
      this[this.t-1] |= x<<sh;
    sh += k;
    if(sh >= this.DB) sh -= this.DB;
  }
  if(k == 8 && (s[0]&0x80) != 0) {
    this.s = -1;
    if(sh > 0) this[this.t-1] |= ((1<<(this.DB-sh))-1)<<sh;
  }
  this.clamp();
  if(mi) BigInteger.ZERO.subTo(this,this);
}

// (protected) clamp off excess high words
function bnpClamp() {
  var c = this.s&this.DM;
  while(this.t > 0 && this[this.t-1] == c) --this.t;
}

// (public) return string representation in given radix
function bnToString(b) {
  if(this.s < 0) return "-"+this.negate().toString(b);
  var k;
  if(b == 16) k = 4;
  else if(b == 8) k = 3;
  else if(b == 2) k = 1;
  else if(b == 32) k = 5;
  else if(b == 4) k = 2;
  else return this.toRadix(b);
  var km = (1<<k)-1, d, m = false, r = "", i = this.t;
  var p = this.DB-(i*this.DB)%k;
  if(i-- > 0) {
    if(p < this.DB && (d = this[i]>>p) > 0) { m = true; r = int2char(d); }
    while(i >= 0) {
      if(p < k) {
        d = (this[i]&((1<<p)-1))<<(k-p);
        d |= this[--i]>>(p+=this.DB-k);
      }
      else {
        d = (this[i]>>(p-=k))&km;
        if(p <= 0) { p += this.DB; --i; }
      }
      if(d > 0) m = true;
      if(m) r += int2char(d);
    }
  }
  return m?r:"0";
}

// (public) -this
function bnNegate() { var r = nbi(); BigInteger.ZERO.subTo(this,r); return r; }

// (public) |this|
function bnAbs() { return (this.s<0)?this.negate():this; }

// (public) return + if this > a, - if this < a, 0 if equal
function bnCompareTo(a) {
  var r = this.s-a.s;
  if(r != 0) return r;
  var i = this.t;
  r = i-a.t;
  if(r != 0) return r;
  while(--i >= 0) if((r=this[i]-a[i]) != 0) return r;
  return 0;
}

// returns bit length of the integer x
function nbits(x) {
  var r = 1, t;
  if((t=x>>>16) != 0) { x = t; r += 16; }
  if((t=x>>8) != 0) { x = t; r += 8; }
  if((t=x>>4) != 0) { x = t; r += 4; }
  if((t=x>>2) != 0) { x = t; r += 2; }
  if((t=x>>1) != 0) { x = t; r += 1; }
  return r;
}

// (public) return the number of bits in "this"
function bnBitLength() {
  if(this.t <= 0) return 0;
  return this.DB*(this.t-1)+nbits(this[this.t-1]^(this.s&this.DM));
}

// (protected) r = this << n*DB
function bnpDLShiftTo(n,r) {
  var i;
  for(i = this.t-1; i >= 0; --i) r[i+n] = this[i];
  for(i = n-1; i >= 0; --i) r[i] = 0;
  r.t = this.t+n;
  r.s = this.s;
}

// (protected) r = this >> n*DB
function bnpDRShiftTo(n,r) {
  for(var i = n; i < this.t; ++i) r[i-n] = this[i];
  r.t = Math.max(this.t-n,0);
  r.s = this.s;
}

// (protected) r = this << n
function bnpLShiftTo(n,r) {
  var bs = n%this.DB;
  var cbs = this.DB-bs;
  var bm = (1<<cbs)-1;
  var ds = Math.floor(n/this.DB), c = (this.s<<bs)&this.DM, i;
  for(i = this.t-1; i >= 0; --i) {
    r[i+ds+1] = (this[i]>>cbs)|c;
    c = (this[i]&bm)<<bs;
  }
  for(i = ds-1; i >= 0; --i) r[i] = 0;
  r[ds] = c;
  r.t = this.t+ds+1;
  r.s = this.s;
  r.clamp();
}

// (protected) r = this >> n
function bnpRShiftTo(n,r) {
  r.s = this.s;
  var ds = Math.floor(n/this.DB);
  if(ds >= this.t) { r.t = 0; return; }
  var bs = n%this.DB;
  var cbs = this.DB-bs;
  var bm = (1<<bs)-1;
  r[0] = this[ds]>>bs;
  for(var i = ds+1; i < this.t; ++i) {
    r[i-ds-1] |= (this[i]&bm)<<cbs;
    r[i-ds] = this[i]>>bs;
  }
  if(bs > 0) r[this.t-ds-1] |= (this.s&bm)<<cbs;
  r.t = this.t-ds;
  r.clamp();
}

// (protected) r = this - a
function bnpSubTo(a,r) {
  var i = 0, c = 0, m = Math.min(a.t,this.t);
  while(i < m) {
    c += this[i]-a[i];
    r[i++] = c&this.DM;
    c >>= this.DB;
  }
  if(a.t < this.t) {
    c -= a.s;
    while(i < this.t) {
      c += this[i];
      r[i++] = c&this.DM;
      c >>= this.DB;
    }
    c += this.s;
  }
  else {
    c += this.s;
    while(i < a.t) {
      c -= a[i];
      r[i++] = c&this.DM;
      c >>= this.DB;
    }
    c -= a.s;
  }
  r.s = (c<0)?-1:0;
  if(c < -1) r[i++] = this.DV+c;
  else if(c > 0) r[i++] = c;
  r.t = i;
  r.clamp();
}

// (protected) r = this * a, r != this,a (HAC 14.12)
// "this" should be the larger one if appropriate.
function bnpMultiplyTo(a,r) {
  var x = this.abs(), y = a.abs();
  var i = x.t;
  r.t = i+y.t;
  while(--i >= 0) r[i] = 0;
  for(i = 0; i < y.t; ++i) r[i+x.t] = x.am(0,y[i],r,i,0,x.t);
  r.s = 0;
  r.clamp();
  if(this.s != a.s) BigInteger.ZERO.subTo(r,r);
}

// (protected) r = this^2, r != this (HAC 14.16)
function bnpSquareTo(r) {
  var x = this.abs();
  var i = r.t = 2*x.t;
  while(--i >= 0) r[i] = 0;
  for(i = 0; i < x.t-1; ++i) {
    var c = x.am(i,x[i],r,2*i,0,1);
    if((r[i+x.t]+=x.am(i+1,2*x[i],r,2*i+1,c,x.t-i-1)) >= x.DV) {
      r[i+x.t] -= x.DV;
      r[i+x.t+1] = 1;
    }
  }
  if(r.t > 0) r[r.t-1] += x.am(i,x[i],r,2*i,0,1);
  r.s = 0;
  r.clamp();
}

// (protected) divide this by m, quotient and remainder to q, r (HAC 14.20)
// r != q, this != m.  q or r may be null.
function bnpDivRemTo(m,q,r) {
  var pm = m.abs();
  if(pm.t <= 0) return;
  var pt = this.abs();
  if(pt.t < pm.t) {
    if(q != null) q.fromInt(0);
    if(r != null) this.copyTo(r);
    return;
  }
  if(r == null) r = nbi();
  var y = nbi(), ts = this.s, ms = m.s;
  var nsh = this.DB-nbits(pm[pm.t-1]);	// normalize modulus
  if(nsh > 0) { pm.lShiftTo(nsh,y); pt.lShiftTo(nsh,r); }
  else { pm.copyTo(y); pt.copyTo(r); }
  var ys = y.t;
  var y0 = y[ys-1];
  if(y0 == 0) return;
  var yt = y0*(1<<this.F1)+((ys>1)?y[ys-2]>>this.F2:0);
  var d1 = this.FV/yt, d2 = (1<<this.F1)/yt, e = 1<<this.F2;
  var i = r.t, j = i-ys, t = (q==null)?nbi():q;
  y.dlShiftTo(j,t);
  if(r.compareTo(t) >= 0) {
    r[r.t++] = 1;
    r.subTo(t,r);
  }
  BigInteger.ONE.dlShiftTo(ys,t);
  t.subTo(y,y);	// "negative" y so we can replace sub with am later
  while(y.t < ys) y[y.t++] = 0;
  while(--j >= 0) {
    // Estimate quotient digit
    var qd = (r[--i]==y0)?this.DM:Math.floor(r[i]*d1+(r[i-1]+e)*d2);
    if((r[i]+=y.am(0,qd,r,j,0,ys)) < qd) {	// Try it out
      y.dlShiftTo(j,t);
      r.subTo(t,r);
      while(r[i] < --qd) r.subTo(t,r);
    }
  }
  if(q != null) {
    r.drShiftTo(ys,q);
    if(ts != ms) BigInteger.ZERO.subTo(q,q);
  }
  r.t = ys;
  r.clamp();
  if(nsh > 0) r.rShiftTo(nsh,r);	// Denormalize remainder
  if(ts < 0) BigInteger.ZERO.subTo(r,r);
}

// (public) this mod a
function bnMod(a) {
  var r = nbi();
  this.abs().divRemTo(a,null,r);
  if(this.s < 0 && r.compareTo(BigInteger.ZERO) > 0) a.subTo(r,r);
  return r;
}

// Modular reduction using "classic" algorithm
function Classic(m) { this.m = m; }
function cConvert(x) {
  if(x.s < 0 || x.compareTo(this.m) >= 0) return x.mod(this.m);
  else return x;
}
function cRevert(x) { return x; }
function cReduce(x) { x.divRemTo(this.m,null,x); }
function cMulTo(x,y,r) { x.multiplyTo(y,r); this.reduce(r); }
function cSqrTo(x,r) { x.squareTo(r); this.reduce(r); }

Classic.prototype.convert = cConvert;
Classic.prototype.revert = cRevert;
Classic.prototype.reduce = cReduce;
Classic.prototype.mulTo = cMulTo;
Classic.prototype.sqrTo = cSqrTo;

// (protected) return "-1/this % 2^DB"; useful for Mont. reduction
// justification:
//         xy == 1 (mod m)
//         xy =  1+km
//   xy(2-xy) = (1+km)(1-km)
// x[y(2-xy)] = 1-k^2m^2
// x[y(2-xy)] == 1 (mod m^2)
// if y is 1/x mod m, then y(2-xy) is 1/x mod m^2
// should reduce x and y(2-xy) by m^2 at each step to keep size bounded.
// JS multiply "overflows" differently from C/C++, so care is needed here.
function bnpInvDigit() {
  if(this.t < 1) return 0;
  var x = this[0];
  if((x&1) == 0) return 0;
  var y = x&3;		// y == 1/x mod 2^2
  y = (y*(2-(x&0xf)*y))&0xf;	// y == 1/x mod 2^4
  y = (y*(2-(x&0xff)*y))&0xff;	// y == 1/x mod 2^8
  y = (y*(2-(((x&0xffff)*y)&0xffff)))&0xffff;	// y == 1/x mod 2^16
  // last step - calculate inverse mod DV directly;
  // assumes 16 < DB <= 32 and assumes ability to handle 48-bit ints
  y = (y*(2-x*y%this.DV))%this.DV;		// y == 1/x mod 2^dbits
  // we really want the negative inverse, and -DV < y < DV
  return (y>0)?this.DV-y:-y;
}

// Montgomery reduction
function Montgomery(m) {
  this.m = m;
  this.mp = m.invDigit();
  this.mpl = this.mp&0x7fff;
  this.mph = this.mp>>15;
  this.um = (1<<(m.DB-15))-1;
  this.mt2 = 2*m.t;
}

// xR mod m
function montConvert(x) {
  var r = nbi();
  x.abs().dlShiftTo(this.m.t,r);
  r.divRemTo(this.m,null,r);
  if(x.s < 0 && r.compareTo(BigInteger.ZERO) > 0) this.m.subTo(r,r);
  return r;
}

// x/R mod m
function montRevert(x) {
  var r = nbi();
  x.copyTo(r);
  this.reduce(r);
  return r;
}

// x = x/R mod m (HAC 14.32)
function montReduce(x) {
  while(x.t <= this.mt2)	// pad x so am has enough room later
    x[x.t++] = 0;
  for(var i = 0; i < this.m.t; ++i) {
    // faster way of calculating u0 = x[i]*mp mod DV
    var j = x[i]&0x7fff;
    var u0 = (j*this.mpl+(((j*this.mph+(x[i]>>15)*this.mpl)&this.um)<<15))&x.DM;
    // use am to combine the multiply-shift-add into one call
    j = i+this.m.t;
    x[j] += this.m.am(0,u0,x,i,0,this.m.t);
    // propagate carry
    while(x[j] >= x.DV) { x[j] -= x.DV; x[++j]++; }
  }
  x.clamp();
  x.drShiftTo(this.m.t,x);
  if(x.compareTo(this.m) >= 0) x.subTo(this.m,x);
}

// r = "x^2/R mod m"; x != r
function montSqrTo(x,r) { x.squareTo(r); this.reduce(r); }

// r = "xy/R mod m"; x,y != r
function montMulTo(x,y,r) { x.multiplyTo(y,r); this.reduce(r); }

Montgomery.prototype.convert = montConvert;
Montgomery.prototype.revert = montRevert;
Montgomery.prototype.reduce = montReduce;
Montgomery.prototype.mulTo = montMulTo;
Montgomery.prototype.sqrTo = montSqrTo;

// (protected) true iff this is even
function bnpIsEven() { return ((this.t>0)?(this[0]&1):this.s) == 0; }

// (protected) this^e, e < 2^32, doing sqr and mul with "r" (HAC 14.79)
function bnpExp(e,z) {
  if(e > 0xffffffff || e < 1) return BigInteger.ONE;
  var r = nbi(), r2 = nbi(), g = z.convert(this), i = nbits(e)-1;
  g.copyTo(r);
  while(--i >= 0) {
    z.sqrTo(r,r2);
    if((e&(1<<i)) > 0) z.mulTo(r2,g,r);
    else { var t = r; r = r2; r2 = t; }
  }
  return z.revert(r);
}

// (public) this^e % m, 0 <= e < 2^32
function bnModPowInt(e,m) {
  var z;
  if(e < 256 || m.isEven()) z = new Classic(m); else z = new Montgomery(m);
  return this.exp(e,z);
}

// protected
BigInteger.prototype.copyTo = bnpCopyTo;
BigInteger.prototype.fromInt = bnpFromInt;
BigInteger.prototype.fromString = bnpFromString;
BigInteger.prototype.clamp = bnpClamp;
BigInteger.prototype.dlShiftTo = bnpDLShiftTo;
BigInteger.prototype.drShiftTo = bnpDRShiftTo;
BigInteger.prototype.lShiftTo = bnpLShiftTo;
BigInteger.prototype.rShiftTo = bnpRShiftTo;
BigInteger.prototype.subTo = bnpSubTo;
BigInteger.prototype.multiplyTo = bnpMultiplyTo;
BigInteger.prototype.squareTo = bnpSquareTo;
BigInteger.prototype.divRemTo = bnpDivRemTo;
BigInteger.prototype.invDigit = bnpInvDigit;
BigInteger.prototype.isEven = bnpIsEven;
BigInteger.prototype.exp = bnpExp;

// public
BigInteger.prototype.toString = bnToString;
BigInteger.prototype.negate = bnNegate;
BigInteger.prototype.abs = bnAbs;
BigInteger.prototype.compareTo = bnCompareTo;
BigInteger.prototype.bitLength = bnBitLength;
BigInteger.prototype.mod = bnMod;
BigInteger.prototype.modPowInt = bnModPowInt;

// "constants"
BigInteger.ZERO = nbv(0);
BigInteger.ONE = nbv(1);

// interface to eval
function _e_(x) {
  var x_, xx, x_next;
  if (x !== undefined && x !== null && x.__eOrV__ !== undefined) {
    x_ = x;
    do {
      if (typeof x.__eOrV__ === 'function' && !x.fe) {
        xx = x.__eOrV__();
        x.__eOrV__ = xx;
        x = xx;
      } else {
        x = x.__eOrV__;
      }
    } while (x !== undefined && x !== null && x.__eOrV__ !== undefined);
    while (x_ !== undefined && x_ !== null && x_.__eOrV__ !== undefined) {
      x_next = x_.__eOrV__;
      x_.__eOrV__ = x;
      x_.fe = true;
      x_ = x_next;
    }
  }
  return x;
}

function _A_undersat_(fun, args) {
  // this.needs = fun.needs - args.length;
  this.fun = fun;
  this.args = args;
}

// Apply node, not enough args
_A_undersat_.prototype = {
  __aN__ : function (args) {
    var needs, fun;
    needs = this.needsNrArgs();
    if (args.length < needs) {
      return new _A_undersat_(this, args);
    } else if (args.length === needs) {
      return this.fun.__aN__(this.args.concat(args));
    } else {
      fun = _e_(this.__aN__(args.slice(0, needs)));
      return {
        __eOrV__ : function () {
          return fun.__aN__(args.slice(needs));
        }
      };
    }
  },
  needsNrArgs : function () {
    return this.fun.needsNrArgs() - this.args.length;
  },
};

// Apply node, unknown how much is missing or too much
_A_.prototype = {
  __aN__ : function (args) {
    var fun = _e_(this);
    return {
      __eOrV__ : function () {
        return fun.__aN__(args);
      }
    };
  },
};
function _A_(fun, args) {
  this.__eOrV__ = function () {
    var x = fun.__aN__(args);
    return x;
  };
  this.fe = false;
}

function _F_(evalN) {
  //this.needs = evalN.length;
  this.__evN__ = evalN;
}

// Function node
_F_.prototype = {
  __aN__ : function (args) {
    var x, fun, remargs;
    if (args.length < this.__evN__.length) {
      return new _A_undersat_(this, args);
    } else if (args.length === this.__evN__.length) {
      x = this.__evN__.apply(null, args);
      return x;
    } else {
      fun = _e_(this.__evN__.apply(null, args.slice(0, this.__evN__.length)));
      remargs = args.slice(this.__evN__.length);
      return {
        __eOrV__ : function () {
          return fun.__aN__(remargs);
        }
      };
    }
  },
  needsNrArgs : function () {
    return this.__evN__.length;
  },
}

// lazy application wrappers
function _a0_(f) {
  return new _A_(f, []);
}

// indirection
function _i_() {
  return new _A_(new _F_(
    function () {throw "_i_: attempt to prematurely evaluate indirection"; }
  ), []);
}

function _i_set_(i, x) {
  i.__eOrV__ = x;
}

if (typeof document !== 'object') {
  document = {
    write   : function (x) {return write(x); },
    writeln : function (x) {return writeln(x); }
  };
};

PrimDataOrdering_EQ = {_tag_ : 0}
PrimDataOrdering_GT = {_tag_ : 1}
PrimDataOrdering_LT = {_tag_ : 2}

PrimDataBool_False = {_tag_ : 0}
PrimDataBool_True  = {_tag_ : 1}

PrimDataList_Nil = {_tag_ : 1}
PrimDataList_Cons  = {_tag_ : 0}

PrimMkBool = function(x) {
  return ( (x) ? PrimDataBool_True : PrimDataBool_False ) ;
}

// signed, int
primAddInt = function(x,y) {
  return x+y ;
}
primSubInt = function(x,y) {
  return x-y ;
}
primMulInt = function(x,y) {
  return x*y ;
}

// primDivInt = function(x,y) {var r = primQuotInt(x,y) ; return ( (r<0) ? r-1 : r ) ;}
primDivInt = function(x,y) {
  return Math.floor(x/y) ;
}
primModInt = function(x,y) {
  var r = x%y ;
  return ( (r > 0 && y < 0 || r < 0 && y > 0) ? r+y : r ) ;
}
primDivModInt = function(x,y) {
  return [primDivInt (x,y), primModInt(x,y)] ;
}

// primQuotInt = function(x,y) {return Math.floor(x/y) ;}
primQuotInt = function(x,y) {
  var r = primDivInt(x,y) ;
  return ( (r<0) ? r+1 : r ) ;
}
primRemInt = function(x,y) {
  return x%y ;
}
primQuotRemInt = function(x,y) {
  return [primQuotInt(x,y), x%y] ;
}

primNegInt = function(x) {
  return -x ;
}
primComplementInt = function(x) {
  return ~x ;
}

primShiftLeftInt  = function(x,y) {
  return x<<y ;
}
primShiftRightInt = function(x,y) {
  return x>>y ;
}

primRotateLeftInt  = function(x,y) {
  var s = (x<0 ? -1 : 1) ;
  x=x*s ;
  return s * ((x << y) | (x >> (31 - y))) ;
}
primRotateRightInt = function(x,y) {
  var s = (x<0 ? -1 : 1) ;
  x=x*s ;
  return s * ((x >> y) | (x << (31 - y))) ;
}

primEqInt = function(x,y) {
  return PrimMkBool(x==y) ;
}
primNeInt = function(x,y) {
  return PrimMkBool(x!=y) ;
}
primLtInt = function(x,y) {
  return PrimMkBool(x< y) ;
}
primGtInt = function(x,y) {
  return PrimMkBool(x> y) ;
}
primLeInt = function(x,y) {
  return PrimMkBool(x<=y) ;
}
primGeInt = function(x,y) {
  return PrimMkBool(x>=y) ;
}

primCmpInt = function(x,y) {
  return ( (x>y) ? PrimDataOrdering_GT : ( (x<y) ? PrimDataOrdering_LT : PrimDataOrdering_EQ ) ) ;
}

/*
primMinInt = function() {return -(1<<31) ;}
primMaxInt = function() {return (1<<31)-1 ;}
*/
primMinInt = function() {return -(1<<30) ;}
primMaxInt = function() {return (1<<30)-1 ;}

primUnsafeId = function(x) {
  return x ;
}

primIntegerToInt = function(x) {
  return x.intValue() ;
}
primIntToInteger = function(x) {
  var r = nbi();
  r.fromDouble(x);
  return r;
}
// primIntToInteger = nbv ;

primAndInt = function(x,y) {
  return x&y ;
}
primOrInt  = function(x,y) {
  return x|y ;
}
primXorInt = function(x,y) {
  return x^y ;
}

// Integer
primEqInteger = function(x,y) {
  return PrimMkBool(x.compareTo(y) == 0) ;
}
primNeInteger = function(x,y) {
  return PrimMkBool(x.compareTo(y) != 0) ;
}
primLtInteger = function(x,y) {
  return PrimMkBool(x.compareTo(y) <  0) ;
}
primGtInteger = function(x,y) {
  return PrimMkBool(x.compareTo(y) >  0) ;
}
primLeInteger = function(x,y) {
  return PrimMkBool(x.compareTo(y) <= 0) ;
}
primGeInteger = function(x,y) {
  return PrimMkBool(x.compareTo(y) >= 0) ;
}

primCmpInteger = function(x,y) {
  var c=x.compareTo(y) ;
  return ( (c>0) ? PrimDataOrdering_GT : ( (c<0) ? PrimDataOrdering_LT : PrimDataOrdering_EQ ) ) ;
}
primQuotRemInteger = function(x,y) {
  var q = nbi() ;
  var r = nbi() ;
  x.divRemTo(y,q,r) ;
  return [q,r] ;
}

primDivInteger = function(  v1,  v2 ) {
	var r = v1.divide(v2) ;
	if ( r.signum() < 0 )
		return r.subtract( BigInteger.ONE ) ;
	return r ;
}
primModInteger = function(  v1,  v2 ) {
	return ( v2.signum() < 0 ? v1.mod(v2.negate()).add(v2) : v1.mod(v2) ) ;
}
primDivModInteger = function(x,y) {
  return [primDivInteger (x,y), primModInteger(x,y)] ;
}

primAndInteger = function(x,y) {
  return x.and(y) ;
}
primOrInteger  = function(x,y) {
  return x.or(y) ;
}
primXorInteger = function(x,y) {
  return x.xor(y) ;
}

primComplementInteger = function(x) {
  return x.not() ;
}

primShiftLeftInteger = function(x,y) {
  return x.shiftLeft(y) ;
}
primShiftRightInteger = function(x,y) {
  return x.shiftRight(y) ;
}

primRotateLeftWord  = function(x,y) {
  return (x << y) | (x >> (32 - y)) ;
}
primRotateRightWord = function(x,y) {
  return (x >> y) | (x << (32 - y)) ;
}

primComplementWord = primComplementInt ;

// unsigned specific
primMinWord = function() {return 0 ;}
primMaxWord = function() {return (1<<32)-1 ;}

primAndWord = primAndInt ;
primOrWord  = primOrInt  ;
primXorWord = primXorInt ;

primShiftLeftWord  = primShiftLeftInt  ;
primShiftRightWord = primShiftRightInt ;

/// TODO: sign
primIntegerToWord = primIntegerToInt ;

// float, double
primDivideDouble = function(x,y) {
  return x/y ;
}
primRecipDouble = function(x) {
  return 1/x ;
}
primRationalToDouble = function(x) {
  var e1 = _e_(x._1);
  var e2 = _e_(x._2);
  return e1.doubleValue() / e2.doubleValue() ;
}

primSinDouble  = function(x) {
  return Math.sin(x) ;
}
primCosDouble  = function(x) {
  return Math.cos(x) ;
}
primTanDouble  = function(x) {
  return Math.tan(x) ;
}
primAsinDouble = function(x) {
  return Math.asin(x) ;
}
primAcosDouble = function(x) {
  return Math.acos(x) ;
}
primAtanDouble = function(x) {
  return Math.atan(x) ;
}
primExpDouble  = function(x) {
  return Math.exp(x) ;
}
primLogDouble  = function(x) {
  return Math.log(x) ;
}
primSqrtDouble = function(x) {
  return Math.sqrt(x) ;
}
primSinhDouble = function(x) {
  return (Math.exp(x) - Math.exp(-x))/2 ;
}
primCoshDouble = function(x) {
  return (Math.exp(x) + Math.exp(-x))/2 ;
}
primTanhDouble = function(x) {
  return primSinhDouble(x) / primCoshDouble(x) ;
}

primAtan2Double = function(x,y) {
  return Math.atan2(x,y) ;
}



primCharIsUpper = function(x) {
  return PrimMkBool(x > 64 && x < 91 ) ;
}
primCharIsLower = function(x) {
  return PrimMkBool(x > 96 && x < 123) ;
}
primCharToLower = function(charCode) {
  return String.fromCharCode(charCode).toLowerCase().charCodeAt(0);
};
primCharToUpper = function(charCode) {
  return String.fromCharCode(charCode).toUpperCase().charCodeAt(0);
};

primPackedStringNull = function(x) {
  return PrimMkBool(x.length == 0) ;
}
primPackedStringHead = function(x) {
  return x.charCodeAt(0) ;
}
primPackedStringTail = function(x) {
  return x.slice(1) ;
}
// primPackedStringToInteger = function(x) { return parseInt(x) ; }
primPackedStringToInteger = function(x) {
  return new BigInteger(x,10);
}
primStringToPackedString = function(l) {
	var pos = 0 ;
	var a = new Array() ;
	while (l._tag_ != PrimDataList_Nil._tag_) {
		a[pos] = _e_(l._1) ;
		++pos ;
		l = _e_(l._2) ;
	}
	return String.fromCharCode.apply(null,a) ;
}

primNewArray = function(len,x) {
	var a = new Array() ;
	for (var i = 0 ; i < len ; i++ ) {
		a[i] = x ;
	}
	return a ;
}
primIndexArray = function(a,i) { return a[i] ; }
primWriteArray = function(a,i,x) { a[i] = x ; return [] ; }
primSameArray = function(x,y) { return PrimMkBool(x===y) ; }

primByteArrayLength = function(x) { return x.length ; }
primByteArrayToPackedString = primUnsafeId ;

primThrowException = function(x) { throw x ; }
primExitWith = function(x) { throw "EXIT:" + x ; }

// primShowIntegerToPackedString = function(x) { return x.toString() ; }

primShowDoubleToPackedString = function(x) {
  return x.toString() ;
}
primShowFloatToPackedString = primShowDoubleToPackedString ;

// TODO:
// primShowDoubleToPackedString = primShowIntegerToPackedString
// primShowFloatToPackedString  = primShowIntegerToPackedString

// decode a double for a radix b, into (non fractional) Integer and exponent
function decodeFloat(d,b,logb) {
	var sign = 1 ;
	if ( d < 0 ) {
		sign = -1 ;
		d = -d ;
	}
	if ( d == 0 ) {
		return [primIntToInteger(d),0] ;
	}
	var m = Math.floor(d) ;
	var r = d - m ;
	var e = 0 ;
	if ( r > 0 ) {
		// scale up until no fractional part remains
		var d2 = d ;
		do {
			d = d * b ;
			e = e - logb ;
			m = Math.floor(d) ;
			r = d - m ;
		} while ( r > 0 ) ;
		// d = primIntToInteger(sign * d2).shiftLeft(logb).add( primIntToInteger(sign * r * b) ) ;
		d = primIntToInteger(d) ;
	} else {
		// scale down until a fractional part arises
		var d2, e2 ;
		do {
			d2 = d ;
			e2 = e ;
			d = d / b ;
			e = e + logb ;
			m = Math.floor(d) ;
			r = d - m ;
		} while ( r == 0 )
		d = primIntToInteger(d2) ;
		e = e2 ;
	}
	var shift = 53 - d.bitLength() ;
	if ( shift ) {
		d = d.shiftLeft(shift) ;
		e = e - shift ;
	}
	return [sign < 0 ? d.negate() : d, e] ;
}

primDecodeDouble = function(d) {
  var x = decodeFloat(d,2,1);
  return x;
}
primEncodeDouble = function(d,e) {
  return d.doubleValue() * Math.pow(2,e);
}

primIsIEEE = function() {
  return PrimDataBool_True;
}
primRadixDoubleFloat = function() {
  return 2;
}

primIsNaNDouble = function(x) {
  return PrimMkBool(x==Number.NaN);
}
primIsNegativeZeroDouble = function(x) {
  return PrimMkBool(x==-0.0);
}
primIsDenormalizedDouble = function(x) {
  return PrimDataBool_False;
}
primIsInfiniteDouble = function(x) {
  return PrimMkBool(x==Number.POSITIVE_INFINITY || x==Number.NEGATIVE_INFINITY);
}
primDigitsDouble = function() {
  return 53 ;
}
primMaxExpDouble = function() {
  return 1024 ;
}
primMinExpDouble = function() {
  return -1021 ;
}


_MutVar_id_ = 0 ;
_MutVar_.prototype = {
	// identity, a global variable for all MutVar's, used for checking identity equality because this is not offered by javascript
	_id_ : 0
}
function _MutVar_(a) {
	this._val_ = a ;
	this._id_ = ++_MutVar_id_ ;
	// this should be the _id_ of the proto, but I do something wrong:
	// this._id_ = ++this.prototype._id_ ;
}
primNewMutVar 	= function(a,s) {
  return [s,new _MutVar_(a)];
}
primReadMutVar 	= function(m,s) {
  return [s,m._val_];
}
primWriteMutVar = function(m,a,s) {
  m._val_ = a; return s;
}
primSameMutVar 	= function(m1,m2) {
  return PrimMkBool(m1._id_ === m2._id_);
}

primHPutChar = function(h,c) {
 switch(c) {
  case 10 :
   document.writeln("") ;
   break ;
  default :
   document.write(String.fromCharCode(c)) ;
   break ;
 }
 return [] ;
}

// Primitive functions for dealing with JS objects

// primMkCtor :: String -> IO (JSFunPtr c)
primMkCtor = function(nm) {
  if (typeof(window[nm]) !== 'function') {
    primSetCtor(nm, new Function());
  }
  return window[nm];
}

// primMkAnonObj :: IO (JSPtr c)
primMkAnonObj = function() { return {} }

// primMkObj :: JSString -> IO (JSPtr c)
primMkObj     = function(nm) {
  return new (primGetCtor(nm));
}

// Alias to primMkCtor
primGetCtor   = primMkCtor;

// primSetCtor :: JSString -> JSFunPtr c -> IO ()
primSetCtor   = function(nm, fn) {
  window[nm] = fn;
}

// primGetAttr :: JSString -> JSPtr c -> a
primGetAttr   = function(attr, obj) {
  return obj[attr];
}

// primSetAttr :: JSString -> a -> JSPtr c -> IO (JSPtr c)
primSetAttr   = function(attr, val, obj) {
  obj[attr] = val; return obj;
}

// primPureSetAttr :: JSString -> a -> JSPtr c -> JSPtr c
primPureSetAttr = function(attr, val, obj) {
  var clone = primClone(obj);
  primSetAttr(attr, val, clone);
  return clone;
}

// primModAttr :: JSString -> (a -> b) -> JSPtr c -> IO (JSPtr c)
primModAttr   = function (attr, f, obj) {
  primSetAttr(attr, _e_(new _A_(f, [primGetAttr(attr, obj)])), obj);
  return obj;
}

// primPureModAttr :: JSString -> (a -> b) -> JSPtr c -> JSPtr c
primPureModAttr   = function (attr, f, obj) {
  var clone = primClone(obj);
  primModAttr(attr, f, clone);
  return clone;
}


// primGetProtoAttr :: JSString -> JSString -> IO a
primGetProtoAttr = function(attr, cls) {
  primMkCtor(cls);
  return window[cls].prototype[attr];
}

// primSetProtoAttr :: JSString -> a -> JSString -> IO ()
primSetProtoAttr = function(attr, val, cls) {
  primMkCtor(cls);
  window[cls].prototype[attr] = val;
}

// primModProtoAttr :: JSString -> (a -> b) -> JSString -> IO ()
primModProtoAttr = function(attr, f, cls) {
  primSetProtoAttr(attr, _e_(new _A_(f, [primGetProtoAttr(attr, cls)])), cls);
}

// Object cloning facilities

// Clones a JS object
// primClone :: JSPtr a -> JSPtr a
primClone = function(obj) {
  var cloneAlg = function(name, target, copy) {
    target[ name ] = copy;
  };
  return foldObj(cloneAlg, {}, obj, false);
}

// Converts a UHC JS datatype object to a plain JS object
// primToPlainObj :: JSPtr a -> JSPtr b
primToPlainObj = function ( obj ) {
  var toPlainAlg = function(name, target, copy) {
    if (name != "_tag_") {
      target[name] = _e_(copy);
    }
  };
  return foldObj(toPlainAlg, {}, obj, true);
};

foldObj = function (alg, target, original, deep) {
  var name, src, copy, copyIsArray, clone;

  // Extend the base object
  for ( name in original ) {
    src = target[ name ];
    copy = original[ name ];

    // Prevent never-ending loop
    if ( target === copy ) {
      continue;
    }

    // Recurse if we're merging plain objects or arrays
    if (deep && copy && ( isPlainObject(copy) || (copyIsArray = isArray(copy)) ) ) {
      if ( copyIsArray ) {
        copyIsArray = false;
        clone = src && isArray(src) ? src : [];
      } else {
        clone = src && isPlainObject(src) ? src : {};
      }

      // Never move original objects, clone them
      target[ name ] = foldObj(alg, clone, copy, deep);

    // Don't bring in undefined values
    } else if ( copy !== undefined ) {
      alg(name, target, copy);
    }
  }

  // Return the modified object
  return target;
};

type = function( obj ) {
  return obj == null ? String( obj ) : "object";
};

isArray = Array.isArray || function( obj ) {
  return type(obj) === "array";
};

isWindow = function( obj ) {
  return obj && typeof obj === "object" && "setInterval" in obj;
};

isPlainObject = function( obj ) {
  // Must be an Object.
  // Because of IE, we also have to check the presence of the constructor property.
  // Make sure that DOM nodes and window objects don't pass through, as well
  if ( !obj || type(obj) !== "object" || obj.nodeType || isWindow( obj ) ) {
    return false;
  }

  try {
    // Not own constructor property must be Object
    if ( obj.constructor &&
      !hasOwn.call(obj, "constructor") &&
      !hasOwn.call(obj.constructor.prototype, "isPrototypeOf") ) {
      return false;
    }
  } catch ( e ) {
    // IE8,9 Will throw exceptions on certain host objects #9897
    return false;
  }

  // Own properties are enumerated firstly, so to speed up,
  // if last one is own, then all properties are own.

  var key;
  for ( key in obj ) {}

  return key === undefined || hasOwn.call( obj, key );
}

function wrappedThis(cps) {
  return function() {
    var args = Array.prototype.slice.call(arguments);
    args.unshift(this);
    return cps.apply(this, args);
  }
}

primIsFunction = function(a) {
  return PrimMkBool(typeof a === "function");
}

primIsBool = function(a) {
  return PrimMkBool(typeof a === "boolean" || _primIsA(a, Boolean));
}

_primIsNumber = function(a) {
  return typeof a === "number" || _primIsA(a, Number);
}

primIsNumber = function(a) {
  return PrimMkBool(_primIsNumber(a));
}

_primIsString = function(a) {
  return typeof a === "string" || _primIsA(a, String);
}

primIsString = function(a) {
  return PrimMkBool(_primIsString(a));
}

primIsChar = function(a) {
  return PrimMkBool(_primIsString(a) && a.length == 1);
}

primIsInt = function(a) {
  return PrimMkBool(_primIsNumber(a) && parseFloat(a) == parseInt(a, 10) && !isNaN(a));
}

primIsDouble = function(a) {
  return PrimMkBool(_primIsNumber(a) && parseFloat(a) != parseInt(a, 10) && !isNaN(a));
}

primIsNull = function(a) {
  //typeof does not work, known bug.
  return PrimMkBool(a === null);
}

primIsUndefined = function(a) {
  return PrimMkBool(typeof a === "undefined");
}

primIsObject = function(a) {
  return PrimMkBool(typeof a === "object" && a !== null);
}

_primIsA = function(a, b) {
  //if a isObject and b isFunction
  if(typeof a === "object" && a !== null && typeof b === "function") {
    return a.constructor == b;
  }
  return false;
}

primIsA = function(a, b) {
  return PrimMkBool(_primIsA(a,b));
}

primInstanceOf = function(a, b) {
  if(typeof a === "object" && typeof b === "function") {
    return PrimMkBool(a instanceof b);
  }
  return PrimMkBool(false);
}

primEq = function(a, b) {
  return PrimMkBool(a == b);
}

primCharToUpper = function(c) {
  return String.fromCharCode(c).toUpperCase().charCodeAt(0);
}
// Asteroids
var $Language=
 ($Language ? $Language : {});
$Language.$UHC=
 ($Language.$UHC ? $Language.$UHC : {});
var $Language=
 ($Language ? $Language : {});
var $Graphics=
 ($Graphics ? $Graphics : {});
$Language.$UHC.$JS=
 ($Language.$UHC.$JS ? $Language.$UHC.$JS : {});
$Language.$UHC=
 ($Language.$UHC ? $Language.$UHC : {});
$Graphics.$UI=
 ($Graphics.$UI ? $Graphics.$UI : {});
var $Graphics=
 ($Graphics ? $Graphics : {});
var $UHC=
 ($UHC ? $UHC : {});
var $System=
 ($System ? $System : {});
var $Control=
 ($Control ? $Control : {});
$Language.$UHC.$JS.$HTML5=
 ($Language.$UHC.$JS.$HTML5 ? $Language.$UHC.$JS.$HTML5 : {});
$Language.$UHC.$JS=
 ($Language.$UHC.$JS ? $Language.$UHC.$JS : {});
$Graphics.$UI.$WXCore=
 ($Graphics.$UI.$WXCore ? $Graphics.$UI.$WXCore : {});
$Graphics.$UI=
 ($Graphics.$UI ? $Graphics.$UI : {});
var $Data=
 ($Data ? $Data : {});
$Graphics.$UI.$WX=
 ($Graphics.$UI.$WX ? $Graphics.$UI.$WX : {});
var $UHC=
 ($UHC ? $UHC : {});
$UHC.$Generics=
 ($UHC.$Generics ? $UHC.$Generics : {});
$System.$IO=
 ($System.$IO ? $System.$IO : {});
var $Control=
 ($Control ? $Control : {});
var $LightOO=
 ($LightOO ? $LightOO : {});
$Control.$Monad=
 ($Control.$Monad ? $Control.$Monad : {});
$Language.$UHC.$JS.$HTML5.$Types=
 ($Language.$UHC.$JS.$HTML5.$Types ? $Language.$UHC.$JS.$HTML5.$Types : {});
$Language.$UHC.$JS.$Prelude=
 ($Language.$UHC.$JS.$Prelude ? $Language.$UHC.$JS.$Prelude : {});
$Graphics.$UI.$WXCore.$TimerClass=
 ($Graphics.$UI.$WXCore.$TimerClass ? $Graphics.$UI.$WXCore.$TimerClass : {});
$Graphics.$UI.$WXCore=
 ($Graphics.$UI.$WXCore ? $Graphics.$UI.$WXCore : {});
$Language.$UHC.$JS.$JSRef=
 ($Language.$UHC.$JS.$JSRef ? $Language.$UHC.$JS.$JSRef : {});
$Data.$Typeable=
 ($Data.$Typeable ? $Data.$Typeable : {});
$Graphics.$UI.$WXCore.$Types=
 ($Graphics.$UI.$WXCore.$Types ? $Graphics.$UI.$WXCore.$Types : {});
$Graphics.$UI.$WXCore.$EvtHandler=
 ($Graphics.$UI.$WXCore.$EvtHandler ? $Graphics.$UI.$WXCore.$EvtHandler : {});
$Graphics.$UI.$WXCore.$GraphicsObject=
 ($Graphics.$UI.$WXCore.$GraphicsObject ? $Graphics.$UI.$WXCore.$GraphicsObject : {});
$Graphics.$UI.$WX.$Attributes=
 ($Graphics.$UI.$WX.$Attributes ? $Graphics.$UI.$WX.$Attributes : {});
var $Asteroids=
 ($Asteroids ? $Asteroids : {});
$Language.$UHC.$JS.$HTML5.$HTMLElement=
 ($Language.$UHC.$JS.$HTML5.$HTMLElement ? $Language.$UHC.$JS.$HTML5.$HTMLElement : {});
$Graphics.$UI.$WXCore.$WindowClass=
 ($Graphics.$UI.$WXCore.$WindowClass ? $Graphics.$UI.$WXCore.$WindowClass : {});
$Language.$UHC.$JS.$HTML5.$CSSStyleDeclaration=
 ($Language.$UHC.$JS.$HTML5.$CSSStyleDeclaration ? $Language.$UHC.$JS.$HTML5.$CSSStyleDeclaration : {});
$Data.$Dynamic=
 ($Data.$Dynamic ? $Data.$Dynamic : {});
$UHC.$STRef=
 ($UHC.$STRef ? $UHC.$STRef : {});
$Graphics.$UI.$WXCore.$WebWindow=
 ($Graphics.$UI.$WXCore.$WebWindow ? $Graphics.$UI.$WXCore.$WebWindow : {});
$UHC.$Generics.$Tuple=
 ($UHC.$Generics.$Tuple ? $UHC.$Generics.$Tuple : {});
$Graphics.$UI.$WXCore.$GraphicsContext=
 ($Graphics.$UI.$WXCore.$GraphicsContext ? $Graphics.$UI.$WXCore.$GraphicsContext : {});
$Language.$UHC.$JS.$Primitives=
 ($Language.$UHC.$JS.$Primitives ? $Language.$UHC.$JS.$Primitives : {});
$Graphics.$UI.$WXCore.$GraphicsContextClass=
 ($Graphics.$UI.$WXCore.$GraphicsContextClass ? $Graphics.$UI.$WXCore.$GraphicsContextClass : {});
$Language.$UHC.$JS.$HTML5.$HTMLDocument=
 ($Language.$UHC.$JS.$HTML5.$HTMLDocument ? $Language.$UHC.$JS.$HTML5.$HTMLDocument : {});
$Language.$UHC.$JS.$HTML5.$HTMLCanvasElement=
 ($Language.$UHC.$JS.$HTML5.$HTMLCanvasElement ? $Language.$UHC.$JS.$HTML5.$HTMLCanvasElement : {});
$Graphics.$UI.$WX.$Events=
 ($Graphics.$UI.$WX.$Events ? $Graphics.$UI.$WX.$Events : {});
$UHC.$ST=
 ($UHC.$ST ? $UHC.$ST : {});
$Graphics.$UI.$WXCore.$EventClass=
 ($Graphics.$UI.$WXCore.$EventClass ? $Graphics.$UI.$WXCore.$EventClass : {});
$Graphics.$UI.$WXCore.$Event=
 ($Graphics.$UI.$WXCore.$Event ? $Graphics.$UI.$WXCore.$Event : {});
$Graphics.$UI.$WX.$Types=
 ($Graphics.$UI.$WX.$Types ? $Graphics.$UI.$WX.$Types : {});
$Data.$IORef=
 ($Data.$IORef ? $Data.$IORef : {});
$Language.$UHC.$JS.$HTML5.$CanvasRenderingContext2D=
 ($Language.$UHC.$JS.$HTML5.$CanvasRenderingContext2D ? $Language.$UHC.$JS.$HTML5.$CanvasRenderingContext2D : {});
$Graphics.$UI.$WXCore.$KeyEvent=
 ($Graphics.$UI.$WXCore.$KeyEvent ? $Graphics.$UI.$WXCore.$KeyEvent : {});
$UHC.$OldIO=
 ($UHC.$OldIO ? $UHC.$OldIO : {});
$System.$IO.$Unsafe=
 ($System.$IO.$Unsafe ? $System.$IO.$Unsafe : {});
$Graphics.$UI.$WX.$Classes=
 ($Graphics.$UI.$WX.$Classes ? $Graphics.$UI.$WX.$Classes : {});
$Graphics.$UI.$WX.$Media=
 ($Graphics.$UI.$WX.$Media ? $Graphics.$UI.$WX.$Media : {});
$Language.$UHC.$JS.$HTML5.$Node=
 ($Language.$UHC.$JS.$HTML5.$Node ? $Language.$UHC.$JS.$HTML5.$Node : {});
$System.$IO.$Fix=
 ($System.$IO.$Fix ? $System.$IO.$Fix : {});
$Graphics.$UI.$WXCore.$Timer=
 ($Graphics.$UI.$WXCore.$Timer ? $Graphics.$UI.$WXCore.$Timer : {});
$Graphics.$UI.$WX.$Draw=
 ($Graphics.$UI.$WX.$Draw ? $Graphics.$UI.$WX.$Draw : {});
$Graphics.$UI.$WX.$Window=
 ($Graphics.$UI.$WX.$Window ? $Graphics.$UI.$WX.$Window : {});
$Graphics.$UI.$WXCore.$EvtHandlerClass=
 ($Graphics.$UI.$WXCore.$EvtHandlerClass ? $Graphics.$UI.$WXCore.$EvtHandlerClass : {});
$Data.$Maybe=
 ($Data.$Maybe ? $Data.$Maybe : {});
$Control.$Monad=
 ($Control.$Monad ? $Control.$Monad : {});
$UHC.$Base=
 ($UHC.$Base ? $UHC.$Base : {});
$UHC.$IOBase=
 ($UHC.$IOBase ? $UHC.$IOBase : {});
$Language.$UHC.$JS.$HTML5.$HTMLImageElement=
 ($Language.$UHC.$JS.$HTML5.$HTMLImageElement ? $Language.$UHC.$JS.$HTML5.$HTMLImageElement : {});
$LightOO.$Core=
 ($LightOO.$Core ? $LightOO.$Core : {});
$Graphics.$UI.$WXCore.$PaintEventClass=
 ($Graphics.$UI.$WXCore.$PaintEventClass ? $Graphics.$UI.$WXCore.$PaintEventClass : {});
$Data.$Char=
 ($Data.$Char ? $Data.$Char : {});
$Language.$UHC.$JS.$HTML5.$Window=
 ($Language.$UHC.$JS.$HTML5.$Window ? $Language.$UHC.$JS.$HTML5.$Window : {});
$Graphics.$UI.$WX.$Timer=
 ($Graphics.$UI.$WX.$Timer ? $Graphics.$UI.$WX.$Timer : {});
$Graphics.$UI.$WXCore.$KeyEventClass=
 ($Graphics.$UI.$WXCore.$KeyEventClass ? $Graphics.$UI.$WXCore.$KeyEventClass : {});
$Language.$UHC.$JS.$Marshal=
 ($Language.$UHC.$JS.$Marshal ? $Language.$UHC.$JS.$Marshal : {});
$Graphics.$UI.$WXCore.$GraphicsBitmapClass=
 ($Graphics.$UI.$WXCore.$GraphicsBitmapClass ? $Graphics.$UI.$WXCore.$GraphicsBitmapClass : {});
$Graphics.$UI.$WXCore.$Events=
 ($Graphics.$UI.$WXCore.$Events ? $Graphics.$UI.$WXCore.$Events : {});
$Graphics.$UI.$WXCore.$GraphicsObjectClass=
 ($Graphics.$UI.$WXCore.$GraphicsObjectClass ? $Graphics.$UI.$WXCore.$GraphicsObjectClass : {});
$Graphics.$UI.$WXCore.$PaintEvent=
 ($Graphics.$UI.$WXCore.$PaintEvent ? $Graphics.$UI.$WXCore.$PaintEvent : {});
$Graphics.$UI.$WXCore.$GraphicsBitmap=
 ($Graphics.$UI.$WXCore.$GraphicsBitmap ? $Graphics.$UI.$WXCore.$GraphicsBitmap : {});
$Control.$Monad.$Fix=
 ($Control.$Monad.$Fix ? $Control.$Monad.$Fix : {});
$Graphics.$UI.$WX=
 ($Graphics.$UI.$WX ? $Graphics.$UI.$WX : {});
$UHC.$Run=
 ($UHC.$Run ? $UHC.$Run : {});
var $LightOO=
 ($LightOO ? $LightOO : {});
$UHC.$MutVar=
 ($UHC.$MutVar ? $UHC.$MutVar : {});
$UHC.$Run.$ehcRunMain=
 new _F_(function($m)
         {return $m;});
$Graphics.$UI.$WX.$Window.$_24okUNQ144=
 new _F_(function($props,$_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$_24x]);
          var $__4=
           new _A_($Graphics.$UI.$WX.$Attributes.$set,[$_24x,$props]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$__199__1114NEW456=
 new _F_(function($ident)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Types.$wxID__ANY,[$UHC.$Base.$Num__DCT74__101__0]);
          var $__3=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,$ident,$__]);
          var $__4=
           _e_($__3);
          var $__swJSW0__0;
          switch($__4._tag_)
           {case 0:
             var $__5=
              new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$ident]);
             $__swJSW0__0=
              $__5;
             break;
            case 1:
             $__swJSW0__0=
              $Graphics.$UI.$WXCore.$Types.$idCreate;
             break;}
          return $__swJSW0__0;});
$Language.$UHC.$JS.$HTML5.$Types.$__htmlElementRef=
 new _A_(new _F_(function()
                 {return HTMLElement;}),[]);
$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__DCT99__0__0DFLLanguage_2eUHC_2eJS_2ePrelude_2egetObjectRef=
 new _F_(function($__)
         {return $Language.$UHC.$JS.$HTML5.$Types.$__htmlElementRef;});
$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__NEW187UNQ395EVLDCT99__0__0RDC=
 new _F_(function($GetObjectRef__)
         {var $GetObjectRef__2=
           _e_(new _A_($Language.$UHC.$JS.$Prelude.$GetObjectRef__CLS93__0__0,[$GetObjectRef__]));
          var $__4=
           {_tag_:0,_1:$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__DCT99__0__0DFLLanguage_2eUHC_2eJS_2ePrelude_2egetObjectRef};
          return $__4;});
$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__NEW185UNQ394DCT99__0__0RDC=
 new _F_(function($GetObjectRef__)
         {var $GetObjectRef__2=
           new _A_($Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__NEW187UNQ395EVLDCT99__0__0RDC,[$GetObjectRef__]);
          return $GetObjectRef__2;});
$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__UNQ394DCT99__0__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__NEW185UNQ394DCT99__0__0RDC,[$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__UNQ394DCT99__0__0RDC]);}),[]);
$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__DCT99__0__0=
 new _A_(new _F_(function()
                 {return $Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__UNQ394DCT99__0__0RDC;}),[]);
$Language.$UHC.$JS.$HTML5.$Node.$appendChild=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__);
          var $__5=
           _e_($__2);
          var $__6=
           _e_($__4.appendChild($__5));
          return [$__3,$__6];});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ296=
 new _F_(function($htmlDiv,$_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$htmlDiv]);
          var $__4=
           new _A_($LightOO.$Core.$_23,[$_24x,$Language.$UHC.$JS.$HTML5.$Node.$appendChild]);
          var $__5=
           new _A_($UHC.$Base.$_24,[$__4,$htmlDiv]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__5,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ287=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($Language.$UHC.$JS.$Prelude.$cast,[$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__DCT99__0__0,$_24x2]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToString,["not a div element"]);
          var $__5=
           new _A_($UHC.$Base.$error,[$__4]);
          var $htmlDiv=
           new _A_($UHC.$Base.$maybe,[$__5,$UHC.$Base.$id,$__]);
          var $__7=
           new _A_($UHC.$Base.$packedStringToString,["body"]);
          var $__8=
           new _A_($Language.$UHC.$JS.$Prelude.$getAttr,[$__7,$_24x]);
          var $__9=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ296,[$htmlDiv]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__8,$__9]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ281=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["div"]);
          var $__3=
           new _A_($Language.$UHC.$JS.$Marshal.$str,[$__]);
          var $__4=
           new _A_($Language.$UHC.$JS.$HTML5.$HTMLDocument.$createElement,[$_24x,$__3]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ287,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__5]);});
$Graphics.$UI.$WXCore.$WebWindow.$initializeUNQ275=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Language.$UHC.$JS.$HTML5.$HTMLDocument.$document,$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ281]);}),[]);
$Graphics.$UI.$WXCore.$KeyEventClass.$Widen__DCT93__2__0DFLLightOO_2eCore_2ewiden=
 new _F_(function($__,$o)
         {return new _A_($LightOO.$Core.$genericWiden,[$__,$o,$Graphics.$UI.$WXCore.$EventClass.$get__Event__Tail,$Graphics.$UI.$WXCore.$EventClass.$set__Event__Tail]);});
$Graphics.$UI.$WXCore.$KeyEventClass.$Widen__NEW147UNQ246EVLDCT93__2__0RDC=
 new _F_(function($__,$Widen__)
         {var $Widen__3=
           _e_(new _A_($LightOO.$Core.$Widen__CLS69__6__0,[$Widen__]));
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$KeyEventClass.$Widen__DCT93__2__0DFLLightOO_2eCore_2ewiden,[$__]);
          var $__6=
           {_tag_:0,_1:$__5};
          return $__6;});
$Graphics.$UI.$WXCore.$KeyEventClass.$Widen__NEW144UNQ243DCT93__2__0RDC=
 new _F_(function($__,$Widen__)
         {var $Widen__3=
           new _A_($Graphics.$UI.$WXCore.$KeyEventClass.$Widen__NEW147UNQ246EVLDCT93__2__0RDC,[$__,$Widen__]);
          return $Widen__3;});
$Graphics.$UI.$WXCore.$KeyEventClass.$__95__702__2__0UNQ244=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$KeyEventClass.$Typeable__DCT93__9__0,[$Data.$Typeable.$Typeable__DCT320__26__0]);}),[]);
$Graphics.$UI.$WXCore.$KeyEventClass.$Widen__UNQ243DCT93__2__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$KeyEventClass.$Widen__NEW144UNQ243DCT93__2__0RDC,[$Graphics.$UI.$WXCore.$KeyEventClass.$__95__702__2__0UNQ244,$Graphics.$UI.$WXCore.$KeyEventClass.$Widen__UNQ243DCT93__2__0RDC]);}),[]);
$Graphics.$UI.$WXCore.$KeyEventClass.$Widen__DCT93__2__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$KeyEventClass.$Widen__UNQ243DCT93__2__0RDC;}),[]);
$Language.$UHC.$JS.$JSRef.$newReadOnlyJSRef=
 new _F_(function($r)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["read only ref"]);
          var $__3=
           new _A_($UHC.$Base.$error,[$__]);
          var $__4=
           new _A_($Language.$UHC.$JS.$JSRef.$newJSRef,[$r,$__3]);
          return new _A_($UHC.$Base.$_24,[$UHC.$Base.$unsafeCoerce,$__4]);});
$Language.$UHC.$JS.$HTML5.$CSSStyleDeclaration.$setProperty=
 new _F_(function($__,$__2,$__3,$__4)
         {var $__5=
           _e_($__);
          var $__6=
           _e_($__2);
          var $__7=
           _e_($__3);
          var $__8=
           _e_($__5.setProperty($__6,$__7));
          var $__9=
           _e_([]);
          return [$__4,$__9];});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ351=
 new _F_(function($p,$v,$_24x)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["px"]);
          var $__5=
           new _A_($UHC.$Base.$show,[$UHC.$Base.$Show__DCT74__128__0,$v]);
          var $v_27=
           new _A_($UHC.$Base.$_2b_2b,[$__5,$__]);
          var $__7=
           new _A_($Language.$UHC.$JS.$Marshal.$str,[$v_27]);
          var $__8=
           new _A_($Language.$UHC.$JS.$Marshal.$str,[$p]);
          return new _A_($Language.$UHC.$JS.$HTML5.$CSSStyleDeclaration.$setProperty,[$_24x,$__8,$__7]);});
$Graphics.$UI.$WXCore.$WebWindow.$settUNQ305=
 new _F_(function($p,$cssRef,$v)
         {var $__=
           new _A_($Language.$UHC.$JS.$JSRef.$readJSRef,[$cssRef]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ351,[$p,$v]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__5]);});
$Language.$UHC.$JS.$Primitives.$__primIsNull=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primIsNull($__2);});
$Language.$UHC.$JS.$Prelude.$unwrapFunc1=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__3());
          return [$__2,$__4];});
$Language.$UHC.$JS.$HTML5.$HTMLElement.$_24okUNQ12=
 new _F_(function($_24x)
         {var $__=
           new _A_($Language.$UHC.$JS.$Primitives.$__primIsNull,[$_24x]);
          var $__3=
           _e_($__);
          var $__swJSW3__0;
          switch($__3._tag_)
           {case 0:
             var $__4=
              new _A_($UHC.$Base.$unsafeCoerce,[$_24x]);
             var $__5=
              new _A_($Language.$UHC.$JS.$Prelude.$unwrapFunc1,[$__4]);
             $__swJSW3__0=
              $__5;
             break;
            case 1:
             var $__6=
              new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
             var $__7=
              new _A_($UHC.$Base.$const,[$__6]);
             var $__8=
              new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
             var $__9=
              new _A_($UHC.$Base.$_24,[$__8,$__7]);
             $__swJSW3__0=
              $__9;
             break;}
          return $__swJSW3__0;});
$Language.$UHC.$JS.$HTML5.$HTMLElement.$gNEW5UNQ6=
 new _F_(function($e)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["onkeydown"]);
          var $__3=
           new _A_($Language.$UHC.$JS.$Prelude.$getAttr,[$__,$e]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__3,$Language.$UHC.$JS.$HTML5.$HTMLElement.$_24okUNQ12]);});
$Language.$UHC.$JS.$Prelude.$wrapFunc1=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_(function(v1)
               {var res=
                 _e_(new _A_($__3,[v1,[]]));
                _e_(res[0]);
                return _e_(res[1]);});
          return [$__2,$__4];});
$Language.$UHC.$JS.$HTML5.$HTMLElement.$__151__10__0=
 new _F_(function($e,$f)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["onkeydown"]);
          return new _A_($Language.$UHC.$JS.$Prelude.$setAttr__,[$__,$f,$e]);});
$Language.$UHC.$JS.$HTML5.$HTMLElement.$sUNQ4=
 new _F_(function($e,$f)
         {var $__=
           new _A_($Language.$UHC.$JS.$Prelude.$wrapFunc1,[$f]);
          var $__4=
           new _A_($Language.$UHC.$JS.$HTML5.$HTMLElement.$__151__10__0,[$e]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Language.$UHC.$JS.$HTML5.$HTMLElement.$onkeydown=
 new _F_(function($e)
         {var $g=
           new _A_($Language.$UHC.$JS.$HTML5.$HTMLElement.$gNEW5UNQ6,[$e]);
          var $__=
           new _A_($Language.$UHC.$JS.$HTML5.$HTMLElement.$sUNQ4,[$e]);
          return new _A_($Language.$UHC.$JS.$JSRef.$newJSRef,[$g,$__]);});
$Language.$UHC.$JS.$HTML5.$CSSStyleDeclaration.$getPropertyValue=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__);
          var $__5=
           _e_($__2);
          var $__6=
           _e_($__4.getPropertyValue($__5));
          return [$__3,$__6];});
$Language.$UHC.$JS.$Marshal.$jsStringToString=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$UHC.$Base.$packedStringToString,$UHC.$Base.$unsafeCoerce]);}),[]);
$Language.$UHC.$JS.$Primitives.$__primIsString=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primIsString($__2);});
$Language.$UHC.$JS.$Marshal.$FromJS__DCT81__15__0DFLLanguage_2eUHC_2eJS_2eMarshal_2efromJS=
 new _F_(function($v)
         {var $__=
           new _A_($Language.$UHC.$JS.$Primitives.$__primIsString,[$v]);
          var $__3=
           _e_($__);
          var $__swJSW4__0;
          switch($__3._tag_)
           {case 0:
             $__swJSW4__0=
              $UHC.$Base.$Nothing__;
             break;
            case 1:
             var $__4=
              new _A_($Language.$UHC.$JS.$Marshal.$jsStringToString,[$v]);
             var $__5=
              new _A_($UHC.$Base.$Just__,[$__4]);
             $__swJSW4__0=
              $__5;
             break;}
          return $__swJSW4__0;});
$Language.$UHC.$JS.$Marshal.$FromJS__CLS81__1__0=
 new _F_(function($FromJS__)
         {var $FromJS__2=
           {_tag_:0,_1:$UHC.$Base.$undefined};
          return $FromJS__2;});
$Language.$UHC.$JS.$Marshal.$FromJS__NEW53UNQ174EVLDCT81__15__0RDC=
 new _F_(function($FromJS__)
         {var $FromJS__2=
           _e_(new _A_($Language.$UHC.$JS.$Marshal.$FromJS__CLS81__1__0,[$FromJS__]));
          var $__4=
           {_tag_:0,_1:$Language.$UHC.$JS.$Marshal.$FromJS__DCT81__15__0DFLLanguage_2eUHC_2eJS_2eMarshal_2efromJS};
          return $__4;});
$Language.$UHC.$JS.$Marshal.$FromJS__NEW51UNQ173DCT81__15__0RDC=
 new _F_(function($FromJS__)
         {var $FromJS__2=
           new _A_($Language.$UHC.$JS.$Marshal.$FromJS__NEW53UNQ174EVLDCT81__15__0RDC,[$FromJS__]);
          return $FromJS__2;});
$Language.$UHC.$JS.$Marshal.$FromJS__UNQ173DCT81__15__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Language.$UHC.$JS.$Marshal.$FromJS__NEW51UNQ173DCT81__15__0RDC,[$Language.$UHC.$JS.$Marshal.$FromJS__UNQ173DCT81__15__0RDC]);}),[]);
$Language.$UHC.$JS.$Marshal.$FromJS__DCT81__15__0=
 new _A_(new _F_(function()
                 {return $Language.$UHC.$JS.$Marshal.$FromJS__UNQ173DCT81__15__0RDC;}),[]);
$Language.$UHC.$JS.$Marshal.$fromJS=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$Graphics.$UI.$WXCore.$WebWindow.$__199__74=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$takeWhile,[$UHC.$Base.$isDigit]);}),[]);
$UHC.$Base.$_24okUNQ8788=
 new _F_(function($x,$_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           _e_($__[0]);
          var $__swJSW8__0;
          switch($__6._tag_)
           {case 0:
             $__swJSW8__0=
              $UHC.$Base.$_5b_5d;
             break;
            case 1:
             var $__9=
              _e_($__[1]);
             var $__swJSW9__0;
             switch($__9._tag_)
              {case 0:
                $__swJSW9__0=
                 $UHC.$Base.$_5b_5d;
                break;
               case 1:
                var $__12=
                 new _A_($UHC.$Base.$_3a,[$x,$UHC.$Base.$_5b_5d]);
                $__swJSW9__0=
                 $__12;
                break;}
             $__swJSW8__0=
              $__swJSW9__0;
             break;}
          return $__swJSW8__0;});
$UHC.$Base.$_24okUNQ8775=
 new _F_(function($_24x)
         {var $__=
           _e_($_24x);
          var $__5=
           new _A_($UHC.$Base.$lex,[$__[1]]);
          var $__6=
           new _A_($UHC.$Base.$_24okUNQ8788,[$__[0]]);
          return new _A_($UHC.$Base.$concatMap,[$__6,$__5]);});
$UHC.$Base.$__76__40235__0NEW5286UNQ8774=
 new _F_(function($__,$s)
         {var $__3=
           new _A_($UHC.$Base.$reads,[$__,$s]);
          return new _A_($UHC.$Base.$concatMap,[$UHC.$Base.$_24okUNQ8775,$__3]);});
$UHC.$Base.$read=
 new _F_(function($__,$s)
         {var $__3=
           new _A_($UHC.$Base.$__76__40235__0NEW5286UNQ8774,[$__,$s]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToString,["Prelude.read: ambiguous parse"]);
          var $__5=
           new _A_($UHC.$Base.$error,[$__4]);
          var $__6=
           _e_($__3);
          var $__swJSW11__0;
          switch($__6._tag_)
           {case 0:
             var $__9=
              _e_($__6._2);
             var $__swJSW12__0;
             switch($__9._tag_)
              {case 0:
                $__swJSW12__0=
                 $__5;
                break;
               case 1:
                $__swJSW12__0=
                 $__6._1;
                break;}
             $__swJSW11__0=
              $__swJSW12__0;
             break;
            case 1:
             var $__12=
              new _A_($UHC.$Base.$packedStringToString,["Prelude.read: no parse"]);
             var $__13=
              new _A_($UHC.$Base.$error,[$__12]);
             $__swJSW11__0=
              $__13;
             break;}
          return $__swJSW11__0;});
$UHC.$Base.$__76__30251__2__0NEW6074UNQ7406=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Base.$__76__30220__2__3NEW6071UNQ7405=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Base.$fromEnum__0=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$fromEnum,[$UHC.$Base.$Enum__DCT74__60__0,48]);}),[]);
$UHC.$Base.$__78__12185__0=
 new _F_(function($d)
         {var $__=
           new _A_($UHC.$Base.$fromEnum,[$UHC.$Base.$Enum__DCT74__60__0,$d]);
          return new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$__,$UHC.$Base.$fromEnum__0]);});
$UHC.$Base.$__76__28332__4__0NEW6044UNQ7112=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Base.$__76__28081__2__1NEW6041UNQ7111=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Base.$foldl1=
 new _F_(function($f,$__)
         {var $__3=
           _e_($__);
          var $__swJSW17__0;
          switch($__3._tag_)
           {case 0:
             var $__6=
              new _A_($UHC.$Base.$foldl,[$f,$__3._1,$__3._2]);
             $__swJSW17__0=
              $__6;
             break;
            case 1:
             $__swJSW17__0=
              $UHC.$Base.$undefined;
             break;}
          return $__swJSW17__0;});
$UHC.$Base.$__78__12129__0=
 new _F_(function($__,$radix,$n)
         {var $__4=
           new _A_($UHC.$Base.$_2a,[$__,$n,$radix]);
          return new _A_($UHC.$Base.$_2b,[$__,$__4]);});
$UHC.$Base.$_24okUNQ7138=
 new _F_(function($__,$radix,$digToInt,$_24x)
         {var $__5=
           _e_($_24x);
          var $__8=
           new _A_($UHC.$Base.$fromIntegral,[$UHC.$Base.$Integral__DCT74__110__0,$__]);
          var $__9=
           new _A_($UHC.$Base.$_2e,[$__8,$digToInt]);
          var $__10=
           new _A_($UHC.$Base.$map,[$__9,$__5[0]]);
          var $__11=
           new _A_($UHC.$Base.$__78__12129__0,[$__,$radix]);
          var $__12=
           new _A_($UHC.$Base.$foldl1,[$__11,$__10]);
          var $__13=
           [$__12,$__5[1]];
          var $__14=
           new _A_($UHC.$Base.$_3a,[$__13,$UHC.$Base.$_5b_5d]);
          return $__14;});
$UHC.$Base.$__78__12110__0=
 new _F_(function($__,$radix,$isDig,$digToInt,$s)
         {var $__6=
           new _A_($UHC.$Base.$nonnull,[$isDig,$s]);
          var $__7=
           new _A_($UHC.$Base.$_24okUNQ7138,[$__,$radix,$digToInt]);
          return new _A_($UHC.$Base.$concatMap,[$__7,$__6]);});
$UHC.$Base.$readInt=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$__76__28081__2__1NEW6041UNQ7111,[$__]);
          var $__3=
           new _A_($UHC.$Base.$__76__28332__4__0NEW6044UNQ7112,[$__2]);
          return new _A_($UHC.$Base.$__78__12110__0,[$__3]);});
$UHC.$Base.$readDec=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$__76__30220__2__3NEW6071UNQ7405,[$__]);
          var $__3=
           new _A_($UHC.$Base.$__76__30251__2__0NEW6074UNQ7406,[$__2]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToInteger,["10"]);
          var $__5=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__4]);
          return new _A_($UHC.$Base.$readInt,[$__,$__5,$UHC.$Base.$isDigit,$UHC.$Base.$__78__12185__0]);});
$UHC.$Base.$__76__36972__2__0NEW5079UNQ8165=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Base.$_24okUNQ8230=
 new _F_(function($__,$_24x)
         {var $__3=
           _e_($_24x);
          var $__6=
           new _A_($UHC.$Base.$negate,[$__,$__3[0]]);
          var $__7=
           [$__6,$__3[1]];
          var $__8=
           new _A_($UHC.$Base.$_3a,[$__7,$UHC.$Base.$_5b_5d]);
          return $__8;});
$UHC.$Base.$_24okUNQ8211=
 new _F_(function($__,$readPos,$_24x)
         {var $__4=
           _e_($_24x);
          var $__7=
           _e_($__4[0]);
          var $__swJSW22__0;
          switch($__7._tag_)
           {case 0:
             var $__10=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,45,$__7._1]));
             var $__swJSW23__0;
             switch($__10._tag_)
              {case 0:
                $__swJSW23__0=
                 $UHC.$Base.$_5b_5d;
                break;
               case 1:
                var $__11=
                 _e_($__7._2);
                var $__swJSW24__0;
                switch($__11._tag_)
                 {case 0:
                   $__swJSW24__0=
                    $UHC.$Base.$_5b_5d;
                   break;
                  case 1:
                   var $__14=
                    new _A_($UHC.$Base.$read_27_27UNQ8179,[$readPos,$__4[1]]);
                   var $__15=
                    new _A_($UHC.$Base.$_24okUNQ8230,[$__]);
                   $__swJSW24__0=
                    new _A_($UHC.$Base.$concatMap,[$__15,$__14]);
                   break;}
                $__swJSW23__0=
                 $__swJSW24__0;
                break;}
             $__swJSW22__0=
              $__swJSW23__0;
             break;
            case 1:
             $__swJSW22__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW22__0;});
$UHC.$Base.$__78__10073NEW5094=
 new _F_(function($__,$readPos,$r)
         {var $__4=
           new _A_($UHC.$Base.$lex,[$r]);
          var $__5=
           new _A_($UHC.$Base.$_24okUNQ8211,[$__,$readPos]);
          return new _A_($UHC.$Base.$concatMap,[$__5,$__4]);});
$UHC.$Base.$_24okUNQ8195=
 new _F_(function($s,$_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           _e_($__[1]);
          var $__swJSW26__0;
          switch($__6._tag_)
           {case 0:
             $__swJSW26__0=
              $UHC.$Base.$_5b_5d;
             break;
            case 1:
             var $__9=
              [$__[0],$s];
             var $__10=
              new _A_($UHC.$Base.$_3a,[$__9,$UHC.$Base.$_5b_5d]);
             $__swJSW26__0=
              $__10;
             break;}
          return $__swJSW26__0;});
$UHC.$Base.$_24okUNQ8182=
 new _F_(function($readPos,$_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           new _A_($readPos,[$__[0]]);
          var $__7=
           new _A_($UHC.$Base.$_24okUNQ8195,[$__[1]]);
          return new _A_($UHC.$Base.$concatMap,[$__7,$__6]);});
$UHC.$Base.$read_27_27UNQ8179=
 new _F_(function($readPos,$r)
         {var $__=
           new _A_($UHC.$Base.$lex,[$r]);
          var $__4=
           new _A_($UHC.$Base.$_24okUNQ8182,[$readPos]);
          return new _A_($UHC.$Base.$concatMap,[$__4,$__]);});
$UHC.$Base.$read_27UNQ8180=
 new _F_(function($__,$readPos,$r)
         {var $__4=
           new _A_($UHC.$Base.$__78__10073NEW5094,[$__,$readPos,$r]);
          var $__5=
           new _A_($UHC.$Base.$read_27_27UNQ8179,[$readPos,$r]);
          return new _A_($UHC.$Base.$_2b_2b,[$__5,$__4]);});
$UHC.$Base.$__78__10040__0=
 new _F_(function($__,$readPos)
         {var $__3=
           new _A_($UHC.$Base.$read_27UNQ8180,[$__,$readPos]);
          return new _A_($UHC.$Base.$readParen,[$UHC.$Base.$False__,$__3]);});
$UHC.$Base.$readSigned=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$__76__36972__2__0NEW5079UNQ8165,[$__]);
          return new _A_($UHC.$Base.$__78__10040__0,[$__2]);});
$UHC.$Base.$Read__DCT74__127__0DFLUHC_2eBase_2ereadsPrec=
 new _F_(function($p)
         {var $__=
           new _A_($UHC.$Base.$readDec,[$UHC.$Base.$Integral__DCT74__110__0]);
          return new _A_($UHC.$Base.$readSigned,[$UHC.$Base.$Real__DCT74__100__0,$__]);});
$UHC.$Base.$_24okUNQ8497=
 new _F_(function($_24x)
         {return new _A_($UHC.$Base.$_3a,[$_24x,$UHC.$Base.$_5b_5d]);});
$UHC.$Base.$_24okUNQ8433=
 new _F_(function($_24x)
         {var $__=
           _e_($_24x);
          var $__5=
           _e_($__[0]);
          var $__swJSW29__0;
          switch($__5._tag_)
           {case 0:
             var $__8=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,93,$__5._1]));
             var $__swJSW30__0;
             switch($__8._tag_)
              {case 0:
                $__swJSW30__0=
                 $UHC.$Base.$_5b_5d;
                break;
               case 1:
                var $__9=
                 _e_($__5._2);
                var $__swJSW31__0;
                switch($__9._tag_)
                 {case 0:
                   $__swJSW31__0=
                    $UHC.$Base.$_5b_5d;
                   break;
                  case 1:
                   var $__12=
                    [$UHC.$Base.$_5b_5d,$__[1]];
                   var $__13=
                    new _A_($UHC.$Base.$_3a,[$__12,$UHC.$Base.$_5b_5d]);
                   $__swJSW31__0=
                    $__13;
                   break;}
                $__swJSW30__0=
                 $__swJSW31__0;
                break;}
             $__swJSW29__0=
              $__swJSW30__0;
             break;
            case 1:
             $__swJSW29__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW29__0;});
$UHC.$Base.$__78__10181NEW5156=
 new _F_(function($s)
         {var $__=
           new _A_($UHC.$Base.$lex,[$s]);
          return new _A_($UHC.$Base.$concatMap,[$UHC.$Base.$_24okUNQ8433,$__]);});
$UHC.$Base.$_24okUNQ8464=
 new _F_(function($x,$_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           new _A_($UHC.$Base.$_3a,[$x,$__[0]]);
          var $__7=
           [$__6,$__[1]];
          var $__8=
           new _A_($UHC.$Base.$_3a,[$__7,$UHC.$Base.$_5b_5d]);
          return $__8;});
$UHC.$Base.$_24okUNQ8368=
 new _F_(function($_24x)
         {var $__=
           _e_($_24x);
          var $__5=
           _e_($__[0]);
          var $__swJSW34__0;
          switch($__5._tag_)
           {case 0:
             var $__8=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,93,$__5._1]));
             var $__swJSW35__0;
             switch($__8._tag_)
              {case 0:
                $__swJSW35__0=
                 $UHC.$Base.$_5b_5d;
                break;
               case 1:
                var $__9=
                 _e_($__5._2);
                var $__swJSW36__0;
                switch($__9._tag_)
                 {case 0:
                   $__swJSW36__0=
                    $UHC.$Base.$_5b_5d;
                   break;
                  case 1:
                   var $__12=
                    [$UHC.$Base.$_5b_5d,$__[1]];
                   var $__13=
                    new _A_($UHC.$Base.$_3a,[$__12,$UHC.$Base.$_5b_5d]);
                   $__swJSW36__0=
                    $__13;
                   break;}
                $__swJSW35__0=
                 $__swJSW36__0;
                break;}
             $__swJSW34__0=
              $__swJSW35__0;
             break;
            case 1:
             $__swJSW34__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW34__0;});
$UHC.$Base.$__78__10115NEW5133=
 new _F_(function($s)
         {var $__=
           new _A_($UHC.$Base.$lex,[$s]);
          return new _A_($UHC.$Base.$concatMap,[$UHC.$Base.$_24okUNQ8368,$__]);});
$UHC.$Base.$_24okUNQ8419=
 new _F_(function($x,$_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           new _A_($UHC.$Base.$_3a,[$x,$__[0]]);
          var $__7=
           [$__6,$__[1]];
          var $__8=
           new _A_($UHC.$Base.$_3a,[$__7,$UHC.$Base.$_5b_5d]);
          return $__8;});
$UHC.$Base.$_24okUNQ8408=
 new _F_(function($Read__,$_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           new _A_($UHC.$Base.$readl_27UNQ8364,[$Read__,$__[1]]);
          var $__7=
           new _A_($UHC.$Base.$_24okUNQ8419,[$__[0]]);
          return new _A_($UHC.$Base.$concatMap,[$__7,$__6]);});
$UHC.$Base.$_24okUNQ8389=
 new _F_(function($Read__,$_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           _e_($__[0]);
          var $__swJSW40__0;
          switch($__6._tag_)
           {case 0:
             var $__9=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,44,$__6._1]));
             var $__swJSW41__0;
             switch($__9._tag_)
              {case 0:
                $__swJSW41__0=
                 $UHC.$Base.$_5b_5d;
                break;
               case 1:
                var $__10=
                 _e_($__6._2);
                var $__swJSW42__0;
                switch($__10._tag_)
                 {case 0:
                   $__swJSW42__0=
                    $UHC.$Base.$_5b_5d;
                   break;
                  case 1:
                   var $__13=
                    new _A_($UHC.$Base.$reads,[$Read__,$__[1]]);
                   var $__14=
                    new _A_($UHC.$Base.$_24okUNQ8408,[$Read__]);
                   $__swJSW42__0=
                    new _A_($UHC.$Base.$concatMap,[$__14,$__13]);
                   break;}
                $__swJSW41__0=
                 $__swJSW42__0;
                break;}
             $__swJSW40__0=
              $__swJSW41__0;
             break;
            case 1:
             $__swJSW40__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW40__0;});
$UHC.$Base.$__78__10136NEW5115=
 new _F_(function($Read__,$s)
         {var $__=
           new _A_($UHC.$Base.$lex,[$s]);
          var $__4=
           new _A_($UHC.$Base.$_24okUNQ8389,[$Read__]);
          return new _A_($UHC.$Base.$concatMap,[$__4,$__]);});
$UHC.$Base.$readl_27UNQ8364=
 new _F_(function($Read__,$s)
         {var $__=
           new _A_($UHC.$Base.$__78__10136NEW5115,[$Read__,$s]);
          var $__4=
           new _A_($UHC.$Base.$__78__10115NEW5133,[$s]);
          return new _A_($UHC.$Base.$_2b_2b,[$__4,$__]);});
$UHC.$Base.$_24okUNQ8453=
 new _F_(function($Read__,$_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           new _A_($UHC.$Base.$readl_27UNQ8364,[$Read__,$__[1]]);
          var $__7=
           new _A_($UHC.$Base.$_24okUNQ8464,[$__[0]]);
          return new _A_($UHC.$Base.$concatMap,[$__7,$__6]);});
$UHC.$Base.$readsPrec=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._2;});
$UHC.$Base.$reads=
 new _F_(function($__)
         {return new _A_($UHC.$Base.$readsPrec,[$__,0]);});
$UHC.$Base.$__78__10202NEW5144=
 new _F_(function($Read__,$s)
         {var $__=
           new _A_($UHC.$Base.$reads,[$Read__,$s]);
          var $__4=
           new _A_($UHC.$Base.$_24okUNQ8453,[$Read__]);
          return new _A_($UHC.$Base.$concatMap,[$__4,$__]);});
$UHC.$Base.$readlUNQ8365=
 new _F_(function($Read__,$s)
         {var $__=
           new _A_($UHC.$Base.$__78__10202NEW5144,[$Read__,$s]);
          var $__4=
           new _A_($UHC.$Base.$__78__10181NEW5156,[$s]);
          return new _A_($UHC.$Base.$_2b_2b,[$__4,$__]);});
$UHC.$Base.$_24okUNQ8478=
 new _F_(function($Read__,$_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           _e_($__[0]);
          var $__swJSW46__0;
          switch($__6._tag_)
           {case 0:
             var $__9=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,91,$__6._1]));
             var $__swJSW47__0;
             switch($__9._tag_)
              {case 0:
                $__swJSW47__0=
                 $UHC.$Base.$_5b_5d;
                break;
               case 1:
                var $__10=
                 _e_($__6._2);
                var $__swJSW48__0;
                switch($__10._tag_)
                 {case 0:
                   $__swJSW48__0=
                    $UHC.$Base.$_5b_5d;
                   break;
                  case 1:
                   var $__13=
                    new _A_($UHC.$Base.$readlUNQ8365,[$Read__,$__[1]]);
                   $__swJSW48__0=
                    new _A_($UHC.$Base.$concatMap,[$UHC.$Base.$_24okUNQ8497,$__13]);
                   break;}
                $__swJSW47__0=
                 $__swJSW48__0;
                break;}
             $__swJSW46__0=
              $__swJSW47__0;
             break;
            case 1:
             $__swJSW46__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW46__0;});
$UHC.$Base.$__78__10227__0=
 new _F_(function($Read__,$r)
         {var $__=
           new _A_($UHC.$Base.$lex,[$r]);
          var $__4=
           new _A_($UHC.$Base.$_24okUNQ8478,[$Read__]);
          return new _A_($UHC.$Base.$concatMap,[$__4,$__]);});
$UHC.$Base.$_24okUNQ8142=
 new _F_(function($x,$_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           _e_($__[0]);
          var $__swJSW50__0;
          switch($__6._tag_)
           {case 0:
             var $__9=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,41,$__6._1]));
             var $__swJSW51__0;
             switch($__9._tag_)
              {case 0:
                $__swJSW51__0=
                 $UHC.$Base.$_5b_5d;
                break;
               case 1:
                var $__10=
                 _e_($__6._2);
                var $__swJSW52__0;
                switch($__10._tag_)
                 {case 0:
                   $__swJSW52__0=
                    $UHC.$Base.$_5b_5d;
                   break;
                  case 1:
                   var $__13=
                    [$x,$__[1]];
                   var $__14=
                    new _A_($UHC.$Base.$_3a,[$__13,$UHC.$Base.$_5b_5d]);
                   $__swJSW52__0=
                    $__14;
                   break;}
                $__swJSW51__0=
                 $__swJSW52__0;
                break;}
             $__swJSW50__0=
              $__swJSW51__0;
             break;
            case 1:
             $__swJSW50__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW50__0;});
$UHC.$Base.$_24okUNQ8130=
 new _F_(function($_24x)
         {var $__=
           _e_($_24x);
          var $__5=
           new _A_($UHC.$Base.$lex,[$__[1]]);
          var $__6=
           new _A_($UHC.$Base.$_24okUNQ8142,[$__[0]]);
          return new _A_($UHC.$Base.$concatMap,[$__6,$__5]);});
$UHC.$Base.$_24okUNQ8111=
 new _F_(function($g,$_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           _e_($__[0]);
          var $__swJSW55__0;
          switch($__6._tag_)
           {case 0:
             var $__9=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,40,$__6._1]));
             var $__swJSW56__0;
             switch($__9._tag_)
              {case 0:
                $__swJSW56__0=
                 $UHC.$Base.$_5b_5d;
                break;
               case 1:
                var $__10=
                 _e_($__6._2);
                var $__swJSW57__0;
                switch($__10._tag_)
                 {case 0:
                   $__swJSW57__0=
                    $UHC.$Base.$_5b_5d;
                   break;
                  case 1:
                   var $__13=
                    new _A_($UHC.$Base.$optionalUNQ8106,[$g,$__[1]]);
                   $__swJSW57__0=
                    new _A_($UHC.$Base.$concatMap,[$UHC.$Base.$_24okUNQ8130,$__13]);
                   break;}
                $__swJSW56__0=
                 $__swJSW57__0;
                break;}
             $__swJSW55__0=
              $__swJSW56__0;
             break;
            case 1:
             $__swJSW55__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW55__0;});
$UHC.$Base.$isSpace=
 new _F_(function($c)
         {var $__=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,$c,160]);
          var $__3=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,$c,11]);
          var $__4=
           new _A_($UHC.$Base.$_7c_7c,[$__3,$__]);
          var $__5=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,$c,12]);
          var $__6=
           new _A_($UHC.$Base.$_7c_7c,[$__5,$__4]);
          var $__7=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,$c,13]);
          var $__8=
           new _A_($UHC.$Base.$_7c_7c,[$__7,$__6]);
          var $__9=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,$c,10]);
          var $__10=
           new _A_($UHC.$Base.$_7c_7c,[$__9,$__8]);
          var $__11=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,$c,9]);
          var $__12=
           new _A_($UHC.$Base.$_7c_7c,[$__11,$__10]);
          var $__13=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,$c,32]);
          return new _A_($UHC.$Base.$_7c_7c,[$__13,$__12]);});
$UHC.$Base.$pNEW1245UNQ3432CCN=
 new _F_(function($x1,$x2)
         {var $x23=
           _e_($x2);
          var $__swJSW58__0;
          switch($x23._tag_)
           {case 0:
             var $__=
              new _A_($x1,[$x23._1]);
             var $__7=
              _e_($__);
             var $__swJSW59__0;
             switch($__7._tag_)
              {case 0:
                var $__8=
                 _e_($UHC.$Base.$otherwise);
                var $__swJSW60__0;
                switch($__8._tag_)
                 {case 0:
                   $__swJSW60__0=
                    $UHC.$Base.$undefined;
                   break;
                  case 1:
                   $__swJSW60__0=
                    $x23;
                   break;}
                $__swJSW59__0=
                 $__swJSW60__0;
                break;
               case 1:
                var $__9=
                 new _A_($UHC.$Base.$dropWhile,[$x1,$x23._2]);
                $__swJSW59__0=
                 $__9;
                break;}
             $__swJSW58__0=
              $__swJSW59__0;
             break;
            case 1:
             $__swJSW58__0=
              $UHC.$Base.$undefined;
             break;}
          return $__swJSW58__0;});
$UHC.$Base.$dropWhile=
 new _F_(function($x1,$x2)
         {var $p=
           new _A_($UHC.$Base.$pNEW1245UNQ3432CCN,[$x1,$x2]);
          var $x24=
           _e_($x2);
          var $__swJSW61__0;
          switch($x24._tag_)
           {case 0:
             $__swJSW61__0=
              $p;
             break;
            case 1:
             $__swJSW61__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW61__0;});
$UHC.$Base.$isAlphaNum=
 new _F_(function($c)
         {var $__=
           new _A_($UHC.$Base.$isDigit,[$c]);
          var $__3=
           new _A_($UHC.$Base.$isAlpha,[$c]);
          return new _A_($UHC.$Base.$_7c_7c,[$__3,$__]);});
$UHC.$Base.$isLower=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primCharIsLower($__2);});
$UHC.$Base.$isAlpha=
 new _F_(function($c)
         {var $__=
           new _A_($UHC.$Base.$isLower,[$c]);
          var $__3=
           new _A_($UHC.$Base.$isUpper,[$c]);
          return new _A_($UHC.$Base.$_7c_7c,[$__3,$__]);});
$UHC.$Base.$__78__9318=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["ENQ"]);}),[]);
$UHC.$Base.$__78__9324=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["BEL"]);}),[]);
$UHC.$Base.$__78__9322=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9324,$UHC.$Base.$_5b_5d]);}),[]);
$UHC.$Base.$__78__9321=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["ACK"]);}),[]);
$UHC.$Base.$__78__9319=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9321,$UHC.$Base.$__78__9322]);}),[]);
$UHC.$Base.$__78__9316=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9318,$UHC.$Base.$__78__9319]);}),[]);
$UHC.$Base.$__78__9315=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["EOT"]);}),[]);
$UHC.$Base.$__78__9313=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9315,$UHC.$Base.$__78__9316]);}),[]);
$UHC.$Base.$__78__9363=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["DC2"]);}),[]);
$UHC.$Base.$__78__9366=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["DC3"]);}),[]);
$UHC.$Base.$__78__9364=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9366,$UHC.$Base.$_5b_5d]);}),[]);
$UHC.$Base.$__78__9361=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9363,$UHC.$Base.$__78__9364]);}),[]);
$UHC.$Base.$__78__9360=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["DC1"]);}),[]);
$UHC.$Base.$__78__9358=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9360,$UHC.$Base.$__78__9361]);}),[]);
$UHC.$Base.$__78__9357=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["DLE"]);}),[]);
$UHC.$Base.$__78__9355=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9357,$UHC.$Base.$__78__9358]);}),[]);
$UHC.$Base.$__78__9399=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["FS"]);}),[]);
$UHC.$Base.$__78__9402=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["GS"]);}),[]);
$UHC.$Base.$__78__9408=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["US"]);}),[]);
$UHC.$Base.$__78__9406=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9408,$UHC.$Base.$_5b_5d]);}),[]);
$UHC.$Base.$__78__9405=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["RS"]);}),[]);
$UHC.$Base.$__78__9403=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9405,$UHC.$Base.$__78__9406]);}),[]);
$UHC.$Base.$__78__9400=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9402,$UHC.$Base.$__78__9403]);}),[]);
$UHC.$Base.$__78__9397=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9399,$UHC.$Base.$__78__9400]);}),[]);
$UHC.$Base.$__78__9411=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["SP"]);}),[]);
$UHC.$Base.$__78__9409=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9411,$UHC.$Base.$_5b_5d]);}),[]);
$UHC.$Base.$__78__9395=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2b_2b,[$UHC.$Base.$__78__9397,$UHC.$Base.$__78__9409]);}),[]);
$UHC.$Base.$__78__9385=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["CAN"]);}),[]);
$UHC.$Base.$__78__9394=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["ESC"]);}),[]);
$UHC.$Base.$__78__9392=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9394,$UHC.$Base.$_5b_5d]);}),[]);
$UHC.$Base.$__78__9391=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["SUB"]);}),[]);
$UHC.$Base.$__78__9389=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9391,$UHC.$Base.$__78__9392]);}),[]);
$UHC.$Base.$__78__9388=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["EM"]);}),[]);
$UHC.$Base.$__78__9386=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9388,$UHC.$Base.$__78__9389]);}),[]);
$UHC.$Base.$__78__9383=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9385,$UHC.$Base.$__78__9386]);}),[]);
$UHC.$Base.$__78__9381=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2b_2b,[$UHC.$Base.$__78__9383,$UHC.$Base.$__78__9395]);}),[]);
$UHC.$Base.$__78__9371=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["DC4"]);}),[]);
$UHC.$Base.$__78__9374=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["NAK"]);}),[]);
$UHC.$Base.$__78__9377=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["SYN"]);}),[]);
$UHC.$Base.$__78__9380=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["ETB"]);}),[]);
$UHC.$Base.$__78__9378=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9380,$UHC.$Base.$_5b_5d]);}),[]);
$UHC.$Base.$__78__9375=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9377,$UHC.$Base.$__78__9378]);}),[]);
$UHC.$Base.$__78__9372=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9374,$UHC.$Base.$__78__9375]);}),[]);
$UHC.$Base.$__78__9369=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9371,$UHC.$Base.$__78__9372]);}),[]);
$UHC.$Base.$__78__9367=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2b_2b,[$UHC.$Base.$__78__9369,$UHC.$Base.$__78__9381]);}),[]);
$UHC.$Base.$__78__9353=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2b_2b,[$UHC.$Base.$__78__9355,$UHC.$Base.$__78__9367]);}),[]);
$UHC.$Base.$__78__9352=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["SI"]);}),[]);
$UHC.$Base.$__78__9350=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9352,$UHC.$Base.$_5b_5d]);}),[]);
$UHC.$Base.$__78__9349=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["SO"]);}),[]);
$UHC.$Base.$__78__9347=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9349,$UHC.$Base.$__78__9350]);}),[]);
$UHC.$Base.$__78__9346=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["CR"]);}),[]);
$UHC.$Base.$__78__9344=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9346,$UHC.$Base.$__78__9347]);}),[]);
$UHC.$Base.$__78__9343=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["FF"]);}),[]);
$UHC.$Base.$__78__9341=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9343,$UHC.$Base.$__78__9344]);}),[]);
$UHC.$Base.$__78__9339=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2b_2b,[$UHC.$Base.$__78__9341,$UHC.$Base.$__78__9353]);}),[]);
$UHC.$Base.$__78__9332=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["HT"]);}),[]);
$UHC.$Base.$__78__9335=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["LF"]);}),[]);
$UHC.$Base.$__78__9338=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["VT"]);}),[]);
$UHC.$Base.$__78__9336=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9338,$UHC.$Base.$_5b_5d]);}),[]);
$UHC.$Base.$__78__9333=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9335,$UHC.$Base.$__78__9336]);}),[]);
$UHC.$Base.$__78__9330=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9332,$UHC.$Base.$__78__9333]);}),[]);
$UHC.$Base.$__78__9329=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["BS"]);}),[]);
$UHC.$Base.$__78__9327=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9329,$UHC.$Base.$__78__9330]);}),[]);
$UHC.$Base.$__78__9325=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2b_2b,[$UHC.$Base.$__78__9327,$UHC.$Base.$__78__9339]);}),[]);
$UHC.$Base.$__78__9311=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2b_2b,[$UHC.$Base.$__78__9313,$UHC.$Base.$__78__9325]);}),[]);
$UHC.$Base.$__78__9301=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["NUL"]);}),[]);
$UHC.$Base.$__78__9307=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["STX"]);}),[]);
$UHC.$Base.$__78__9310=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["ETX"]);}),[]);
$UHC.$Base.$__78__9308=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9310,$UHC.$Base.$_5b_5d]);}),[]);
$UHC.$Base.$__78__9305=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9307,$UHC.$Base.$__78__9308]);}),[]);
$UHC.$Base.$__78__9304=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["SOH"]);}),[]);
$UHC.$Base.$__78__9302=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9304,$UHC.$Base.$__78__9305]);}),[]);
$UHC.$Base.$__78__9299=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$__78__9301,$UHC.$Base.$__78__9302]);}),[]);
$UHC.$Base.$__78__9297=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2b_2b,[$UHC.$Base.$__78__9299,$UHC.$Base.$__78__9311]);}),[]);
$UHC.$Base.$__78__9294=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$enumFromTo,[$UHC.$Base.$Enum__DCT74__60__0,0,32]);}),[]);
$UHC.$Base.$asciiTab=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$zip,[$UHC.$Base.$__78__9294,$UHC.$Base.$__78__9297]);}),[]);
$UHC.$Base.$_24okUNQ7707=
 new _F_(function($mne,$_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           _e_($__[0]);
          var $__swJSW63__0;
          switch($__6._tag_)
           {case 0:
             $__swJSW63__0=
              $UHC.$Base.$_5b_5d;
             break;
            case 1:
             var $__9=
              [$mne,$__[1]];
             var $__10=
              new _A_($UHC.$Base.$_3a,[$__9,$UHC.$Base.$_5b_5d]);
             $__swJSW63__0=
              $__10;
             break;}
          return $__swJSW63__0;});
$UHC.$Base.$lexmatch=
 new _F_(function($__,$x1,$x2)
         {var $__4=
           [$x1,$x2];
          var $x15=
           _e_($x1);
          var $__swJSW64__0;
          switch($x15._tag_)
           {case 0:
             var $x28=
              _e_($x2);
             var $__swJSW65__0;
             switch($x28._tag_)
              {case 0:
                var $__11=
                 new _A_($UHC.$Base.$_3d_3d,[$__,$x15._1,$x28._1]);
                var $__12=
                 _e_($__11);
                var $__swJSW66__0;
                switch($__12._tag_)
                 {case 0:
                   $__swJSW66__0=
                    $__4;
                   break;
                  case 1:
                   var $__13=
                    new _A_($UHC.$Base.$lexmatch,[$__,$x15._2,$x28._2]);
                   $__swJSW66__0=
                    $__13;
                   break;}
                $__swJSW65__0=
                 $__swJSW66__0;
                break;
               case 1:
                $__swJSW65__0=
                 $__4;
                break;}
             $__swJSW64__0=
              $__swJSW65__0;
             break;
            case 1:
             $__swJSW64__0=
              $__4;
             break;}
          return $__swJSW64__0;});
$UHC.$Base.$_24okUNQ7692=
 new _F_(function($__,$_24x)
         {var $__3=
           _e_($_24x);
          var $__6=
           new _A_($UHC.$Base.$lexmatch,[$UHC.$Base.$Eq__DCT74__56__0,$__3[1],$__]);
          var $__7=
           new _A_($UHC.$Base.$_3a,[$__6,$UHC.$Base.$_5b_5d]);
          var $__8=
           new _A_($UHC.$Base.$_24okUNQ7707,[$__3[1]]);
          return new _A_($UHC.$Base.$concatMap,[$__8,$__7]);});
$UHC.$Base.$__76__32943__0NEW4820UNQ7690=
 new _F_(function($table,$__)
         {var $__3=
           new _A_($UHC.$Base.$_24okUNQ7692,[$__]);
          return new _A_($UHC.$Base.$concatMap,[$__3,$table]);});
$UHC.$Base.$isUpper=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primCharIsUpper($__2);});
$UHC.$Base.$__76__32620__0NEW4812UNQ7684CCN=
 new _F_(function($table,$__,$c)
         {var $__4=
           new _A_($UHC.$Base.$isDigit,[$c]);
          var $__5=
           _e_($__4);
          var $__swJSW68__0;
          switch($__5._tag_)
           {case 0:
             var $__6=
              new _A_($UHC.$Base.$isUpper,[$c]);
             var $__7=
              _e_($__6);
             var $__swJSW69__0;
             switch($__7._tag_)
              {case 0:
                $__swJSW69__0=
                 $UHC.$Base.$_5b_5d;
                break;
               case 1:
                var $__8=
                 new _A_($UHC.$Base.$__76__32943__0NEW4820UNQ7690,[$table,$__]);
                var $__9=
                 _e_($__8);
                var $__swJSW70__0;
                switch($__9._tag_)
                 {case 0:
                   var $__12=
                    new _A_($UHC.$Base.$_3a,[$__9._1,$UHC.$Base.$_5b_5d]);
                   $__swJSW70__0=
                    $__12;
                   break;
                  case 1:
                   $__swJSW70__0=
                    $UHC.$Base.$_5b_5d;
                   break;}
                $__swJSW69__0=
                 $__swJSW70__0;
                break;}
             $__swJSW68__0=
              $__swJSW69__0;
             break;
            case 1:
             var $__13=
              new _A_($UHC.$Base.$span,[$UHC.$Base.$isDigit,$__]);
             var $__14=
              new _A_($UHC.$Base.$_3a,[$__13,$UHC.$Base.$_5b_5d]);
             $__swJSW68__0=
              $__14;
             break;}
          return $__swJSW68__0;});
$UHC.$Base.$isOctDigit=
 new _F_(function($c)
         {var $__=
           new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,55]);
          var $__3=
           new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,48]);
          return new _A_($UHC.$Base.$_26_26,[$__3,$__]);});
$UHC.$Base.$isHexDigit=
 new _F_(function($c)
         {var $__=
           new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,102]);
          var $__3=
           new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,97]);
          var $__4=
           new _A_($UHC.$Base.$_26_26,[$__3,$__]);
          var $__5=
           new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,70]);
          var $__6=
           new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,65]);
          var $__7=
           new _A_($UHC.$Base.$_26_26,[$__6,$__5]);
          var $__8=
           new _A_($UHC.$Base.$_7c_7c,[$__7,$__4]);
          var $__9=
           new _A_($UHC.$Base.$isDigit,[$c]);
          return new _A_($UHC.$Base.$_7c_7c,[$__9,$__8]);});
$UHC.$Base.$prefixUNQ7643=
 new _F_(function($c,$__)
         {var $__3=
           _e_($__);
          var $__6=
           new _A_($UHC.$Base.$_3a,[$c,$__3[0]]);
          var $__7=
           [$__6,$__3[1]];
          return $__7;});
$UHC.$Base.$cNEW4807UNQ7683CCN=
 new _F_(function($table,$__,$s,$c)
         {var $__5=
           new _A_($UHC.$Base.$__76__32620__0NEW4812UNQ7684CCN,[$table,$__,$c]);
          var $c6=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,94,$c]));
          var $__swJSW72__0;
          switch($c6._tag_)
           {case 0:
             var $c7=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,111,$c]));
             var $__swJSW73__0;
             switch($c7._tag_)
              {case 0:
                var $c8=
                 _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,120,$c]));
                var $__swJSW74__0;
                switch($c8._tag_)
                 {case 0:
                   $__swJSW74__0=
                    $__5;
                   break;
                  case 1:
                   var $__9=
                    new _A_($UHC.$Base.$span,[$UHC.$Base.$isHexDigit,$s]);
                   var $__10=
                    new _A_($UHC.$Base.$prefixUNQ7643,[120,$__9]);
                   var $__11=
                    new _A_($UHC.$Base.$_3a,[$__10,$UHC.$Base.$_5b_5d]);
                   $__swJSW74__0=
                    $__11;
                   break;}
                $__swJSW73__0=
                 $__swJSW74__0;
                break;
               case 1:
                var $__12=
                 new _A_($UHC.$Base.$span,[$UHC.$Base.$isOctDigit,$s]);
                var $__13=
                 new _A_($UHC.$Base.$prefixUNQ7643,[111,$__12]);
                var $__14=
                 new _A_($UHC.$Base.$_3a,[$__13,$UHC.$Base.$_5b_5d]);
                $__swJSW73__0=
                 $__14;
                break;}
             $__swJSW72__0=
              $__swJSW73__0;
             break;
            case 1:
             var $s15=
              _e_($s);
             var $__swJSW75__0;
             switch($s15._tag_)
              {case 0:
                var $__18=
                 new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,$s15._1,95]);
                var $__19=
                 new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__58__0,$s15._1,64]);
                var $__20=
                 new _A_($UHC.$Base.$_26_26,[$__19,$__18]);
                var $__21=
                 _e_($__20);
                var $__swJSW76__0;
                switch($__21._tag_)
                 {case 0:
                   $__swJSW76__0=
                    $__5;
                   break;
                  case 1:
                   var $__22=
                    new _A_($UHC.$Base.$_3a,[$s15._1,$UHC.$Base.$_5b_5d]);
                   var $__23=
                    new _A_($UHC.$Base.$_3a,[94,$__22]);
                   var $__24=
                    [$__23,$s15._2];
                   var $__25=
                    new _A_($UHC.$Base.$_3a,[$__24,$UHC.$Base.$_5b_5d]);
                   $__swJSW76__0=
                    $__25;
                   break;}
                $__swJSW75__0=
                 $__swJSW76__0;
                break;
               case 1:
                $__swJSW75__0=
                 $__5;
                break;}
             $__swJSW72__0=
              $__swJSW75__0;
             break;}
          return $__swJSW72__0;});
$UHC.$Base.$lexEscUNQ7650=
 new _F_(function($table,$x1)
         {var $__=
           _e_($x1);
          var $__swJSW77__0;
          switch($__._tag_)
           {case 0:
             var $c6=
              new _A_($UHC.$Base.$cNEW4807UNQ7683CCN,[$table,$__,$__._2,$__._1]);
             var $__7=
              new _A_($UHC.$Base.$packedStringToString,["abfnrtv\\\"'"]);
             var $__8=
              new _A_($UHC.$Base.$elem,[$UHC.$Base.$Eq__DCT74__56__0,$__._1,$__7]);
             var $__9=
              _e_($__8);
             var $__swJSW78__0;
             switch($__9._tag_)
              {case 0:
                $__swJSW78__0=
                 $c6;
                break;
               case 1:
                var $__10=
                 new _A_($UHC.$Base.$_3a,[$__._1,$UHC.$Base.$_5b_5d]);
                var $__11=
                 [$__10,$__._2];
                var $__12=
                 new _A_($UHC.$Base.$_3a,[$__11,$UHC.$Base.$_5b_5d]);
                $__swJSW78__0=
                 $__12;
                break;}
             $__swJSW77__0=
              $__swJSW78__0;
             break;
            case 1:
             $__swJSW77__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW77__0;});
$UHC.$Base.$lexLitChar=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW79__0;
          switch($__._tag_)
           {case 0:
             var $__5=
              new _A_($UHC.$Base.$packedStringToString,["DEL"]);
             var $__6=
              [127,$__5];
             var $table=
              new _A_($UHC.$Base.$_3a,[$__6,$UHC.$Base.$asciiTab]);
             var $__8=
              new _A_($UHC.$Base.$_2f_3d,[$UHC.$Base.$Eq__DCT74__56__0,$__._1,92]);
             var $__9=
              _e_($__8);
             var $__swJSW80__0;
             switch($__9._tag_)
              {case 0:
                var $__10=
                 _e_($UHC.$Base.$otherwise);
                var $__swJSW81__0;
                switch($__10._tag_)
                 {case 0:
                   $__swJSW81__0=
                    $UHC.$Base.$undefined;
                   break;
                  case 1:
                   var $__11=
                    new _A_($UHC.$Base.$lexEscUNQ7650,[$table,$__._2]);
                   var $__12=
                    new _A_($UHC.$Base.$prefixUNQ7643,[92]);
                   var $__13=
                    new _A_($UHC.$Base.$map,[$__12,$__11]);
                   $__swJSW81__0=
                    $__13;
                   break;}
                $__swJSW80__0=
                 $__swJSW81__0;
                break;
               case 1:
                var $__14=
                 new _A_($UHC.$Base.$_3a,[$__._1,$UHC.$Base.$_5b_5d]);
                var $__15=
                 [$__14,$__._2];
                var $__16=
                 new _A_($UHC.$Base.$_3a,[$__15,$UHC.$Base.$_5b_5d]);
                $__swJSW80__0=
                 $__16;
                break;}
             $__swJSW79__0=
              $__swJSW80__0;
             break;
            case 1:
             $__swJSW79__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW79__0;});
$UHC.$Base.$pNEW1799UNQ3626CCN=
 new _F_(function($x1,$x2)
         {var $x23=
           _e_($x2);
          var $__swJSW82__0;
          switch($x23._tag_)
           {case 0:
             var $__=
              new _A_($UHC.$Base.$span,[$x1,$x23._2]);
             var $zs=
              new _A_($UHC.$Base.$zsNEW1804UNQ3636,[$__]);
             var $ys=
              new _A_($UHC.$Base.$ysNEW1807UNQ3635,[$__]);
             var $__9=
              new _A_($x1,[$x23._1]);
             var $__10=
              _e_($__9);
             var $__swJSW83__0;
             switch($__10._tag_)
              {case 0:
                var $__11=
                 _e_($UHC.$Base.$otherwise);
                var $__swJSW84__0;
                switch($__11._tag_)
                 {case 0:
                   $__swJSW84__0=
                    $UHC.$Base.$undefined;
                   break;
                  case 1:
                   var $__12=
                    [$UHC.$Base.$_5b_5d,$x23];
                   $__swJSW84__0=
                    $__12;
                   break;}
                $__swJSW83__0=
                 $__swJSW84__0;
                break;
               case 1:
                var $__13=
                 new _A_($UHC.$Base.$_3a,[$x23._1,$ys]);
                var $__14=
                 [$__13,$zs];
                $__swJSW83__0=
                 $__14;
                break;}
             $__swJSW82__0=
              $__swJSW83__0;
             break;
            case 1:
             $__swJSW82__0=
              $UHC.$Base.$undefined;
             break;}
          return $__swJSW82__0;});
$UHC.$Base.$zsNEW1804UNQ3636=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Base.$ysNEW1807UNQ3635=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Base.$span=
 new _F_(function($x1,$x2)
         {var $p=
           new _A_($UHC.$Base.$pNEW1799UNQ3626CCN,[$x1,$x2]);
          var $x24=
           _e_($x2);
          var $__swJSW87__0;
          switch($x24._tag_)
           {case 0:
             $__swJSW87__0=
              $p;
             break;
            case 1:
             var $__=
              [$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d];
             $__swJSW87__0=
              $__;
             break;}
          return $__swJSW87__0;});
$UHC.$Base.$_24okUNQ3658=
 new _F_(function($_24x)
         {var $__=
           _e_($_24x);
          var $cs5=
           _e_($__[0]);
          var $__swJSW89__0;
          switch($cs5._tag_)
           {case 0:
             var $__8=
              [$cs5,$__[1]];
             var $__9=
              new _A_($UHC.$Base.$_3a,[$__8,$UHC.$Base.$_5b_5d]);
             $__swJSW89__0=
              $__9;
             break;
            case 1:
             $__swJSW89__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW89__0;});
$UHC.$Base.$nonnull=
 new _F_(function($p,$s)
         {var $__=
           new _A_($UHC.$Base.$span,[$p,$s]);
          var $__4=
           new _A_($UHC.$Base.$_3a,[$__,$UHC.$Base.$_5b_5d]);
          return new _A_($UHC.$Base.$concatMap,[$UHC.$Base.$_24okUNQ3658,$__4]);});
$UHC.$Base.$lexDigits=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$nonnull,[$UHC.$Base.$isDigit]);}),[]);
$UHC.$Base.$any=
 new _F_(function($p)
         {var $__=
           new _A_($UHC.$Base.$map,[$p]);
          return new _A_($UHC.$Base.$_2e,[$UHC.$Base.$or,$__]);});
$UHC.$Base.$elem=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$_3d_3d,[$__]);
          return new _A_($UHC.$Base.$_2e,[$UHC.$Base.$any,$__2]);});
$UHC.$Base.$primCmpChar=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primCmpInt($__3,$__4);});
$UHC.$Base.$Ord__NEW2187UNQ10858EVLDCT74__58__0RDC=
 new _F_(function($Ord__)
         {var $Ord__2=
           _e_(new _A_($UHC.$Base.$Ord__CLS74__5__0,[$Ord__]));
          var $__11=
           {_tag_:0,_1:$Ord__2._1,_2:$Ord__2._2,_3:$Ord__2._3,_4:$Ord__2._4,_5:$UHC.$Base.$Eq__DCT74__56__0,_6:$UHC.$Base.$primCmpChar,_7:$Ord__2._7,_8:$Ord__2._8};
          return $__11;});
$UHC.$Base.$Ord__NEW2185UNQ10857DCT74__58__0RDC=
 new _F_(function($Ord__)
         {var $Ord__2=
           new _A_($UHC.$Base.$Ord__NEW2187UNQ10858EVLDCT74__58__0RDC,[$Ord__]);
          return $Ord__2;});
$UHC.$Base.$Ord__UNQ10857DCT74__58__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Ord__NEW2185UNQ10857DCT74__58__0RDC,[$UHC.$Base.$Ord__UNQ10857DCT74__58__0RDC]);}),[]);
$UHC.$Base.$Ord__DCT74__58__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Ord__UNQ10857DCT74__58__0RDC;}),[]);
$UHC.$Base.$isDigit=
 new _F_(function($c)
         {var $__=
           new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,57]);
          var $__3=
           new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__58__0,$c,48]);
          return new _A_($UHC.$Base.$_26_26,[$__3,$__]);});
$UHC.$Base.$cNEW4871UNQ7768CCN=
 new _F_(function($s,$c)
         {var $__=
           new _A_($UHC.$Base.$__76__33435__0NEW4874UNQ7769CCN,[$s,$c]);
          var $c4=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,34,$c]));
          var $__swJSW91__0;
          switch($c4._tag_)
           {case 0:
             var $c5=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,39,$c]));
             var $__swJSW92__0;
             switch($c5._tag_)
              {case 0:
                $__swJSW92__0=
                 $__;
                break;
               case 1:
                var $__6=
                 new _A_($UHC.$Base.$Eq__DCT74__394__0,[$UHC.$Base.$Eq__DCT74__56__0]);
                var $__7=
                 new _A_($UHC.$Base.$lexLitChar,[$s]);
                var $__8=
                 new _A_($UHC.$Base.$_24okUNQ7977,[$__6]);
                $__swJSW92__0=
                 new _A_($UHC.$Base.$concatMap,[$__8,$__7]);
                break;}
             $__swJSW91__0=
              $__swJSW92__0;
             break;
            case 1:
             var $__9=
              new _A_($UHC.$Base.$lexStringUNQ8005,[$s]);
             $__swJSW91__0=
              new _A_($UHC.$Base.$concatMap,[$UHC.$Base.$_24okUNQ8006,$__9]);
             break;}
          return $__swJSW91__0;});
$UHC.$Base.$__76__33435__0NEW4874UNQ7769CCN=
 new _F_(function($s,$c)
         {var $__=
           new _A_($UHC.$Base.$isSymUNQ7773,[$c]);
          var $__4=
           _e_($__);
          var $__swJSW93__0;
          switch($__4._tag_)
           {case 0:
             var $__5=
              new _A_($UHC.$Base.$isAlpha,[$c]);
             var $__6=
              _e_($__5);
             var $__swJSW94__0;
             switch($__6._tag_)
              {case 0:
                var $__7=
                 new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,$c,95]);
                var $__8=
                 _e_($__7);
                var $__swJSW95__0;
                switch($__8._tag_)
                 {case 0:
                   var $__9=
                    new _A_($UHC.$Base.$isSingleUNQ7777,[$c]);
                   var $__10=
                    _e_($__9);
                   var $__swJSW96__0;
                   switch($__10._tag_)
                    {case 0:
                      var $__11=
                       new _A_($UHC.$Base.$isDigit,[$c]);
                      var $__12=
                       _e_($__11);
                      var $__swJSW97__0;
                      switch($__12._tag_)
                       {case 0:
                         var $__13=
                          _e_($UHC.$Base.$otherwise);
                         var $__swJSW98__0;
                         switch($__13._tag_)
                          {case 0:
                            $__swJSW98__0=
                             $UHC.$Base.$undefined;
                            break;
                           case 1:
                            $__swJSW98__0=
                             $UHC.$Base.$_5b_5d;
                            break;}
                         $__swJSW97__0=
                          $__swJSW98__0;
                         break;
                        case 1:
                         var $__14=
                          new _A_($UHC.$Base.$span,[$UHC.$Base.$isDigit,$s]);
                         var $__15=
                          new _A_($UHC.$Base.$_3a,[$__14,$UHC.$Base.$_5b_5d]);
                         var $__16=
                          new _A_($UHC.$Base.$_24okUNQ7898,[$c]);
                         $__swJSW97__0=
                          new _A_($UHC.$Base.$concatMap,[$__16,$__15]);
                         break;}
                      $__swJSW96__0=
                       $__swJSW97__0;
                      break;
                     case 1:
                      var $__17=
                       new _A_($UHC.$Base.$_3a,[$c,$UHC.$Base.$_5b_5d]);
                      var $__18=
                       [$__17,$s];
                      var $__19=
                       new _A_($UHC.$Base.$_3a,[$__18,$UHC.$Base.$_5b_5d]);
                      $__swJSW96__0=
                       $__19;
                      break;}
                   $__swJSW95__0=
                    $__swJSW96__0;
                   break;
                  case 1:
                   var $__20=
                    new _A_($UHC.$Base.$span,[$UHC.$Base.$isIdCharUNQ7781,$s]);
                   var $__21=
                    _e_($__20);
                   var $__24=
                    new _A_($UHC.$Base.$_3a,[$c,$__21[0]]);
                   var $__25=
                    [$__24,$__21[1]];
                   var $__26=
                    new _A_($UHC.$Base.$_3a,[$__25,$UHC.$Base.$_5b_5d]);
                   var $__27=
                    _e_($__21[0]);
                   var $__swJSW100__0;
                   switch($__27._tag_)
                    {case 0:
                      $__swJSW100__0=
                       $__26;
                      break;
                     case 1:
                      var $__30=
                       new _A_($UHC.$Base.$_3a,[$c,$UHC.$Base.$_5b_5d]);
                      var $__31=
                       [$__30,$s];
                      var $__32=
                       new _A_($UHC.$Base.$_3a,[$__31,$UHC.$Base.$_5b_5d]);
                      $__swJSW100__0=
                       $__32;
                      break;}
                   $__swJSW95__0=
                    $__swJSW100__0;
                   break;}
                $__swJSW94__0=
                 $__swJSW95__0;
                break;
               case 1:
                var $__33=
                 new _A_($UHC.$Base.$span,[$UHC.$Base.$isIdCharUNQ7781,$s]);
                var $__34=
                 new _A_($UHC.$Base.$_3a,[$__33,$UHC.$Base.$_5b_5d]);
                var $__35=
                 new _A_($UHC.$Base.$_24okUNQ7939,[$c]);
                $__swJSW94__0=
                 new _A_($UHC.$Base.$concatMap,[$__35,$__34]);
                break;}
             $__swJSW93__0=
              $__swJSW94__0;
             break;
            case 1:
             var $__36=
              new _A_($UHC.$Base.$span,[$UHC.$Base.$isSymUNQ7773,$s]);
             var $__37=
              new _A_($UHC.$Base.$_3a,[$__36,$UHC.$Base.$_5b_5d]);
             var $__38=
              new _A_($UHC.$Base.$_24okUNQ7955,[$c]);
             $__swJSW93__0=
              new _A_($UHC.$Base.$concatMap,[$__38,$__37]);
             break;}
          return $__swJSW93__0;});
$UHC.$Base.$isSymUNQ7773=
 new _F_(function($c)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["!@#$%&*+./<=>?\\^|:-~"]);
          return new _A_($UHC.$Base.$elem,[$UHC.$Base.$Eq__DCT74__56__0,$c,$__]);});
$UHC.$Base.$isSingleUNQ7777=
 new _F_(function($c)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,[",;()[]{}_`"]);
          return new _A_($UHC.$Base.$elem,[$UHC.$Base.$Eq__DCT74__56__0,$c,$__]);});
$UHC.$Base.$lexExpUNQ7775=
 new _F_(function($x1)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,[""]);
          var $__3=
           [$__,$x1];
          var $__4=
           new _A_($UHC.$Base.$_3a,[$__3,$UHC.$Base.$_5b_5d]);
          var $__5=
           _e_($x1);
          var $__swJSW101__0;
          switch($__5._tag_)
           {case 0:
             var $__8=
              new _A_($UHC.$Base.$packedStringToString,["eE"]);
             var $__9=
              new _A_($UHC.$Base.$elem,[$UHC.$Base.$Eq__DCT74__56__0,$__5._1,$__8]);
             var $__10=
              _e_($__9);
             var $__swJSW102__0;
             switch($__10._tag_)
              {case 0:
                $__swJSW102__0=
                 $__4;
                break;
               case 1:
                var $__11=
                 new _A_($UHC.$Base.$__78__9635NEW4889,[$__5._1,$__5._2]);
                var $__12=
                 new _A_($UHC.$Base.$__78__9603NEW4898,[$__5._1,$__5._2]);
                var $__13=
                 new _A_($UHC.$Base.$_2b_2b,[$__12,$__11]);
                $__swJSW102__0=
                 $__13;
                break;}
             $__swJSW101__0=
              $__swJSW102__0;
             break;
            case 1:
             $__swJSW101__0=
              $__4;
             break;}
          return $__swJSW101__0;});
$UHC.$Base.$__78__9635NEW4889=
 new _F_(function($e,$s)
         {var $__=
           new _A_($UHC.$Base.$lexDigits,[$s]);
          var $__4=
           new _A_($UHC.$Base.$_24okUNQ7824,[$e]);
          return new _A_($UHC.$Base.$concatMap,[$__4,$__]);});
$UHC.$Base.$_24okUNQ7824=
 new _F_(function($e,$_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           new _A_($UHC.$Base.$_3a,[$e,$__[0]]);
          var $__7=
           [$__6,$__[1]];
          var $__8=
           new _A_($UHC.$Base.$_3a,[$__7,$UHC.$Base.$_5b_5d]);
          return $__8;});
$UHC.$Base.$__78__9603NEW4898=
 new _F_(function($e,$s)
         {var $__=
           new _A_($UHC.$Base.$_3a,[$s,$UHC.$Base.$_5b_5d]);
          var $__4=
           new _A_($UHC.$Base.$_24okUNQ7798,[$e]);
          return new _A_($UHC.$Base.$concatMap,[$__4,$__]);});
$UHC.$Base.$_24okUNQ7798=
 new _F_(function($e,$_24x)
         {var $__=
           _e_($_24x);
          var $__swJSW104__0;
          switch($__._tag_)
           {case 0:
             var $__6=
              new _A_($UHC.$Base.$packedStringToString,["+-"]);
             var $__7=
              new _A_($UHC.$Base.$elem,[$UHC.$Base.$Eq__DCT74__56__0,$__._1,$__6]);
             var $__8=
              _e_($__7);
             var $__swJSW105__0;
             switch($__8._tag_)
              {case 0:
                $__swJSW105__0=
                 $UHC.$Base.$_5b_5d;
                break;
               case 1:
                var $__9=
                 new _A_($UHC.$Base.$lexDigits,[$__._2]);
                var $__10=
                 new _A_($UHC.$Base.$_24okUNQ7808,[$e,$__._1]);
                $__swJSW105__0=
                 new _A_($UHC.$Base.$concatMap,[$__10,$__9]);
                break;}
             $__swJSW104__0=
              $__swJSW105__0;
             break;
            case 1:
             $__swJSW104__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW104__0;});
$UHC.$Base.$_24okUNQ7808=
 new _F_(function($e,$c,$_24x)
         {var $__=
           _e_($_24x);
          var $__7=
           new _A_($UHC.$Base.$_3a,[$c,$__[0]]);
          var $__8=
           new _A_($UHC.$Base.$_3a,[$e,$__7]);
          var $__9=
           [$__8,$__[1]];
          var $__10=
           new _A_($UHC.$Base.$_3a,[$__9,$UHC.$Base.$_5b_5d]);
          return $__10;});
$UHC.$Base.$lexFracExpUNQ7779=
 new _F_(function($x1)
         {var $__=
           new _A_($UHC.$Base.$lexExpUNQ7775,[$x1]);
          var $__3=
           _e_($x1);
          var $__swJSW107__0;
          switch($__3._tag_)
           {case 0:
             var $__6=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,46,$__3._1]));
             var $__swJSW108__0;
             switch($__6._tag_)
              {case 0:
                $__swJSW108__0=
                 $__;
                break;
               case 1:
                var $__7=
                 _e_($__3._2);
                var $__swJSW109__0;
                switch($__7._tag_)
                 {case 0:
                   var $__10=
                    new _A_($UHC.$Base.$isDigit,[$__7._1]);
                   var $__11=
                    _e_($__10);
                   var $__swJSW110__0;
                   switch($__11._tag_)
                    {case 0:
                      $__swJSW110__0=
                       $__;
                      break;
                     case 1:
                      var $__12=
                       new _A_($UHC.$Base.$_3a,[$__7._1,$__7._2]);
                      var $__13=
                       new _A_($UHC.$Base.$lexDigits,[$__12]);
                      $__swJSW110__0=
                       new _A_($UHC.$Base.$concatMap,[$UHC.$Base.$_24okUNQ7856,$__13]);
                      break;}
                   $__swJSW109__0=
                    $__swJSW110__0;
                   break;
                  case 1:
                   $__swJSW109__0=
                    $__;
                   break;}
                $__swJSW108__0=
                 $__swJSW109__0;
                break;}
             $__swJSW107__0=
              $__swJSW108__0;
             break;
            case 1:
             $__swJSW107__0=
              $__;
             break;}
          return $__swJSW107__0;});
$UHC.$Base.$_24okUNQ7856=
 new _F_(function($_24x)
         {var $__=
           _e_($_24x);
          var $__5=
           new _A_($UHC.$Base.$lexExpUNQ7775,[$__[1]]);
          var $__6=
           new _A_($UHC.$Base.$_24okUNQ7869,[$__[0]]);
          return new _A_($UHC.$Base.$concatMap,[$__6,$__5]);});
$UHC.$Base.$_24okUNQ7869=
 new _F_(function($ds,$_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           new _A_($UHC.$Base.$_2b_2b,[$ds,$__[0]]);
          var $__7=
           new _A_($UHC.$Base.$_3a,[46,$__6]);
          var $__8=
           [$__7,$__[1]];
          var $__9=
           new _A_($UHC.$Base.$_3a,[$__8,$UHC.$Base.$_5b_5d]);
          return $__9;});
$UHC.$Base.$isIdCharUNQ7781=
 new _F_(function($c)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["_'"]);
          var $__3=
           new _A_($UHC.$Base.$elem,[$UHC.$Base.$Eq__DCT74__56__0,$c,$__]);
          var $__4=
           new _A_($UHC.$Base.$isAlphaNum,[$c]);
          return new _A_($UHC.$Base.$_7c_7c,[$__4,$__3]);});
$UHC.$Base.$_24okUNQ7898=
 new _F_(function($c,$_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           new _A_($UHC.$Base.$lexFracExpUNQ7779,[$__[1]]);
          var $__7=
           new _A_($UHC.$Base.$_24okUNQ7911,[$c,$__[0]]);
          return new _A_($UHC.$Base.$concatMap,[$__7,$__6]);});
$UHC.$Base.$_24okUNQ7911=
 new _F_(function($c,$ds,$_24x)
         {var $__=
           _e_($_24x);
          var $__7=
           new _A_($UHC.$Base.$_2b_2b,[$ds,$__[0]]);
          var $__8=
           new _A_($UHC.$Base.$_3a,[$c,$__7]);
          var $__9=
           [$__8,$__[1]];
          var $__10=
           new _A_($UHC.$Base.$_3a,[$__9,$UHC.$Base.$_5b_5d]);
          return $__10;});
$UHC.$Base.$_24okUNQ7939=
 new _F_(function($c,$_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           new _A_($UHC.$Base.$_3a,[$c,$__[0]]);
          var $__7=
           [$__6,$__[1]];
          var $__8=
           new _A_($UHC.$Base.$_3a,[$__7,$UHC.$Base.$_5b_5d]);
          return $__8;});
$UHC.$Base.$_24okUNQ7955=
 new _F_(function($c,$_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           new _A_($UHC.$Base.$_3a,[$c,$__[0]]);
          var $__7=
           [$__6,$__[1]];
          var $__8=
           new _A_($UHC.$Base.$_3a,[$__7,$UHC.$Base.$_5b_5d]);
          return $__8;});
$UHC.$Base.$_24okUNQ7977=
 new _F_(function($__,$_24x)
         {var $__3=
           _e_($_24x);
          var $__6=
           _e_($__3[1]);
          var $__swJSW118__0;
          switch($__6._tag_)
           {case 0:
             var $__9=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,39,$__6._1]));
             var $__swJSW119__0;
             switch($__9._tag_)
              {case 0:
                $__swJSW119__0=
                 $UHC.$Base.$_5b_5d;
                break;
               case 1:
                var $__10=
                 new _A_($UHC.$Base.$packedStringToString,["'"]);
                var $__11=
                 new _A_($UHC.$Base.$_2f_3d,[$__,$__3[0],$__10]);
                var $__12=
                 _e_($__11);
                var $__swJSW120__0;
                switch($__12._tag_)
                 {case 0:
                   $__swJSW120__0=
                    $UHC.$Base.$_5b_5d;
                   break;
                  case 1:
                   var $__13=
                    new _A_($UHC.$Base.$packedStringToString,["'"]);
                   var $__14=
                    new _A_($UHC.$Base.$_2b_2b,[$__3[0],$__13]);
                   var $__15=
                    new _A_($UHC.$Base.$_3a,[39,$__14]);
                   var $__16=
                    [$__15,$__6._2];
                   var $__17=
                    new _A_($UHC.$Base.$_3a,[$__16,$UHC.$Base.$_5b_5d]);
                   $__swJSW120__0=
                    $__17;
                   break;}
                $__swJSW119__0=
                 $__swJSW120__0;
                break;}
             $__swJSW118__0=
              $__swJSW119__0;
             break;
            case 1:
             $__swJSW118__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW118__0;});
$UHC.$Base.$_24okUNQ8006=
 new _F_(function($_24x)
         {var $__=
           _e_($_24x);
          var $__5=
           new _A_($UHC.$Base.$_3a,[34,$__[0]]);
          var $__6=
           [$__5,$__[1]];
          var $__7=
           new _A_($UHC.$Base.$_3a,[$__6,$UHC.$Base.$_5b_5d]);
          return $__7;});
$UHC.$Base.$lexStrItemUNQ8003=
 new _F_(function($x1)
         {var $__=
           new _A_($UHC.$Base.$lexLitChar,[$x1]);
          var $__3=
           _e_($x1);
          var $__swJSW122__0;
          switch($__3._tag_)
           {case 0:
             var $__6=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,92,$__3._1]));
             var $__swJSW123__0;
             switch($__6._tag_)
              {case 0:
                $__swJSW123__0=
                 $__;
                break;
               case 1:
                var $__7=
                 _e_($__3._2);
                var $__swJSW124__0;
                switch($__7._tag_)
                 {case 0:
                   var $__10=
                    new _A_($UHC.$Base.$__76__33677__0NEW5011UNQ8021CCN,[$__,$__7._2,$__7._1]);
                   var $__11=
                    _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,38,$__7._1]));
                   var $__swJSW125__0;
                   switch($__11._tag_)
                    {case 0:
                      $__swJSW125__0=
                       $__10;
                      break;
                     case 1:
                      var $__12=
                       new _A_($UHC.$Base.$packedStringToString,["\\&"]);
                      var $__13=
                       [$__12,$__7._2];
                      var $__14=
                       new _A_($UHC.$Base.$_3a,[$__13,$UHC.$Base.$_5b_5d]);
                      $__swJSW125__0=
                       $__14;
                      break;}
                   $__swJSW124__0=
                    $__swJSW125__0;
                   break;
                  case 1:
                   $__swJSW124__0=
                    $__;
                   break;}
                $__swJSW123__0=
                 $__swJSW124__0;
                break;}
             $__swJSW122__0=
              $__swJSW123__0;
             break;
            case 1:
             $__swJSW122__0=
              $__;
             break;}
          return $__swJSW122__0;});
$UHC.$Base.$__76__33677__0NEW5011UNQ8021CCN=
 new _F_(function($__,$s,$__3)
         {var $__4=
           new _A_($UHC.$Base.$isSpace,[$__3]);
          var $__5=
           _e_($__4);
          var $__swJSW126__0;
          switch($__5._tag_)
           {case 0:
             $__swJSW126__0=
              $__;
             break;
            case 1:
             var $__6=
              new _A_($UHC.$Base.$dropWhile,[$UHC.$Base.$isSpace,$s]);
             var $__7=
              new _A_($UHC.$Base.$_3a,[$__6,$UHC.$Base.$_5b_5d]);
             $__swJSW126__0=
              new _A_($UHC.$Base.$concatMap,[$UHC.$Base.$_24okUNQ8027,$__7]);
             break;}
          return $__swJSW126__0;});
$UHC.$Base.$_24okUNQ8027=
 new _F_(function($_24x)
         {var $__=
           _e_($_24x);
          var $__swJSW127__0;
          switch($__._tag_)
           {case 0:
             var $__5=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,92,$__._1]));
             var $__swJSW128__0;
             switch($__5._tag_)
              {case 0:
                $__swJSW128__0=
                 $UHC.$Base.$_5b_5d;
                break;
               case 1:
                var $__6=
                 new _A_($UHC.$Base.$packedStringToString,[""]);
                var $__7=
                 [$__6,$__._2];
                var $__8=
                 new _A_($UHC.$Base.$_3a,[$__7,$UHC.$Base.$_5b_5d]);
                $__swJSW128__0=
                 $__8;
                break;}
             $__swJSW127__0=
              $__swJSW128__0;
             break;
            case 1:
             $__swJSW127__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW127__0;});
$UHC.$Base.$lexStringUNQ8005=
 new _F_(function($x1)
         {var $__=
           new _A_($UHC.$Base.$__76__33928__0NEW5030UNQ8045CCN,[$x1]);
          var $__3=
           _e_($x1);
          var $__swJSW129__0;
          switch($__3._tag_)
           {case 0:
             var $__6=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__56__0,34,$__3._1]));
             var $__swJSW130__0;
             switch($__6._tag_)
              {case 0:
                $__swJSW130__0=
                 $__;
                break;
               case 1:
                var $__7=
                 new _A_($UHC.$Base.$packedStringToString,["\""]);
                var $__8=
                 [$__7,$__3._2];
                var $__9=
                 new _A_($UHC.$Base.$_3a,[$__8,$UHC.$Base.$_5b_5d]);
                $__swJSW130__0=
                 $__9;
                break;}
             $__swJSW129__0=
              $__swJSW130__0;
             break;
            case 1:
             $__swJSW129__0=
              $__;
             break;}
          return $__swJSW129__0;});
$UHC.$Base.$__76__33928__0NEW5030UNQ8045CCN=
 new _F_(function($x1)
         {var $__=
           new _A_($UHC.$Base.$lexStrItemUNQ8003,[$x1]);
          return new _A_($UHC.$Base.$concatMap,[$UHC.$Base.$_24okUNQ8047,$__]);});
$UHC.$Base.$_24okUNQ8047=
 new _F_(function($_24x)
         {var $__=
           _e_($_24x);
          var $__5=
           new _A_($UHC.$Base.$lexStringUNQ8005,[$__[1]]);
          var $__6=
           new _A_($UHC.$Base.$_24okUNQ8064,[$__[0]]);
          return new _A_($UHC.$Base.$concatMap,[$__6,$__5]);});
$UHC.$Base.$_24okUNQ8064=
 new _F_(function($ch,$_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           new _A_($UHC.$Base.$_2b_2b,[$ch,$__[0]]);
          var $__7=
           [$__6,$__[1]];
          var $__8=
           new _A_($UHC.$Base.$_3a,[$__7,$UHC.$Base.$_5b_5d]);
          return $__8;});
$UHC.$Base.$lex=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW133__0;
          switch($__._tag_)
           {case 0:
             var $c5=
              new _A_($UHC.$Base.$cNEW4871UNQ7768CCN,[$__._2,$__._1]);
             var $__6=
              new _A_($UHC.$Base.$isSpace,[$__._1]);
             var $__7=
              _e_($__6);
             var $__swJSW134__0;
             switch($__7._tag_)
              {case 0:
                $__swJSW134__0=
                 $c5;
                break;
               case 1:
                var $__8=
                 new _A_($UHC.$Base.$dropWhile,[$UHC.$Base.$isSpace,$__._2]);
                var $__9=
                 new _A_($UHC.$Base.$lex,[$__8]);
                $__swJSW134__0=
                 $__9;
                break;}
             $__swJSW133__0=
              $__swJSW134__0;
             break;
            case 1:
             var $__10=
              new _A_($UHC.$Base.$packedStringToString,[""]);
             var $__11=
              new _A_($UHC.$Base.$packedStringToString,[""]);
             var $__12=
              [$__11,$__10];
             var $__13=
              new _A_($UHC.$Base.$_3a,[$__12,$UHC.$Base.$_5b_5d]);
             $__swJSW133__0=
              $__13;
             break;}
          return $__swJSW133__0;});
$UHC.$Base.$optionalUNQ8106=
 new _F_(function($g,$r)
         {var $__=
           new _A_($UHC.$Base.$mandatoryUNQ8107,[$g,$r]);
          var $__4=
           new _A_($g,[$r]);
          return new _A_($UHC.$Base.$_2b_2b,[$__4,$__]);});
$UHC.$Base.$mandatoryUNQ8107=
 new _F_(function($g,$r)
         {var $__=
           new _A_($UHC.$Base.$lex,[$r]);
          var $__4=
           new _A_($UHC.$Base.$_24okUNQ8111,[$g]);
          return new _A_($UHC.$Base.$concatMap,[$__4,$__]);});
$UHC.$Base.$readParen=
 new _F_(function($b,$g)
         {var $__=
           _e_($b);
          var $__swJSW135__0;
          switch($__._tag_)
           {case 0:
             var $__4=
              new _A_($UHC.$Base.$optionalUNQ8106,[$g]);
             $__swJSW135__0=
              $__4;
             break;
            case 1:
             var $__5=
              new _A_($UHC.$Base.$mandatoryUNQ8107,[$g]);
             $__swJSW135__0=
              $__5;
             break;}
          return $__swJSW135__0;});
$UHC.$Base.$Read__NEW5112CLS74__41__0DFLUHC_2eBase_2ereadList=
 new _F_(function($Read__)
         {var $__=
           new _A_($UHC.$Base.$__78__10227__0,[$Read__]);
          return new _A_($UHC.$Base.$readParen,[$UHC.$Base.$False__,$__]);});
$UHC.$Base.$Read__CLS74__41__0=
 new _F_(function($Read__)
         {var $Read__CLS74__41__0DFLUHC_2eBase_2ereadList=
           new _A_($UHC.$Base.$Read__NEW5112CLS74__41__0DFLUHC_2eBase_2ereadList,[$Read__]);
          var $Read__2=
           {_tag_:0,_1:$Read__CLS74__41__0DFLUHC_2eBase_2ereadList,_2:$UHC.$Base.$undefined};
          return $Read__2;});
$UHC.$Base.$Read__NEW6116UNQ11601EVLDCT74__127__0RDC=
 new _F_(function($Read__)
         {var $Read__2=
           _e_(new _A_($UHC.$Base.$Read__CLS74__41__0,[$Read__]));
          var $__5=
           {_tag_:0,_1:$Read__2._1,_2:$UHC.$Base.$Read__DCT74__127__0DFLUHC_2eBase_2ereadsPrec};
          return $__5;});
$UHC.$Base.$Read__NEW6114UNQ11598DCT74__127__0RDC=
 new _F_(function($Read__)
         {var $Read__2=
           new _A_($UHC.$Base.$Read__NEW6116UNQ11601EVLDCT74__127__0RDC,[$Read__]);
          return $Read__2;});
$UHC.$Base.$Read__UNQ11598DCT74__127__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Read__NEW6114UNQ11598DCT74__127__0RDC,[$UHC.$Base.$Read__UNQ11598DCT74__127__0RDC]);}),[]);
$UHC.$Base.$Read__DCT74__127__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Read__UNQ11598DCT74__127__0RDC;}),[]);
$Graphics.$UI.$WXCore.$WebWindow.$__199__73=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$read,[$UHC.$Base.$Read__DCT74__127__0]);}),[]);
$Graphics.$UI.$WXCore.$WebWindow.$parseInt=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$WebWindow.$__199__73,$Graphics.$UI.$WXCore.$WebWindow.$__199__74]);}),[]);
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ368=
 new _F_(function($_24x)
         {var $__=
           new _A_($Language.$UHC.$JS.$Marshal.$fromJS,[$Language.$UHC.$JS.$Marshal.$FromJS__DCT81__15__0,$_24x]);
          var $__3=
           new _A_($UHC.$Base.$_24,[$Data.$Maybe.$fromJust,$__]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$parseInt,[$__3]);
          var $__5=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
          return new _A_($UHC.$Base.$_24,[$__5,$__4]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ364=
 new _F_(function($p,$_24x)
         {var $__=
           new _A_($Language.$UHC.$JS.$Marshal.$str,[$p]);
          var $__4=
           new _A_($Language.$UHC.$JS.$HTML5.$CSSStyleDeclaration.$getPropertyValue,[$_24x,$__]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ368]);});
$Graphics.$UI.$WXCore.$WebWindow.$gettUNQ307=
 new _F_(function($p,$cssRef)
         {var $__=
           new _A_($Language.$UHC.$JS.$JSRef.$readJSRef,[$cssRef]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ364,[$p]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Language.$UHC.$JS.$JSRef.$Lens__=
 new _F_(function($x1,$x2)
         {return {_tag_:0,_1:$x1,_2:$x2};});
$Language.$UHC.$JS.$JSRef.$newJSRef=
 new _A_(new _F_(function()
                 {return $Language.$UHC.$JS.$JSRef.$Lens__;}),[]);
$Graphics.$UI.$WXCore.$WebWindow.$__199__393__0=
 new _F_(function($_24x,$v)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["id"]);
          return new _A_($Language.$UHC.$JS.$Prelude.$setAttr__,[$__,$v,$_24x]);});
$Graphics.$UI.$WXCore.$WebWindow.$__197__4045__0UNQ696=
 new _F_(function($heightProp,$widthProp,$rect)
         {var $size=
           new _A_($Graphics.$UI.$WXCore.$Types.$rectSize,[$UHC.$Base.$Num__DCT74__101__0,$rect]);
          var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
          var $__6=
           new _A_($Graphics.$UI.$WXCore.$Types.$sizeH,[$size]);
          var $__7=
           new _A_($Language.$UHC.$JS.$JSRef.$writeJSRef,[$heightProp,$__6]);
          var $__8=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__7,$__]);
          var $__9=
           new _A_($Graphics.$UI.$WXCore.$Types.$sizeW,[$size]);
          var $__10=
           new _A_($Language.$UHC.$JS.$JSRef.$writeJSRef,[$widthProp,$__9]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__10,$__8]);});
$Graphics.$UI.$WXCore.$GraphicsContextClass.$__graphicsContextClear=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__graphicsContextClear;});
$Graphics.$UI.$WXCore.$GraphicsContextClass.$graphicsContextClear=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$GraphicsContextClass.$__graphicsContextClear,$Graphics.$UI.$WXCore.$GraphicsContextClass.$graphicsContext__Methods]);}),[]);
$Graphics.$UI.$WXCore.$PaintEventClass.$__67__27=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["PaintEvent"]);}),[]);
$Graphics.$UI.$WXCore.$PaintEventClass.$paintEventTc=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Typeable.$mkTyCon,[$Graphics.$UI.$WXCore.$PaintEventClass.$__67__27]);}),[]);
$Graphics.$UI.$WXCore.$PaintEventClass.$Typeable1__DCT63__8__0DFLData_2eTypeable_2etypeOf1=
 new _F_(function($__)
         {return new _A_($Data.$Typeable.$mkTyConApp,[$Graphics.$UI.$WXCore.$PaintEventClass.$paintEventTc,$UHC.$Base.$_5b_5d]);});
$Graphics.$UI.$WXCore.$PaintEventClass.$Typeable1__NEW20UNQ114EVLDCT63__8__0RDC=
 new _F_(function($Typeable1__)
         {var $Typeable1__2=
           _e_(new _A_($Data.$Typeable.$Typeable1__CLS320__8__0,[$Typeable1__]));
          var $__4=
           {_tag_:0,_1:$Graphics.$UI.$WXCore.$PaintEventClass.$Typeable1__DCT63__8__0DFLData_2eTypeable_2etypeOf1};
          return $__4;});
$Graphics.$UI.$WXCore.$PaintEventClass.$Typeable1__NEW18UNQ113DCT63__8__0RDC=
 new _F_(function($Typeable1__)
         {var $Typeable1__2=
           new _A_($Graphics.$UI.$WXCore.$PaintEventClass.$Typeable1__NEW20UNQ114EVLDCT63__8__0RDC,[$Typeable1__]);
          return $Typeable1__2;});
$Graphics.$UI.$WXCore.$PaintEventClass.$Typeable1__UNQ113DCT63__8__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$PaintEventClass.$Typeable1__NEW18UNQ113DCT63__8__0RDC,[$Graphics.$UI.$WXCore.$PaintEventClass.$Typeable1__UNQ113DCT63__8__0RDC]);}),[]);
$Graphics.$UI.$WXCore.$PaintEventClass.$Typeable1__DCT63__8__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$PaintEventClass.$Typeable1__UNQ113DCT63__8__0RDC;}),[]);
$Graphics.$UI.$WXCore.$PaintEventClass.$Typeable__NEW32UNQ98EVLDCT63__9__0RDC=
 new _F_(function($Typeable__,$Typeable__2)
         {var $Typeable__3=
           _e_(new _A_($Data.$Typeable.$Typeable__CLS320__7__0,[$Typeable__]));
          var $__5=
           {_tag_:0,_1:$Typeable__2};
          return $__5;});
$Graphics.$UI.$WXCore.$PaintEventClass.$Typeable__NEW29UNQ95DCT63__9__0RDC=
 new _F_(function($Typeable__,$Typeable__2)
         {var $Typeable__3=
           new _A_($Graphics.$UI.$WXCore.$PaintEventClass.$Typeable__NEW32UNQ98EVLDCT63__9__0RDC,[$Typeable__,$Typeable__2]);
          return $Typeable__3;});
$Graphics.$UI.$WXCore.$PaintEventClass.$Typeable__DCT63__9__0=
 new _F_(function($__)
         {var $Typeable__DCT63__9__0DFLData_2eTypeable_2etypeOf=
           new _A_($Data.$Typeable.$typeOfDefault,[$Graphics.$UI.$WXCore.$PaintEventClass.$Typeable1__DCT63__8__0,$__]);
          var $Typeable__=
           _i_();
          _i_set_($Typeable__,new _A_($Graphics.$UI.$WXCore.$PaintEventClass.$Typeable__NEW29UNQ95DCT63__9__0RDC,[$Typeable__,$Typeable__DCT63__9__0DFLData_2eTypeable_2etypeOf]));
          return $Typeable__;});
$Graphics.$UI.$WXCore.$PaintEventClass.$__65__394__1__0UNQ82=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$PaintEventClass.$Typeable__DCT63__9__0,[$Data.$Typeable.$Typeable__DCT320__26__0]);}),[]);
$Graphics.$UI.$WXCore.$PaintEventClass.$__67__63=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$hideRecord,[$Graphics.$UI.$WXCore.$PaintEventClass.$__65__394__1__0UNQ82]);}),[]);
$Graphics.$UI.$WXCore.$PaintEventClass.$Narrow__DCT63__0__0DFLLightOO_2eCore_2enarrow=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$EventClass.$modify__Event__Tail,[$Graphics.$UI.$WXCore.$PaintEventClass.$__67__63]);}),[]);
$Graphics.$UI.$WXCore.$PaintEventClass.$Narrow__NEW44UNQ84EVLDCT63__0__0RDC=
 new _F_(function($Narrow__,$Narrow__2)
         {var $Narrow__3=
           _e_(new _A_($LightOO.$Core.$Narrow__CLS69__5__0,[$Narrow__2]));
          var $__5=
           {_tag_:0,_1:$Narrow__};
          return $__5;});
$Graphics.$UI.$WXCore.$PaintEventClass.$Narrow__NEW41UNQ81DCT63__0__0RDC=
 new _F_(function($Narrow__,$Narrow__2)
         {var $Narrow__3=
           new _A_($Graphics.$UI.$WXCore.$PaintEventClass.$Narrow__NEW44UNQ84EVLDCT63__0__0RDC,[$Narrow__,$Narrow__2]);
          return $Narrow__3;});
$Graphics.$UI.$WXCore.$PaintEventClass.$Narrow__UNQ81DCT63__0__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$PaintEventClass.$Narrow__NEW41UNQ81DCT63__0__0RDC,[$Graphics.$UI.$WXCore.$PaintEventClass.$Narrow__DCT63__0__0DFLLightOO_2eCore_2enarrow,$Graphics.$UI.$WXCore.$PaintEventClass.$Narrow__UNQ81DCT63__0__0RDC]);}),[]);
$Graphics.$UI.$WXCore.$PaintEventClass.$Narrow__DCT63__0__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$PaintEventClass.$Narrow__UNQ81DCT63__0__0RDC;}),[]);
$Graphics.$UI.$WXCore.$PaintEvent.$paintEvent_27UNQ4=
 new _F_(function($tail,$super,$__,$self)
         {var $__5=
           {_tag_:0,__paintEventTail:$tail};
          return new _A_($UHC.$Base.$return,[$__,$__5]);});
$Graphics.$UI.$WXCore.$PaintEvent.$__115__19__0=
 new _F_(function($__,$__2)
         {return new _A_($Graphics.$UI.$WXCore.$PaintEvent.$paintEvent_27UNQ4,[$__,$__2,$UHC.$Base.$Monad__DCT74__339__0]);});
$Graphics.$UI.$WXCore.$PaintEvent.$paintEvent=
 new _F_(function($id)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Event.$event,[$id,$Graphics.$UI.$WXCore.$Types.$wxEVT__PAINT]);
          return new _A_($LightOO.$Core.$extends,[$Graphics.$UI.$WXCore.$PaintEvent.$__115__19__0,$__,$LightOO.$Core.$noOverride,$Graphics.$UI.$WXCore.$EventClass.$set__Event__Tail]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ531=
 new _F_(function($self,$__,$_24x)
         {var $__4=
           new _A_($LightOO.$Core.$Sub__DCT69__17__0,[$LightOO.$Core.$Sub__DCT69__9__0,$Graphics.$UI.$WXCore.$EvtHandlerClass.$Narrow__DCT153__0__0]);
          var $__5=
           new _A_($LightOO.$Core.$Sub__DCT69__24__0,[$__4,$Graphics.$UI.$WXCore.$WindowClass.$Narrow__DCT159__0__0]);
          var $this=
           new _A_($LightOO.$Core.$upcast,[$__5,$self]);
          var $__7=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
          var $__8=
           new _A_($LightOO.$Core.$upcast,[$__,$_24x]);
          var $__9=
           new _A_($LightOO.$Core.$_23,[$self,$Graphics.$UI.$WXCore.$EvtHandlerClass.$evtHandlerProcessEvent]);
          var $__10=
           new _A_($UHC.$Base.$_24,[$__9,$__8]);
          var $__11=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__10,$__7]);
          var $__12=
           new _A_($LightOO.$Core.$_23,[$_24x,$Graphics.$UI.$WXCore.$EventClass.$eventSetEventObject]);
          var $__13=
           new _A_($UHC.$Base.$_24,[$__12,$this]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__13,$__11]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ523=
 new _F_(function($self,$__,$_24x)
         {var $__4=
           new _A_($Graphics.$UI.$WXCore.$PaintEvent.$paintEvent,[$_24x]);
          var $__5=
           new _A_($LightOO.$Core.$new,[$Control.$Monad.$Fix.$MonadFix__DCT97__4__0]);
          var $__6=
           new _A_($UHC.$Base.$_24,[$__5,$__4]);
          var $__7=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ531,[$self,$__]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__7]);});
$Graphics.$UI.$WXCore.$WebWindow.$__199__698NEW306=
 new _F_(function($self)
         {var $__=
           new _A_($LightOO.$Core.$Sub__DCT69__24__0,[$LightOO.$Core.$Sub__DCT69__13__0,$Graphics.$UI.$WXCore.$PaintEventClass.$Narrow__DCT63__0__0]);
          var $__3=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ523,[$self,$__]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Graphics.$UI.$WXCore.$Types.$idCreate,$__3]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ513=
 new _F_(function($self,$_24x,$_24x3)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__698NEW306,[$self]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$GraphicsContextClass.$graphicsContextClear,[$_24x,$_24x3]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__5,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ506=
 new _F_(function($self,$_24x)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$self,$Graphics.$UI.$WXCore.$WindowClass.$windowGetRect]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ513,[$self,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Graphics.$UI.$WXCore.$WebWindow.$__197__4019__0UNQ502=
 new _F_(function($self,$repaintBg)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$self,$Graphics.$UI.$WXCore.$WindowClass.$windowGetGraphicsContext]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ506,[$self]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Language.$UHC.$JS.$JSRef.$writeJSRef=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2._2;});
$Graphics.$UI.$WXCore.$KeyEventClass.$Typeable__NEW124UNQ218EVLDCT93__9__0RDC=
 new _F_(function($Typeable__,$Typeable__2)
         {var $Typeable__3=
           _e_(new _A_($Data.$Typeable.$Typeable__CLS320__7__0,[$Typeable__2]));
          var $__5=
           {_tag_:0,_1:$Typeable__};
          return $__5;});
$Graphics.$UI.$WXCore.$KeyEventClass.$Typeable__NEW121UNQ215DCT93__9__0RDC=
 new _F_(function($Typeable__,$Typeable__2)
         {var $Typeable__3=
           new _A_($Graphics.$UI.$WXCore.$KeyEventClass.$Typeable__NEW124UNQ218EVLDCT93__9__0RDC,[$Typeable__,$Typeable__2]);
          return $Typeable__3;});
$Graphics.$UI.$WXCore.$KeyEventClass.$__97__58=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["KeyEvent"]);}),[]);
$Graphics.$UI.$WXCore.$KeyEventClass.$keyEventTc=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Typeable.$mkTyCon,[$Graphics.$UI.$WXCore.$KeyEventClass.$__97__58]);}),[]);
$Graphics.$UI.$WXCore.$KeyEventClass.$Typeable1__DCT93__8__0DFLData_2eTypeable_2etypeOf1=
 new _F_(function($__)
         {return new _A_($Data.$Typeable.$mkTyConApp,[$Graphics.$UI.$WXCore.$KeyEventClass.$keyEventTc,$UHC.$Base.$_5b_5d]);});
$Graphics.$UI.$WXCore.$KeyEventClass.$Typeable1__NEW115UNQ251EVLDCT93__8__0RDC=
 new _F_(function($Typeable1__)
         {var $Typeable1__2=
           _e_(new _A_($Data.$Typeable.$Typeable1__CLS320__8__0,[$Typeable1__]));
          var $__4=
           {_tag_:0,_1:$Graphics.$UI.$WXCore.$KeyEventClass.$Typeable1__DCT93__8__0DFLData_2eTypeable_2etypeOf1};
          return $__4;});
$Graphics.$UI.$WXCore.$KeyEventClass.$Typeable1__NEW113UNQ250DCT93__8__0RDC=
 new _F_(function($Typeable1__)
         {var $Typeable1__2=
           new _A_($Graphics.$UI.$WXCore.$KeyEventClass.$Typeable1__NEW115UNQ251EVLDCT93__8__0RDC,[$Typeable1__]);
          return $Typeable1__2;});
$Graphics.$UI.$WXCore.$KeyEventClass.$Typeable1__UNQ250DCT93__8__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$KeyEventClass.$Typeable1__NEW113UNQ250DCT93__8__0RDC,[$Graphics.$UI.$WXCore.$KeyEventClass.$Typeable1__UNQ250DCT93__8__0RDC]);}),[]);
$Graphics.$UI.$WXCore.$KeyEventClass.$Typeable1__DCT93__8__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$KeyEventClass.$Typeable1__UNQ250DCT93__8__0RDC;}),[]);
$Graphics.$UI.$WXCore.$KeyEventClass.$Typeable__DCT93__9__0=
 new _F_(function($__)
         {var $Typeable__DCT93__9__0DFLData_2eTypeable_2etypeOf=
           new _A_($Data.$Typeable.$typeOfDefault,[$Graphics.$UI.$WXCore.$KeyEventClass.$Typeable1__DCT93__8__0,$__]);
          var $Typeable__=
           _i_();
          _i_set_($Typeable__,new _A_($Graphics.$UI.$WXCore.$KeyEventClass.$Typeable__NEW121UNQ215DCT93__9__0RDC,[$Typeable__DCT93__9__0DFLData_2eTypeable_2etypeOf,$Typeable__]));
          return $Typeable__;});
$Graphics.$UI.$WXCore.$KeyEventClass.$__95__657__1__0UNQ238=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$KeyEventClass.$Typeable__DCT93__9__0,[$Data.$Typeable.$Typeable__DCT320__26__0]);}),[]);
$Graphics.$UI.$WXCore.$KeyEventClass.$__97__251=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$hideRecord,[$Graphics.$UI.$WXCore.$KeyEventClass.$__95__657__1__0UNQ238]);}),[]);
$Graphics.$UI.$WXCore.$KeyEventClass.$Narrow__DCT93__0__0DFLLightOO_2eCore_2enarrow=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$EventClass.$modify__Event__Tail,[$Graphics.$UI.$WXCore.$KeyEventClass.$__97__251]);}),[]);
$Graphics.$UI.$WXCore.$KeyEventClass.$Narrow__NEW136UNQ240EVLDCT93__0__0RDC=
 new _F_(function($Narrow__,$Narrow__2)
         {var $Narrow__3=
           _e_(new _A_($LightOO.$Core.$Narrow__CLS69__5__0,[$Narrow__]));
          var $__5=
           {_tag_:0,_1:$Narrow__2};
          return $__5;});
$Graphics.$UI.$WXCore.$KeyEventClass.$Narrow__NEW133UNQ237DCT93__0__0RDC=
 new _F_(function($Narrow__,$Narrow__2)
         {var $Narrow__3=
           new _A_($Graphics.$UI.$WXCore.$KeyEventClass.$Narrow__NEW136UNQ240EVLDCT93__0__0RDC,[$Narrow__,$Narrow__2]);
          return $Narrow__3;});
$Graphics.$UI.$WXCore.$KeyEventClass.$Narrow__UNQ237DCT93__0__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$KeyEventClass.$Narrow__NEW133UNQ237DCT93__0__0RDC,[$Graphics.$UI.$WXCore.$KeyEventClass.$Narrow__UNQ237DCT93__0__0RDC,$Graphics.$UI.$WXCore.$KeyEventClass.$Narrow__DCT93__0__0DFLLightOO_2eCore_2enarrow]);}),[]);
$Graphics.$UI.$WXCore.$KeyEventClass.$Narrow__DCT93__0__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$KeyEventClass.$Narrow__UNQ237DCT93__0__0RDC;}),[]);
$Graphics.$UI.$WXCore.$KeyEvent.$__121__22NEW2=
 new _F_(function($x,$y,$keyCode,$unicode,$tail)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$unicode]);
          var $__7=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$y]);
          var $__8=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$x]);
          var $__9=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$keyCode]);
          return {_tag_:0,__keyEventGetKeyCode:$__9,__keyEventGetX:$__8,__keyEventGetY:$__7,__keyEventGetUnicodeKey:$__,__keyEventTail:$tail};});
$Graphics.$UI.$WXCore.$KeyEvent.$keyEvent_27UNQ12=
 new _F_(function($x,$y,$keyCode,$unicode,$tail,$super,$__,$self)
         {var $__9=
           new _A_($Graphics.$UI.$WXCore.$KeyEvent.$__121__22NEW2,[$x,$y,$keyCode,$unicode,$tail]);
          return new _A_($UHC.$Base.$return,[$__,$__9]);});
$Graphics.$UI.$WXCore.$KeyEvent.$__121__43__0=
 new _F_(function($x,$y,$keyCode,$unicode,$__,$__6)
         {return new _A_($Graphics.$UI.$WXCore.$KeyEvent.$keyEvent_27UNQ12,[$x,$y,$keyCode,$unicode,$__,$__6,$UHC.$Base.$Monad__DCT74__339__0]);});
$Graphics.$UI.$WXCore.$KeyEvent.$keyEvent=
 new _F_(function($id,$x,$y,$keyCode,$unicode)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Event.$event,[$id,$Graphics.$UI.$WXCore.$Types.$wxEVT__CHAR]);
          var $__7=
           new _A_($Graphics.$UI.$WXCore.$KeyEvent.$__121__43__0,[$x,$y,$keyCode,$unicode]);
          return new _A_($LightOO.$Core.$extends,[$__7,$__,$LightOO.$Core.$noOverride,$Graphics.$UI.$WXCore.$EventClass.$set__Event__Tail]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ428=
 new _F_(function($self,$__,$_24x)
         {var $__4=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
          var $__5=
           new _A_($LightOO.$Core.$upcast,[$__,$_24x]);
          var $__6=
           new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$evtHandlerProcessEvent,[$self,$__5]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__4]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ421=
 new _F_(function($self,$__,$_24x,$_24x4)
         {var $__5=
           new _A_($UHC.$Base.$packedStringToString,[""]);
          var $__6=
           new _A_($Graphics.$UI.$WXCore.$KeyEvent.$keyEvent,[$_24x,0,0,$_24x4,$__5]);
          var $__7=
           new _A_($LightOO.$Core.$new,[$Control.$Monad.$Fix.$MonadFix__DCT97__4__0]);
          var $__8=
           new _A_($UHC.$Base.$_24,[$__7,$__6]);
          var $__9=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ428,[$self,$__]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__8,$__9]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ409=
 new _F_(function($self,$e,$_24x)
         {var $__=
           new _A_($LightOO.$Core.$Sub__DCT69__24__0,[$LightOO.$Core.$Sub__DCT69__13__0,$Graphics.$UI.$WXCore.$KeyEventClass.$Narrow__DCT93__0__0]);
          var $__5=
           new _A_($UHC.$Base.$packedStringToString,["keyCode"]);
          var $__6=
           new _A_($Language.$UHC.$JS.$Prelude.$getAttr,[$__5,$e]);
          var $__7=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ421,[$self,$__,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__7]);});
$Graphics.$UI.$WXCore.$WebWindow.$__199__467NEW188=
 new _F_(function($self,$e)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ409,[$self,$e]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Graphics.$UI.$WXCore.$Types.$idCreate,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$cbUNQ405=
 new _F_(function($self,$e)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__467NEW188,[$self,$e]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToString,["cb"]);
          var $__5=
           new _A_($Language.$UHC.$JS.$Marshal.$toJS,[$Language.$UHC.$JS.$Marshal.$ToJS__DCT81__13__0,$__4]);
          var $__6=
           new _A_($Language.$UHC.$JS.$Prelude.$__trace,[$__5]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$attachHandlerNEW184UNQ392=
 new _F_(function($self,$onkeydownProp)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["write cb"]);
          var $__4=
           new _A_($Language.$UHC.$JS.$Marshal.$toJS,[$Language.$UHC.$JS.$Marshal.$ToJS__DCT81__13__0,$__]);
          var $__5=
           new _A_($Language.$UHC.$JS.$Prelude.$__trace,[$__4]);
          var $__6=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$cbUNQ405,[$self]);
          var $__7=
           new _A_($Language.$UHC.$JS.$JSRef.$writeJSRef,[$onkeydownProp,$__6]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__7,$__5]);});
$Graphics.$UI.$WXCore.$Events.$EventKey__=
 new _F_(function($x1,$x2,$x3)
         {return {_tag_:0,_1:$x1,_2:$x2,_3:$x3};});
$UHC.$Base.$primIntToChar=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primUnsafeId($__2);});
$UHC.$Base.$primCharToInt=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primUnsafeId($__2);});
$UHC.$Base.$Enum__NEW4716UNQ10449EVLDCT74__60__0RDC=
 new _F_(function($Enum__)
         {var $Enum__2=
           _e_(new _A_($UHC.$Base.$Enum__CLS74__38__0,[$Enum__]));
          var $__11=
           {_tag_:0,_1:$Enum__2._1,_2:$Enum__2._2,_3:$Enum__2._3,_4:$Enum__2._4,_5:$UHC.$Base.$primCharToInt,_6:$Enum__2._6,_7:$Enum__2._7,_8:$UHC.$Base.$primIntToChar};
          return $__11;});
$UHC.$Base.$Enum__NEW4714UNQ10448DCT74__60__0RDC=
 new _F_(function($Enum__)
         {var $Enum__2=
           new _A_($UHC.$Base.$Enum__NEW4716UNQ10449EVLDCT74__60__0RDC,[$Enum__]);
          return $Enum__2;});
$UHC.$Base.$Enum__UNQ10448DCT74__60__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Enum__NEW4714UNQ10448DCT74__60__0RDC,[$UHC.$Base.$Enum__UNQ10448DCT74__60__0RDC]);}),[]);
$UHC.$Base.$Enum__DCT74__60__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Enum__UNQ10448DCT74__60__0RDC;}),[]);
$Graphics.$UI.$WXCore.$Events.$wxK__RIGHT=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$packedStringToInteger,["39"]);
          return new _A_($UHC.$Base.$fromInteger,[$__,$__2]);});
$Graphics.$UI.$WXCore.$Events.$__103__1381=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$Events.$wxK__RIGHT,[$UHC.$Base.$Num__DCT74__101__0]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__103__1379=
 new _A_(new _F_(function()
                 {return [$Graphics.$UI.$WXCore.$Events.$__103__1381,$Graphics.$UI.$WXCore.$Events.$KeyRight__];}),[]);
$Graphics.$UI.$WXCore.$Events.$__103__1377=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$Graphics.$UI.$WXCore.$Events.$__103__1379,$UHC.$Base.$_5b_5d]);}),[]);
$Graphics.$UI.$WXCore.$Events.$wxK__LEFT=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$packedStringToInteger,["37"]);
          return new _A_($UHC.$Base.$fromInteger,[$__,$__2]);});
$Graphics.$UI.$WXCore.$Events.$__103__1376=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$Events.$wxK__LEFT,[$UHC.$Base.$Num__DCT74__101__0]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__103__1374=
 new _A_(new _F_(function()
                 {return [$Graphics.$UI.$WXCore.$Events.$__103__1376,$Graphics.$UI.$WXCore.$Events.$KeyLeft__];}),[]);
$Graphics.$UI.$WXCore.$Events.$keyCodeMap=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a,[$Graphics.$UI.$WXCore.$Events.$__103__1374,$Graphics.$UI.$WXCore.$Events.$__103__1377]);}),[]);
$UHC.$Base.$kNEW1683UNQ3049CCN=
 new _F_(function($__,$x1,$x2)
         {var $x24=
           _e_($x2);
          var $__swJSW146__0;
          switch($x24._tag_)
           {case 0:
             var $__7=
              _e_($x24._1);
             var $__10=
              new _A_($UHC.$Base.$_3d_3d,[$__,$x1,$__7[0]]);
             var $__11=
              _e_($__10);
             var $__swJSW148__0;
             switch($__11._tag_)
              {case 0:
                var $__12=
                 _e_($UHC.$Base.$otherwise);
                var $__swJSW149__0;
                switch($__12._tag_)
                 {case 0:
                   $__swJSW149__0=
                    $UHC.$Base.$undefined;
                   break;
                  case 1:
                   var $__13=
                    new _A_($UHC.$Base.$lookup,[$__,$x1,$x24._2]);
                   $__swJSW149__0=
                    $__13;
                   break;}
                $__swJSW148__0=
                 $__swJSW149__0;
                break;
               case 1:
                var $__14=
                 new _A_($UHC.$Base.$Just__,[$__7[1]]);
                $__swJSW148__0=
                 $__14;
                break;}
             $__swJSW146__0=
              $__swJSW148__0;
             break;
            case 1:
             $__swJSW146__0=
              $UHC.$Base.$undefined;
             break;}
          return $__swJSW146__0;});
$UHC.$Base.$lookup=
 new _F_(function($__,$x1,$x2)
         {var $k=
           new _A_($UHC.$Base.$kNEW1683UNQ3049CCN,[$__,$x1,$x2]);
          var $x25=
           _e_($x2);
          var $__swJSW150__0;
          switch($x25._tag_)
           {case 0:
             $__swJSW150__0=
              $k;
             break;
            case 1:
             $__swJSW150__0=
              $UHC.$Base.$Nothing__;
             break;}
          return $__swJSW150__0;});
$Graphics.$UI.$WXCore.$Events.$keyCodeToKey=
 new _F_(function($keyCode)
         {var $__=
           new _A_($UHC.$Base.$lookup,[$UHC.$Base.$Eq__DCT74__88__0,$keyCode,$Graphics.$UI.$WXCore.$Events.$keyCodeMap]);
          var $__3=
           _e_($__);
          var $__swJSW151__0;
          switch($__3._tag_)
           {case 0:
             $__swJSW151__0=
              $__3._1;
             break;
            case 1:
             var $__5=
              new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__91__0,$keyCode,255]);
             var $__6=
              _e_($__5);
             var $__swJSW152__0;
             switch($__6._tag_)
              {case 0:
                var $__7=
                 _e_($UHC.$Base.$otherwise);
                var $__swJSW153__0;
                switch($__7._tag_)
                 {case 0:
                   $__swJSW153__0=
                    $UHC.$Base.$undefined;
                   break;
                  case 1:
                   var $__8=
                    new _A_($Graphics.$UI.$WXCore.$Events.$KeyOther__,[$keyCode]);
                   $__swJSW153__0=
                    $__8;
                   break;}
                $__swJSW152__0=
                 $__swJSW153__0;
                break;
               case 1:
                var $__9=
                 new _A_($UHC.$Base.$toEnum,[$UHC.$Base.$Enum__DCT74__60__0,$keyCode]);
                var $__10=
                 new _A_($Graphics.$UI.$WXCore.$Events.$KeyChar__,[$__9]);
                $__swJSW152__0=
                 $__10;
                break;}
             $__swJSW151__0=
              $__swJSW152__0;
             break;}
          return $__swJSW151__0;});
$Graphics.$UI.$WXCore.$Events.$_24okUNQ1593=
 new _F_(function($point,$modifiers,$_24x)
         {var $key=
           new _A_($Graphics.$UI.$WXCore.$Events.$keyCodeToKey,[$_24x]);
          var $__=
           new _A_($Graphics.$UI.$WXCore.$Events.$EventKey__,[$key,$modifiers,$point]);
          return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__]);});
$Graphics.$UI.$WXCore.$KeyEventClass.$__keyEventGetKeyCode=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__keyEventGetKeyCode;});
$Graphics.$UI.$WXCore.$KeyEventClass.$keyEventGetKeyCode=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$KeyEventClass.$__keyEventGetKeyCode,$Graphics.$UI.$WXCore.$KeyEventClass.$keyEvent__methods]);}),[]);
$Graphics.$UI.$WXCore.$Events.$Modifiers__=
 new _F_(function($x1,$x2,$x3,$x4)
         {return {_tag_:0,altDown:$x1,shiftDown:$x2,controlDown:$x3,metaDown:$x4};});
$Graphics.$UI.$WXCore.$Events.$_24okUNQ1588=
 new _F_(function($event,$point,$_24x,$_24x4,$_24x5,$_24x6)
         {var $modifiers=
           new _A_($Graphics.$UI.$WXCore.$Events.$Modifiers__,[$_24x,$_24x5,$_24x4,$_24x6]);
          var $__=
           new _A_($Graphics.$UI.$WXCore.$KeyEventClass.$keyEventGetKeyCode,[$event]);
          var $__9=
           new _A_($Graphics.$UI.$WXCore.$Events.$_24okUNQ1593,[$point,$modifiers]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__9]);});
$Graphics.$UI.$WXCore.$Events.$_24okUNQ1580=
 new _F_(function($event,$point,$_24x,$_24x4,$_24x5)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$UHC.$Base.$False__]);
          var $__7=
           new _A_($Graphics.$UI.$WXCore.$Events.$_24okUNQ1588,[$event,$point,$_24x,$_24x4,$_24x5]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__7]);});
$Graphics.$UI.$WXCore.$Events.$_24okUNQ1573=
 new _F_(function($event,$point,$_24x,$_24x4)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$UHC.$Base.$False__]);
          var $__6=
           new _A_($Graphics.$UI.$WXCore.$Events.$_24okUNQ1580,[$event,$point,$_24x,$_24x4]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__6]);});
$Graphics.$UI.$WXCore.$Events.$_24okUNQ1566=
 new _F_(function($event,$point,$_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$UHC.$Base.$False__]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$Events.$_24okUNQ1573,[$event,$point,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__5]);});
$Graphics.$UI.$WXCore.$Events.$_24okUNQ1558=
 new _F_(function($event,$_24x,$_24x3,$_24x4)
         {var $point=
           new _A_($Graphics.$UI.$WXCore.$Types.$pt,[$UHC.$Base.$Num__DCT74__101__0,$_24x,$_24x3]);
          var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$UHC.$Base.$False__]);
          var $__7=
           new _A_($Graphics.$UI.$WXCore.$Events.$_24okUNQ1566,[$event,$point]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__7]);});
$Graphics.$UI.$WXCore.$Events.$_24okUNQ1550=
 new _F_(function($event,$_24x,$_24x3)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$EventClass.$eventGetEventObject,[$event]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$Events.$_24okUNQ1558,[$event,$_24x,$_24x3]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__5]);});
$Graphics.$UI.$WXCore.$KeyEventClass.$__keyEventGetY=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__keyEventGetY;});
$Graphics.$UI.$WXCore.$KeyEventClass.$keyEventGetY=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$KeyEventClass.$__keyEventGetY,$Graphics.$UI.$WXCore.$KeyEventClass.$keyEvent__methods]);}),[]);
$Graphics.$UI.$WXCore.$Events.$_24okUNQ1544=
 new _F_(function($event,$_24x)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$KeyEventClass.$keyEventGetY,[$event]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$Events.$_24okUNQ1550,[$event,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Graphics.$UI.$WXCore.$KeyEventClass.$__keyEventGetX=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__keyEventGetX;});
$Graphics.$UI.$WXCore.$KeyEventClass.$keyEvent__methods=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$LightOO.$Core.$unRecord,$Graphics.$UI.$WXCore.$EventClass.$get__Event__Tail]);}),[]);
$Graphics.$UI.$WXCore.$KeyEventClass.$keyEventGetX=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$KeyEventClass.$__keyEventGetX,$Graphics.$UI.$WXCore.$KeyEventClass.$keyEvent__methods]);}),[]);
$Graphics.$UI.$WXCore.$Events.$eventKeyFromEvent=
 new _F_(function($event)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$KeyEventClass.$keyEventGetX,[$event]);
          var $__3=
           new _A_($Graphics.$UI.$WXCore.$Events.$_24okUNQ1544,[$event]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__3]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ398=
 new _F_(function($handler,$_24x)
         {return new _A_($handler,[$_24x]);});
$Graphics.$UI.$WXCore.$WebWindow.$__199__447NEW170=
 new _F_(function($handler,$keyEvent)
         {var $__=
           new _A_($Data.$Maybe.$fromJust,[$keyEvent]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$Events.$eventKeyFromEvent,[$__]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ398,[$handler]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__5]);});
$Graphics.$UI.$WXCore.$WebWindow.$evtHandlerUNQ389=
 new _F_(function($handler,$__,$event)
         {var $keyEvent=
           new _A_($LightOO.$Core.$downcast,[$__,$event]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__447NEW170,[$handler,$keyEvent]);
          var $__6=
           new _A_($UHC.$Base.$packedStringToString,["not of type KeyEvent"]);
          var $__7=
           new _A_($UHC.$Base.$error,[$__6]);
          var $__8=
           new _A_($UHC.$Base.$primIntToInteger,[123]);
          var $__9=
           new _A_($Language.$UHC.$JS.$Prelude.$__trace,[$__8]);
          var $__10=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__9,$__7]);
          var $__11=
           new _A_($Data.$Maybe.$isJust,[$keyEvent]);
          var $__12=
           new _A_($UHC.$Base.$_24,[$UHC.$Base.$not,$__11]);
          var $__13=
           new _A_($Control.$Monad.$when,[$UHC.$Base.$Monad__DCT74__339__0,$__12,$__10]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__13,$__5]);});
$Graphics.$UI.$WXCore.$Types.$wxEVT__CHAR=
 new _A_(new _F_(function()
                 {return 3;}),[]);
$Graphics.$UI.$WXCore.$WebWindow.$__197__4016__0UNQ382=
 new _F_(function($self,$__,$onkeydownProp,$handler)
         {var $attachHandler=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$attachHandlerNEW184UNQ392,[$self,$onkeydownProp]);
          var $__6=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$evtHandlerUNQ389,[$handler,$__]);
          var $__7=
           new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$evtHandlerBind,[$self,$Graphics.$UI.$WXCore.$Types.$wxEVT__CHAR,$__6,$Graphics.$UI.$WXCore.$Types.$idAny,$Graphics.$UI.$WXCore.$Types.$idAny]);
          var $__8=
           new _A_($UHC.$Base.$packedStringToString,["onkeychar"]);
          var $__9=
           new _A_($Language.$UHC.$JS.$Marshal.$toJS,[$Language.$UHC.$JS.$Marshal.$ToJS__DCT81__13__0,$__8]);
          var $__10=
           new _A_($Language.$UHC.$JS.$Prelude.$__trace,[$__9]);
          var $__11=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__10,$__7]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$attachHandler,$__11]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ711=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Types.$rect,[$UHC.$Base.$Num__DCT74__101__0,$_24x,$_24x2]);
          return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ707=
 new _F_(function($self,$_24x)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$self,$Graphics.$UI.$WXCore.$WindowClass.$windowGetSize]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ711,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Graphics.$UI.$WXCore.$WebWindow.$__197__4047__0NEW442UNQ701=
 new _F_(function($self)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$self,$Graphics.$UI.$WXCore.$WindowClass.$windowGetPosition]);
          var $__3=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ707,[$self]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__3]);});
$Language.$UHC.$JS.$JSRef.$readJSRef=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2._1;});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ692=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Types.$sz,[$UHC.$Base.$Num__DCT74__101__0,$_24x,$_24x2]);
          var $__4=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
          return new _A_($UHC.$Base.$_24,[$__4,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ688=
 new _F_(function($heightProp,$_24x)
         {var $__=
           new _A_($Language.$UHC.$JS.$JSRef.$readJSRef,[$heightProp]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ692,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Graphics.$UI.$WXCore.$WebWindow.$__197__4044__0NEW424UNQ682=
 new _F_(function($heightProp,$widthProp)
         {var $__=
           new _A_($Language.$UHC.$JS.$JSRef.$readJSRef,[$widthProp]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ688,[$heightProp]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Graphics.$UI.$WXCore.$WebWindow.$__199__884__0=
 new _F_(function($r)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$UHC.$Base.$Nothing__]);
          var $__3=
           _e_($r);
          var $__swJSW158__0;
          switch($__3._tag_)
           {case 0:
             var $__5=
              _e_($__3._1);
             var $__9=
              _e_($__5[2]);
             var $__swJSW160__0;
             switch($__9._tag_)
              {case 0:
                var $__12=
                 new _A_($UHC.$Base.$Just__,[$__9._1]);
                var $__13=
                 new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__12]);
                $__swJSW160__0=
                 $__13;
                break;
               case 1:
                $__swJSW160__0=
                 $__;
                break;}
             $__swJSW158__0=
              $__swJSW160__0;
             break;
            case 1:
             var $__14=
              new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$UHC.$Base.$Nothing__]);
             $__swJSW158__0=
              $__14;
             break;}
          return $__swJSW158__0;});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ228=
 new _F_(function($f,$_24x)
         {return new _A_($f,[$_24x]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ224=
 new _F_(function($f,$_24x,$_24x3)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$partitionByWindowId,[$_24x3,$_24x]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ228,[$f]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__5]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ217=
 new _F_(function($self,$f,$_24x)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$self,$Graphics.$UI.$WXCore.$WindowClass.$windowGetId]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ224,[$f,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__5]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ207=
 new _F_(function($self,$f,$_24x)
         {var $__=
           _e_($_24x);
          var $__swJSW161__0;
          switch($__._tag_)
           {case 0:
             var $__6=
              new _A_($LightOO.$Core.$_23,[$__._1,$Graphics.$UI.$WXCore.$WindowClass.$windowGetChildren]);
             var $__7=
              new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ217,[$self,$f]);
             $__swJSW161__0=
              new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__7]);
             break;
            case 1:
             var $__8=
              new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$UHC.$Base.$Nothing__]);
             $__swJSW161__0=
              $__8;
             break;}
          return $__swJSW161__0;});
$Graphics.$UI.$WXCore.$WebWindow.$withSiblingsUNQ155=
 new _F_(function($self,$f)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$self,$Graphics.$UI.$WXCore.$WindowClass.$windowGetParent]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ207,[$self,$f]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ132=
 new _F_(function($__,$x2,$l,$w,$ws,$_24x)
         {var $__7=
           _e_($_24x);
          var $__swJSW162__0;
          switch($__7._tag_)
           {case 0:
             var $__8=
              new _A_($UHC.$Base.$_3a,[$w,$UHC.$Base.$_5b_5d]);
             var $__9=
              new _A_($UHC.$Base.$_2b_2b,[$l,$__8]);
             var $__10=
              [$__9,$UHC.$Base.$_5b_5d];
             var $__11=
              new _A_($Graphics.$UI.$WXCore.$WebWindow.$partition_27UNQ85,[$__10,$__,$x2,$ws]);
             $__swJSW162__0=
              $__11;
             break;
            case 1:
             var $__12=
              [$l,$w,$ws];
             var $__13=
              new _A_($UHC.$Base.$Just__,[$__12]);
             var $__14=
              new _A_($UHC.$Base.$return,[$__]);
             var $__15=
              new _A_($UHC.$Base.$_24,[$__14,$__13]);
             $__swJSW162__0=
              $__15;
             break;}
          return $__swJSW162__0;});
$Graphics.$UI.$WXCore.$WebWindow.$__197__1072__0NEW10UNQ119CCN=
 new _F_(function($x1,$__,$x2,$x3)
         {var $x15=
           _e_($x1);
          var $x38=
           _e_($x3);
          var $__swJSW164__0;
          switch($x38._tag_)
           {case 0:
             var $__11=
              new _A_($x2,[$x38._1]);
             var $__12=
              new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ132,[$__,$x2,$x15[0],$x38._1,$x38._2]);
             $__swJSW164__0=
              new _A_($UHC.$Base.$_3e_3e_3d,[$__,$__11,$__12]);
             break;
            case 1:
             $__swJSW164__0=
              $UHC.$Base.$undefined;
             break;}
          return $__swJSW164__0;});
$Graphics.$UI.$WXCore.$WebWindow.$partition_27UNQ85=
 new _F_(function($x1,$__,$x2,$x3)
         {var $__5=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__197__1072__0NEW10UNQ119CCN,[$x1,$__,$x2,$x3]);
          var $x36=
           _e_($x3);
          var $__swJSW165__0;
          switch($x36._tag_)
           {case 0:
             $__swJSW165__0=
              $__5;
             break;
            case 1:
             var $__9=
              new _A_($UHC.$Base.$return,[$__,$UHC.$Base.$Nothing__]);
             $__swJSW165__0=
              $__9;
             break;}
          return $__swJSW165__0;});
$Graphics.$UI.$WXCore.$WebWindow.$__199__64__0=
 new _F_(function($__)
         {return new _A_($Graphics.$UI.$WXCore.$WebWindow.$partition_27UNQ85,[$__,$UHC.$Base.$Monad__DCT74__339__0]);});
$Graphics.$UI.$WXCore.$WebWindow.$partition=
 new _A_(new _F_(function()
                 {var $__=
                   [$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d];
                  return new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__64__0,[$__]);}),[]);
$Graphics.$UI.$WXCore.$WebWindow.$__199__79__0=
 new _F_(function($id,$w)
         {var $__=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,$id]);
          var $__4=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
          var $__5=
           new _A_($UHC.$Base.$_2e,[$__4,$__]);
          var $__6=
           new _A_($LightOO.$Core.$_23,[$w,$Graphics.$UI.$WXCore.$WindowClass.$windowGetId]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__5]);});
$Graphics.$UI.$WXCore.$WebWindow.$partitionByWindowId=
 new _F_(function($id)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__79__0,[$id]);
          return new _A_($Graphics.$UI.$WXCore.$WebWindow.$partition,[$__]);});
$Graphics.$UI.$WXCore.$WindowClass.$__windowDestroy=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__windowDestroy;});
$Graphics.$UI.$WXCore.$WindowClass.$windowDestroy=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$WindowClass.$__windowDestroy,$Graphics.$UI.$WXCore.$WindowClass.$window__Methods]);}),[]);
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ654=
 new _F_(function($_24x,$w,$_24x3,$_24x4)
         {var $__=
           _e_($_24x4);
          var $__swJSW167__0;
          switch($__._tag_)
           {case 0:
             var $__7=
              _e_($__._1);
             var $__11=
              new _A_($UHC.$Base.$_2b_2b,[$__7[0],$__7[2]]);
             var $__12=
              new _A_($UHC.$IOBase.$writeIORef,[$_24x,$__11]);
             var $__13=
              new _A_($LightOO.$Core.$_23,[$__7[1],$Graphics.$UI.$WXCore.$WindowClass.$windowDestroy]);
             var $__14=
              new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__13,$__12]);
             $__swJSW167__0=
              $__14;
             break;
            case 1:
             var $__15=
              new _A_($UHC.$Base.$flip,[$Graphics.$UI.$WXCore.$WindowClass.$windowRemoveChild,$w]);
             var $__16=
              new _A_($UHC.$Base.$mapM__,[$UHC.$Base.$Monad__DCT74__339__0,$__15,$_24x3]);
             $__swJSW167__0=
              $__16;
             break;}
          return $__swJSW167__0;});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ645=
 new _F_(function($_24x,$w,$_24x3,$_24x4)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$partitionByWindowId,[$_24x3,$_24x4]);
          var $__6=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ654,[$_24x,$w,$_24x4]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__6]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ639=
 new _F_(function($_24x,$w,$_24x3)
         {var $__=
           new _A_($UHC.$IOBase.$readIORef,[$_24x]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ645,[$_24x,$w,$_24x3]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__5]);});
$Graphics.$UI.$WXCore.$WebWindow.$__197__4037__0UNQ635=
 new _F_(function($_24x,$w)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$w,$Graphics.$UI.$WXCore.$WindowClass.$windowGetId]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ639,[$_24x,$w]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Graphics.$UI.$WXCore.$WindowClass.$__windowRemoveChild=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__windowRemoveChild;});
$Graphics.$UI.$WXCore.$WindowClass.$windowRemoveChild=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$WindowClass.$__windowRemoveChild,$Graphics.$UI.$WXCore.$WindowClass.$window__Methods]);}),[]);
$Graphics.$UI.$WXCore.$WebWindow.$__199__817__0=
 new _F_(function($self,$parent)
         {return new _A_($Graphics.$UI.$WXCore.$WindowClass.$windowRemoveChild,[$parent,$self]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ566=
 new _F_(function($self,$newParent,$_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$UHC.$Base.$False__]);
          var $__5=
           new _A_($LightOO.$Core.$_23,[$self,$Graphics.$UI.$WXCore.$WindowClass.$windowAddChild]);
          var $__6=
           new _A_($UHC.$Base.$_24,[$__5,$newParent]);
          var $__7=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__]);
          var $__8=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
          var $__9=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__817__0,[$self]);
          var $__10=
           new _A_($UHC.$Base.$maybe,[$__8,$__9,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__10,$__7]);});
$Graphics.$UI.$WXCore.$WebWindow.$__197__4031__0UNQ559=
 new _F_(function($self,$newParent)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$self,$Graphics.$UI.$WXCore.$WindowClass.$windowGetParent]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ566,[$self,$newParent]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ581=
 new _F_(function($_24x)
         {var $__=
           new _A_($Data.$Maybe.$isJust,[$_24x]);
          var $__3=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
          return new _A_($UHC.$Base.$_24,[$__3,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ577=
 new _F_(function($self,$_24x)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$self,$Graphics.$UI.$WXCore.$WindowClass.$windowFindWindow]);
          var $__4=
           new _A_($UHC.$Base.$_24,[$__,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ581]);});
$Graphics.$UI.$WXCore.$WebWindow.$__197__4032__0UNQ571=
 new _F_(function($self,$w)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$w,$Graphics.$UI.$WXCore.$WindowClass.$windowGetId]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ577,[$self]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$UHC.$Base.$last=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW170__0;
          switch($__._tag_)
           {case 0:
             var $__5=
              new _A_($UHC.$Base.$last,[$__._2]);
             var $__6=
              _e_($__._2);
             var $__swJSW171__0;
             switch($__6._tag_)
              {case 0:
                $__swJSW171__0=
                 $__5;
                break;
               case 1:
                $__swJSW171__0=
                 $__._1;
                break;}
             $__swJSW170__0=
              $__swJSW171__0;
             break;
            case 1:
             $__swJSW170__0=
              $UHC.$Base.$undefined;
             break;}
          return $__swJSW170__0;});
$Graphics.$UI.$WXCore.$WebWindow.$__199__867__0=
 new _F_(function($r)
         {var $__=
           _e_($r);
          var $__swJSW172__0;
          switch($__._tag_)
           {case 0:
             var $__4=
              _e_($__._1);
             var $__8=
              new _A_($UHC.$Base.$last,[$__4[0]]);
             var $__9=
              new _A_($UHC.$Base.$_24,[$UHC.$Base.$Just__,$__8]);
             var $__10=
              new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__9]);
             $__swJSW172__0=
              $__10;
             break;
            case 1:
             var $__11=
              new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$UHC.$Base.$Nothing__]);
             $__swJSW172__0=
              $__11;
             break;}
          return $__swJSW172__0;});
$UHC.$OldIO.$__240__72=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["stdout"]);}),[]);
$UHC.$IOBase.$JSHandle__=
 new _F_(function($x1)
         {return {_tag_:0,_1:$x1};});
$UHC.$OldIO.$__240__71=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$IOBase.$JSHandle__,[$UHC.$OldIO.$__240__72]);}),[]);
$UHC.$IOBase.$OldHandle__=
 new _F_(function($x1)
         {return {_tag_:2,_1:$x1};});
$UHC.$OldIO.$stdout=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$IOBase.$OldHandle__,[$UHC.$OldIO.$__240__71]);}),[]);
$UHC.$OldIO.$hPutStr=
 new _F_(function($h,$s)
         {var $__=
           new _A_($UHC.$Base.$null,[$s]);
          var $__4=
           _e_($__);
          var $__swJSW174__0;
          switch($__4._tag_)
           {case 0:
             var $__5=
              new _A_($UHC.$Base.$tail,[$s]);
             var $__6=
              new _A_($UHC.$OldIO.$hPutStr,[$h,$__5]);
             var $__7=
              new _A_($UHC.$Base.$head,[$s]);
             var $__8=
              new _A_($UHC.$OldIO.$hPutChar,[$h,$__7]);
             var $__9=
              new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__8,$__6]);
             $__swJSW174__0=
              $__9;
             break;
            case 1:
             var $__10=
              new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
             $__swJSW174__0=
              $__10;
             break;}
          return $__swJSW174__0;});
$UHC.$OldIO.$primHPutChar=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primHPutChar($__3,$__4);});
$UHC.$OldIO.$__240__93__0=
 new _F_(function($h,$c,$__)
         {return new _A_($UHC.$OldIO.$primHPutChar,[$h,$c]);});
$UHC.$Base.$ioFromPrim=
 new _F_(function($f,$w)
         {var $x=
           new _A_($f,[$w]);
          var $x4=
           _e_($x);
          return [$w,$x];});
$UHC.$OldIO.$hPutChar=
 new _F_(function($h,$c)
         {var $__=
           new _A_($UHC.$OldIO.$__240__93__0,[$h,$c]);
          return new _A_($UHC.$Base.$ioFromPrim,[$__]);});
$UHC.$OldIO.$hPutStrLn=
 new _F_(function($h,$s)
         {var $__=
           new _A_($UHC.$OldIO.$hPutChar,[$h,10]);
          var $__4=
           new _A_($UHC.$OldIO.$hPutStr,[$h,$s]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__]);});
$UHC.$OldIO.$putStrLn=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$OldIO.$hPutStrLn,[$UHC.$OldIO.$stdout]);}),[]);
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ554=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$UHC.$Base.$False__]);
          var $__3=
           new _A_($UHC.$Base.$show,[$UHC.$Base.$Show__DCT74__128__0,$_24x]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToString,["destroy "]);
          var $__5=
           new _A_($UHC.$Base.$_2b_2b,[$__4,$__3]);
          var $__6=
           new _A_($UHC.$OldIO.$putStrLn,[$__5]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$__197__4029__0NEW343UNQ550=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$IOBase.$readIORef,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ554]);});
$Graphics.$UI.$WXCore.$WindowClass.$__windowGetParent=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__windowGetParent;});
$Graphics.$UI.$WXCore.$WindowClass.$windowGetParent=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$WindowClass.$__windowGetParent,$Graphics.$UI.$WXCore.$WindowClass.$window__Methods]);}),[]);
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ631=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$UHC.$Base.$Nothing__]);
          return new _A_($UHC.$Base.$maybe,[$__,$Graphics.$UI.$WXCore.$WindowClass.$windowGetParent,$_24x]);});
$Graphics.$UI.$WXCore.$WebWindow.$__197__4036__0NEW392UNQ628=
 new _F_(function($self)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$self,$Graphics.$UI.$WXCore.$WindowClass.$windowGetParent]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ631]);});
$Graphics.$UI.$WXCore.$WindowClass.$__windowGetChildren=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__windowGetChildren;});
$Graphics.$UI.$WXCore.$WindowClass.$windowGetChildren=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$WindowClass.$__windowGetChildren,$Graphics.$UI.$WXCore.$WindowClass.$window__Methods]);}),[]);
$Graphics.$UI.$WXCore.$WebWindow.$__199__199__0=
 new _F_(function($ident,$i)
         {var $__=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,$i,$ident]);
          var $__4=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
          return new _A_($UHC.$Base.$_24,[$__4,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$__199__192__0=
 new _F_(function($ident,$w)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$w,$Graphics.$UI.$WXCore.$WindowClass.$windowGetId]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__199__0,[$ident]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Control.$Monad.$__120__30__0=
 new _F_(function($__,$x1,$xs,$fax)
         {return new _A_($Control.$Monad.$foldM,[$__,$x1,$fax,$xs]);});
$Control.$Monad.$foldM=
 new _F_(function($__,$x1,$x2,$x3)
         {var $x35=
           _e_($x3);
          var $__swJSW177__0;
          switch($x35._tag_)
           {case 0:
             var $__8=
              new _A_($x1,[$x2,$x35._1]);
             var $__9=
              new _A_($Control.$Monad.$__120__30__0,[$__,$x1,$x35._2]);
             var $__10=
              new _A_($UHC.$Base.$_3e_3e_3d,[$__,$__8,$__9]);
             $__swJSW177__0=
              $__10;
             break;
            case 1:
             var $__11=
              new _A_($UHC.$Base.$return,[$__,$x2]);
             $__swJSW177__0=
              $__11;
             break;}
          return $__swJSW177__0;});
$Control.$Monad.$mplus=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$Control.$Monad.$MonadPlus__DCT116__4__0DFLControl_2eMonad_2emplus=
 new _F_(function($x1,$x2)
         {var $x13=
           _e_($x1);
          var $__swJSW179__0;
          switch($x13._tag_)
           {case 0:
             $__swJSW179__0=
              $x1;
             break;
            case 1:
             $__swJSW179__0=
              $x2;
             break;}
          return $__swJSW179__0;});
$UHC.$Base.$Monad__DCT74__75__0DFLUHC_2eBase_2e_3e_3e_3d=
 new _F_(function($x1,$x2)
         {var $x13=
           _e_($x1);
          var $__swJSW180__0;
          switch($x13._tag_)
           {case 0:
             var $__=
              new _A_($x2,[$x13._1]);
             $__swJSW180__0=
              $__;
             break;
            case 1:
             $__swJSW180__0=
              $UHC.$Base.$Nothing__;
             break;}
          return $__swJSW180__0;});
$UHC.$Base.$Monad__DCT74__75__0DFLUHC_2eBase_2efail=
 new _F_(function($s)
         {return $UHC.$Base.$Nothing__;});
$UHC.$Base.$Monad__NEW3796UNQ10259EVLDCT74__75__0RDC=
 new _F_(function($Monad__)
         {var $Monad__2=
           _e_(new _A_($UHC.$Base.$Monad__CLS74__45__0,[$Monad__]));
          var $__7=
           {_tag_:0,_1:$Monad__2._1,_2:$UHC.$Base.$Monad__DCT74__75__0DFLUHC_2eBase_2e_3e_3e_3d,_3:$UHC.$Base.$Monad__DCT74__75__0DFLUHC_2eBase_2efail,_4:$UHC.$Base.$Just__};
          return $__7;});
$UHC.$Base.$Monad__NEW3794UNQ10258DCT74__75__0RDC=
 new _F_(function($Monad__)
         {var $Monad__2=
           new _A_($UHC.$Base.$Monad__NEW3796UNQ10259EVLDCT74__75__0RDC,[$Monad__]);
          return $Monad__2;});
$UHC.$Base.$Monad__UNQ10258DCT74__75__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Monad__NEW3794UNQ10258DCT74__75__0RDC,[$UHC.$Base.$Monad__UNQ10258DCT74__75__0RDC]);}),[]);
$UHC.$Base.$Monad__DCT74__75__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Monad__UNQ10258DCT74__75__0RDC;}),[]);
$Control.$Monad.$MonadPlus__CLS116__0__0=
 new _F_(function($MonadPlus__)
         {var $MonadPlus__2=
           {_tag_:0,_1:$UHC.$Base.$undefined,_2:$UHC.$Base.$undefined,_3:$UHC.$Base.$undefined};
          return $MonadPlus__2;});
$Control.$Monad.$MonadPlus__NEW47UNQ312EVLDCT116__4__0RDC=
 new _F_(function($MonadPlus__)
         {var $MonadPlus__2=
           _e_(new _A_($Control.$Monad.$MonadPlus__CLS116__0__0,[$MonadPlus__]));
          var $__6=
           {_tag_:0,_1:$Control.$Monad.$MonadPlus__DCT116__4__0DFLControl_2eMonad_2emplus,_2:$UHC.$Base.$Nothing__,_3:$UHC.$Base.$Monad__DCT74__75__0};
          return $__6;});
$Control.$Monad.$MonadPlus__NEW45UNQ311DCT116__4__0RDC=
 new _F_(function($MonadPlus__)
         {var $MonadPlus__2=
           new _A_($Control.$Monad.$MonadPlus__NEW47UNQ312EVLDCT116__4__0RDC,[$MonadPlus__]);
          return $MonadPlus__2;});
$Control.$Monad.$MonadPlus__UNQ311DCT116__4__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Control.$Monad.$MonadPlus__NEW45UNQ311DCT116__4__0RDC,[$Control.$Monad.$MonadPlus__UNQ311DCT116__4__0RDC]);}),[]);
$Control.$Monad.$MonadPlus__DCT116__4__0=
 new _A_(new _F_(function()
                 {return $Control.$Monad.$MonadPlus__UNQ311DCT116__4__0RDC;}),[]);
$Graphics.$UI.$WXCore.$WebWindow.$__199__178__0=
 new _F_(function($a,$r)
         {var $__=
           new _A_($Control.$Monad.$mplus,[$Control.$Monad.$MonadPlus__DCT116__4__0,$a,$r]);
          var $__4=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
          return new _A_($UHC.$Base.$_24,[$__4,$__]);});
$Graphics.$UI.$WXCore.$WindowClass.$__windowFindWindow=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__windowFindWindow;});
$Graphics.$UI.$WXCore.$WindowClass.$windowFindWindow=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$WindowClass.$__windowFindWindow,$Graphics.$UI.$WXCore.$WindowClass.$window__Methods]);}),[]);
$Graphics.$UI.$WXCore.$WebWindow.$__199__168__0=
 new _F_(function($ident,$a,$w)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$w,$Graphics.$UI.$WXCore.$WindowClass.$windowFindWindow,$ident]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__178__0,[$a]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__5]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ183=
 new _F_(function($ident,$_24x,$_24x3)
         {var $__=
           _e_($_24x3);
          var $__swJSW184__0;
          switch($__._tag_)
           {case 0:
             var $__7=
              new _A_($UHC.$Base.$Just__,[$__._1]);
             var $__8=
              new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
             var $__9=
              new _A_($UHC.$Base.$_24,[$__8,$__7]);
             $__swJSW184__0=
              $__9;
             break;
            case 1:
             var $__10=
              new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__168__0,[$ident]);
             var $__11=
              new _A_($Control.$Monad.$foldM,[$UHC.$Base.$Monad__DCT74__339__0,$__10,$UHC.$Base.$Nothing__,$_24x]);
             $__swJSW184__0=
              $__11;
             break;}
          return $__swJSW184__0;});
$Control.$Monad.$_24okUNQ291=
 new _F_(function($__,$x1,$xs,$x,$_24x)
         {var $__6=
           new _A_($Control.$Monad.$filterM,[$__,$x1,$xs]);
          var $__7=
           new _A_($Control.$Monad.$_24okUNQ295,[$__,$x,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$__6,$__7]);});
$Control.$Monad.$_24okUNQ295=
 new _F_(function($__,$x,$_24x,$_24x4)
         {var $__5=
           new _A_($Control.$Monad.$__120__428NEW95,[$x,$_24x,$_24x4]);
          return new _A_($UHC.$Base.$return,[$__,$__5]);});
$Control.$Monad.$__120__428NEW95=
 new _F_(function($x,$_24x,$_24x3)
         {var $__=
           _e_($_24x);
          var $__swJSW185__0;
          switch($__._tag_)
           {case 0:
             $__swJSW185__0=
              $_24x3;
             break;
            case 1:
             var $__5=
              new _A_($UHC.$Base.$_3a,[$x,$_24x3]);
             $__swJSW185__0=
              $__5;
             break;}
          return $__swJSW185__0;});
$Control.$Monad.$filterM=
 new _F_(function($__,$x1,$x2)
         {var $x24=
           _e_($x2);
          var $__swJSW186__0;
          switch($x24._tag_)
           {case 0:
             var $__7=
              new _A_($x1,[$x24._1]);
             var $__8=
              new _A_($Control.$Monad.$_24okUNQ291,[$__,$x1,$x24._2,$x24._1]);
             $__swJSW186__0=
              new _A_($UHC.$Base.$_3e_3e_3d,[$__,$__7,$__8]);
             break;
            case 1:
             var $__9=
              new _A_($UHC.$Base.$return,[$__,$UHC.$Base.$_5b_5d]);
             $__swJSW186__0=
              $__9;
             break;}
          return $__swJSW186__0;});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ173=
 new _F_(function($ident,$_24x)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__192__0,[$ident]);
          var $__4=
           new _A_($Control.$Monad.$filterM,[$UHC.$Base.$Monad__DCT74__339__0,$__,$_24x]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ183,[$ident,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__5]);});
$Graphics.$UI.$WXCore.$WebWindow.$recurseInChildrenUNQ161=
 new _F_(function($self,$ident)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$self,$Graphics.$UI.$WXCore.$WindowClass.$windowGetChildren]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ173,[$ident]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ165=
 new _F_(function($self,$ident,$_24x)
         {var $__=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,$ident,$_24x]);
          var $__5=
           _e_($__);
          var $__swJSW187__0;
          switch($__5._tag_)
           {case 0:
             var $__6=
              new _A_($Graphics.$UI.$WXCore.$WebWindow.$recurseInChildrenUNQ161,[$self,$ident]);
             $__swJSW187__0=
              $__6;
             break;
            case 1:
             var $__7=
              new _A_($UHC.$Base.$Just__,[$self]);
             var $__8=
              new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
             var $__9=
              new _A_($UHC.$Base.$_24,[$__8,$__7]);
             $__swJSW187__0=
              $__9;
             break;}
          return $__swJSW187__0;});
$Graphics.$UI.$WXCore.$WebWindow.$findWindow_27UNQ153=
 new _F_(function($self,$ident)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$self,$Graphics.$UI.$WXCore.$WindowClass.$windowGetId]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ165,[$self,$ident]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Graphics.$UI.$WXCore.$WebWindow.$__199__977__0=
 new _F_(function($c,$_24x__196__21__0)
         {var $__=
           new _A_($UHC.$Base.$_3a,[$c,$UHC.$Base.$_5b_5d]);
          return new _A_($UHC.$Base.$_2b_2b,[$_24x__196__21__0,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$__197__4042__0UNQ678=
 new _F_(function($_24x,$c)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__977__0,[$c]);
          return new _A_($Data.$IORef.$modifyIORef,[$_24x,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ497=
 new _F_(function($cont,$_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
          return new _A_($UHC.$Base.$maybe,[$cont,$__,$_24x]);});
$Graphics.$UI.$WXCore.$WebWindow.$getDCNEW295UNQ436=
 new _F_(function($_24x,$cont)
         {var $__=
           new _A_($UHC.$IOBase.$readIORef,[$_24x]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ497,[$cont]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Language.$UHC.$JS.$HTML5.$HTMLDocument.$document=
 new _F_(function($__)
         {var $__2=
           _e_(document);
          return [$__,$__2];});
$Language.$UHC.$JS.$HTML5.$HTMLDocument.$createElement=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__);
          var $__5=
           _e_($__2);
          var $__6=
           _e_($__4.createElement($__5));
          return [$__3,$__6];});
$UHC.$Base.$primIntegerToPackedString=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.toString();});
$UHC.$Base.$Show__DCT74__157__0DFLUHC_2eBase_2eshow=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$UHC.$Base.$packedStringToString,$UHC.$Base.$primIntegerToPackedString]);}),[]);
$UHC.$Base.$Show__NEW5756UNQ11694EVLDCT74__157__0RDC=
 new _F_(function($Show__,$Show__2)
         {var $Show__3=
           _e_(new _A_($UHC.$Base.$Show__CLS74__43__0,[$Show__2]));
          var $__7=
           {_tag_:0,_1:$Show__,_2:$Show__3._2,_3:$Show__3._3};
          return $__7;});
$UHC.$Base.$Show__NEW5753UNQ11693DCT74__157__0RDC=
 new _F_(function($Show__,$Show__2)
         {var $Show__3=
           new _A_($UHC.$Base.$Show__NEW5756UNQ11694EVLDCT74__157__0RDC,[$Show__,$Show__2]);
          return $Show__3;});
$UHC.$Base.$Show__UNQ11693DCT74__157__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Show__NEW5753UNQ11693DCT74__157__0RDC,[$UHC.$Base.$Show__DCT74__157__0DFLUHC_2eBase_2eshow,$UHC.$Base.$Show__UNQ11693DCT74__157__0RDC]);}),[]);
$UHC.$Base.$Show__DCT74__157__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Show__UNQ11693DCT74__157__0RDC;}),[]);
$UHC.$Base.$__78__12917=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$show,[$UHC.$Base.$Show__DCT74__157__0]);}),[]);
$UHC.$Base.$__78__12918=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$toInteger,[$UHC.$Base.$Integral__DCT74__110__0]);}),[]);
$UHC.$Base.$Show__DCT74__128__0DFLUHC_2eBase_2eshow=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$UHC.$Base.$__78__12917,$UHC.$Base.$__78__12918]);}),[]);
$UHC.$Base.$showString=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$_2b_2b;}),[]);
$UHC.$Base.$showChar=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$_3a;}),[]);
$UHC.$Base.$shows=
 new _F_(function($__)
         {return new _A_($UHC.$Base.$showsPrec,[$__,0]);});
$UHC.$Base.$showlUNQ8909=
 new _F_(function($__,$x1)
         {var $__3=
           _e_($x1);
          var $__swJSW189__0;
          switch($__3._tag_)
           {case 0:
             var $__6=
              new _A_($UHC.$Base.$showlUNQ8909,[$__,$__3._2]);
             var $__7=
              new _A_($UHC.$Base.$shows,[$__,$__3._1]);
             var $__8=
              new _A_($UHC.$Base.$_2e,[$__7,$__6]);
             var $__9=
              new _A_($UHC.$Base.$showChar,[44]);
             var $__10=
              new _A_($UHC.$Base.$_2e,[$__9,$__8]);
             $__swJSW189__0=
              $__10;
             break;
            case 1:
             var $__11=
              new _A_($UHC.$Base.$showChar,[93]);
             $__swJSW189__0=
              $__11;
             break;}
          return $__swJSW189__0;});
$UHC.$Base.$Show__CLS74__43__0DFLUHC_2eBase_2eshowList=
 new _F_(function($Show__,$x1)
         {var $__=
           _e_($x1);
          var $__swJSW190__0;
          switch($__._tag_)
           {case 0:
             var $__6=
              new _A_($UHC.$Base.$showlUNQ8909,[$Show__,$__._2]);
             var $__7=
              new _A_($UHC.$Base.$shows,[$Show__,$__._1]);
             var $__8=
              new _A_($UHC.$Base.$_2e,[$__7,$__6]);
             var $__9=
              new _A_($UHC.$Base.$showChar,[91]);
             $__swJSW190__0=
              new _A_($UHC.$Base.$_2e,[$__9,$__8]);
             break;
            case 1:
             var $__10=
              new _A_($UHC.$Base.$packedStringToString,["[]"]);
             var $__11=
              new _A_($UHC.$Base.$showString,[$__10]);
             $__swJSW190__0=
              $__11;
             break;}
          return $__swJSW190__0;});
$UHC.$Base.$showsPrec=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._3;});
$UHC.$Base.$Show__CLS74__43__0DFLUHC_2eBase_2eshow=
 new _F_(function($Show__,$x)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,[""]);
          return new _A_($UHC.$Base.$showsPrec,[$Show__,0,$x,$__]);});
$UHC.$Base.$Show__CLS74__43__0DFLUHC_2eBase_2eshowsPrec=
 new _F_(function($Show__,$__,$x)
         {var $__4=
           new _A_($UHC.$Base.$show,[$Show__,$x]);
          return new _A_($UHC.$Base.$_2b_2b,[$__4]);});
$UHC.$Base.$Show__CLS74__43__0=
 new _F_(function($Show__)
         {var $__=
           new _A_($UHC.$Base.$Show__CLS74__43__0DFLUHC_2eBase_2eshowsPrec,[$Show__]);
          var $__3=
           new _A_($UHC.$Base.$Show__CLS74__43__0DFLUHC_2eBase_2eshowList,[$Show__]);
          var $__4=
           new _A_($UHC.$Base.$Show__CLS74__43__0DFLUHC_2eBase_2eshow,[$Show__]);
          var $Show__5=
           {_tag_:0,_1:$__4,_2:$__3,_3:$__};
          return $Show__5;});
$UHC.$Base.$Show__NEW6397UNQ11688EVLDCT74__128__0RDC=
 new _F_(function($Show__,$Show__2)
         {var $Show__3=
           _e_(new _A_($UHC.$Base.$Show__CLS74__43__0,[$Show__]));
          var $__7=
           {_tag_:0,_1:$Show__2,_2:$Show__3._2,_3:$Show__3._3};
          return $__7;});
$UHC.$Base.$Show__NEW6394UNQ11685DCT74__128__0RDC=
 new _F_(function($Show__,$Show__2)
         {var $Show__3=
           new _A_($UHC.$Base.$Show__NEW6397UNQ11688EVLDCT74__128__0RDC,[$Show__,$Show__2]);
          return $Show__3;});
$UHC.$Base.$Show__UNQ11685DCT74__128__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Show__NEW6394UNQ11685DCT74__128__0RDC,[$UHC.$Base.$Show__UNQ11685DCT74__128__0RDC,$UHC.$Base.$Show__DCT74__128__0DFLUHC_2eBase_2eshow]);}),[]);
$UHC.$Base.$Show__DCT74__128__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Show__UNQ11685DCT74__128__0RDC;}),[]);
$UHC.$Base.$show=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$Language.$UHC.$JS.$HTML5.$Node.$firstChild=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__3.firstChild);
          return [$__2,$__4];});
$Language.$UHC.$JS.$HTML5.$Node.$insertBefore=
 new _F_(function($__,$__2,$__3,$__4)
         {var $__5=
           _e_($__);
          var $__6=
           _e_($__2);
          var $__7=
           _e_($__3);
          var $__8=
           _e_($__5.insertBefore($__6,$__7));
          return [$__4,$__8];});
$Language.$UHC.$JS.$HTML5.$Types.$__htmlCanvasElementRef=
 new _A_(new _F_(function()
                 {return HTMLCanvasElement;}),[]);
$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__DCT99__1__0DFLLanguage_2eUHC_2eJS_2ePrelude_2egetObjectRef=
 new _F_(function($__)
         {return $Language.$UHC.$JS.$HTML5.$Types.$__htmlCanvasElementRef;});
$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__NEW195UNQ390EVLDCT99__1__0RDC=
 new _F_(function($GetObjectRef__)
         {var $GetObjectRef__2=
           _e_(new _A_($Language.$UHC.$JS.$Prelude.$GetObjectRef__CLS93__0__0,[$GetObjectRef__]));
          var $__4=
           {_tag_:0,_1:$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__DCT99__1__0DFLLanguage_2eUHC_2eJS_2ePrelude_2egetObjectRef};
          return $__4;});
$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__NEW193UNQ389DCT99__1__0RDC=
 new _F_(function($GetObjectRef__)
         {var $GetObjectRef__2=
           new _A_($Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__NEW195UNQ390EVLDCT99__1__0RDC,[$GetObjectRef__]);
          return $GetObjectRef__2;});
$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__UNQ389DCT99__1__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__NEW193UNQ389DCT99__1__0RDC,[$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__UNQ389DCT99__1__0RDC]);}),[]);
$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__DCT99__1__0=
 new _A_(new _F_(function()
                 {return $Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__UNQ389DCT99__1__0RDC;}),[]);
$Graphics.$UI.$WXCore.$Types.$sizeH=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.sizeH;});
$Graphics.$UI.$WXCore.$Types.$sizeW=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.sizeW;});
$Language.$UHC.$JS.$HTML5.$Types.$__canvasRenderingContext2D=
 new _A_(new _F_(function()
                 {return CanvasRenderingContext2D;}),[]);
$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__DCT99__2__0DFLLanguage_2eUHC_2eJS_2ePrelude_2egetObjectRef=
 new _F_(function($__)
         {return $Language.$UHC.$JS.$HTML5.$Types.$__canvasRenderingContext2D;});
$Language.$UHC.$JS.$Prelude.$GetObjectRef__CLS93__0__0=
 new _F_(function($GetObjectRef__)
         {var $GetObjectRef__2=
           {_tag_:0,_1:$UHC.$Base.$undefined};
          return $GetObjectRef__2;});
$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__NEW203UNQ400EVLDCT99__2__0RDC=
 new _F_(function($GetObjectRef__)
         {var $GetObjectRef__2=
           _e_(new _A_($Language.$UHC.$JS.$Prelude.$GetObjectRef__CLS93__0__0,[$GetObjectRef__]));
          var $__4=
           {_tag_:0,_1:$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__DCT99__2__0DFLLanguage_2eUHC_2eJS_2ePrelude_2egetObjectRef};
          return $__4;});
$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__NEW201UNQ399DCT99__2__0RDC=
 new _F_(function($GetObjectRef__)
         {var $GetObjectRef__2=
           new _A_($Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__NEW203UNQ400EVLDCT99__2__0RDC,[$GetObjectRef__]);
          return $GetObjectRef__2;});
$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__UNQ399DCT99__2__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__NEW201UNQ399DCT99__2__0RDC,[$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__UNQ399DCT99__2__0RDC]);}),[]);
$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__DCT99__2__0=
 new _A_(new _F_(function()
                 {return $Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__UNQ399DCT99__2__0RDC;}),[]);
$Language.$UHC.$JS.$Prelude.$getObjectRef=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$Language.$UHC.$JS.$Primitives.$__primInstanceOf=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primInstanceOf($__3,$__4);});
$Language.$UHC.$JS.$Prelude.$cast=
 new _F_(function($__,$a)
         {var $__3=
           new _A_($Language.$UHC.$JS.$Prelude.$getObjectRef,[$__,$UHC.$Base.$undefined]);
          var $__4=
           new _A_($Language.$UHC.$JS.$Primitives.$__primInstanceOf,[$a,$__3]);
          var $__5=
           _e_($__4);
          var $__swJSW199__0;
          switch($__5._tag_)
           {case 0:
             $__swJSW199__0=
              $UHC.$Base.$Nothing__;
             break;
            case 1:
             var $__6=
              new _A_($UHC.$Base.$unsafeCoerce,[$a]);
             var $__7=
              new _A_($UHC.$Base.$Just__,[$__6]);
             $__swJSW199__0=
              $__7;
             break;}
          return $__swJSW199__0;});
$Language.$UHC.$JS.$HTML5.$HTMLCanvasElement.$_24okUNQ37=
 new _F_(function($_24x)
         {var $__=
           new _A_($Language.$UHC.$JS.$Prelude.$cast,[$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__DCT99__2__0,$_24x]);
          var $__3=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
          return new _A_($UHC.$Base.$_24,[$__3,$__]);});
$Language.$UHC.$JS.$HTML5.$HTMLCanvasElement.$getContext=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__);
          var $__5=
           _e_($__2);
          var $__6=
           _e_($__4.getContext($__5));
          return [$__3,$__6];});
$Language.$UHC.$JS.$HTML5.$HTMLCanvasElement.$get2DContext=
 new _F_(function($e)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["2d"]);
          var $d=
           new _A_($Language.$UHC.$JS.$Marshal.$toJS,[$Language.$UHC.$JS.$Marshal.$ToJS__DCT81__13__0,$__]);
          var $__4=
           new _A_($Language.$UHC.$JS.$HTML5.$HTMLCanvasElement.$getContext,[$e,$d]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$Language.$UHC.$JS.$HTML5.$HTMLCanvasElement.$_24okUNQ37]);});
$Language.$UHC.$JS.$HTML5.$CanvasRenderingContext2D.$drawImage=
 new _F_(function($__,$__2,$__3,$__4,$__5,$__6,$__7)
         {var $__8=
           _e_($__);
          var $__9=
           _e_($__2);
          var $__10=
           _e_($__3);
          var $__11=
           _e_($__4);
          var $__12=
           _e_($__5);
          var $__13=
           _e_($__6);
          var $__14=
           _e_($__8.drawImage($__9,$__10,$__11,$__12,$__13));
          var $__15=
           _e_([]);
          return [$__7,$__15];});
$Graphics.$UI.$WXCore.$GraphicsContext.$_24okUNQ56=
 new _F_(function($dc,$x,$y,$w,$h,$_24x)
         {return new _A_($LightOO.$Core.$_23,[$dc,$Language.$UHC.$JS.$HTML5.$CanvasRenderingContext2D.$drawImage,$_24x,$x,$y,$w,$h]);});
$Graphics.$UI.$WXCore.$GraphicsContext.$__149__89__0UNQ49=
 new _F_(function($dc,$bm,$x,$y,$w,$h)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$bm,$Graphics.$UI.$WXCore.$GraphicsBitmapClass.$graphicsBitmapGetNativeBitmap]);
          var $__8=
           new _A_($Graphics.$UI.$WXCore.$GraphicsContext.$_24okUNQ56,[$dc,$x,$y,$w,$h]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__8]);});
$Graphics.$UI.$WXCore.$Types.$rectTop=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.rectTop;});
$Language.$UHC.$JS.$HTML5.$CanvasRenderingContext2D.$setTransform=
 new _F_(function($__,$__2,$__3,$__4,$__5,$__6,$__7,$__8)
         {var $__9=
           _e_($__);
          var $__10=
           _e_($__2);
          var $__11=
           _e_($__3);
          var $__12=
           _e_($__4);
          var $__13=
           _e_($__5);
          var $__14=
           _e_($__6);
          var $__15=
           _e_($__7);
          var $__16=
           _e_($__9.setTransform($__10,$__11,$__12,$__13,$__14,$__15));
          var $__17=
           _e_([]);
          return [$__8,$__17];});
$Graphics.$UI.$WXCore.$Types.$rectWidth=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.rectWidth;});
$Language.$UHC.$JS.$HTML5.$CanvasRenderingContext2D.$clearRect=
 new _F_(function($__,$__2,$__3,$__4,$__5,$__6)
         {var $__7=
           _e_($__);
          var $__8=
           _e_($__2);
          var $__9=
           _e_($__3);
          var $__10=
           _e_($__4);
          var $__11=
           _e_($__5);
          var $__12=
           _e_($__7.clearRect($__8,$__9,$__10,$__11));
          var $__13=
           _e_([]);
          return [$__6,$__13];});
$Graphics.$UI.$WXCore.$Types.$rectLeft=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.rectLeft;});
$Graphics.$UI.$WXCore.$Types.$rectHeight=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.rectHeight;});
$Graphics.$UI.$WXCore.$GraphicsContext.$__149__68__0UNQ27=
 new _F_(function($dc,$rect)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Types.$rectHeight,[$rect]);
          var $__4=
           new _A_($UHC.$Base.$fromIntegral,[$UHC.$Base.$Integral__DCT74__110__0,$UHC.$Base.$Num__DCT74__177__0]);
          var $__5=
           new _A_($UHC.$Base.$_24,[$__4,$__]);
          var $__6=
           new _A_($Graphics.$UI.$WXCore.$Types.$rectWidth,[$rect]);
          var $__7=
           new _A_($UHC.$Base.$fromIntegral,[$UHC.$Base.$Integral__DCT74__110__0,$UHC.$Base.$Num__DCT74__177__0]);
          var $__8=
           new _A_($UHC.$Base.$_24,[$__7,$__6]);
          var $__9=
           new _A_($Graphics.$UI.$WXCore.$Types.$rectTop,[$rect]);
          var $__10=
           new _A_($UHC.$Base.$fromIntegral,[$UHC.$Base.$Integral__DCT74__110__0,$UHC.$Base.$Num__DCT74__177__0]);
          var $__11=
           new _A_($UHC.$Base.$_24,[$__10,$__9]);
          var $__12=
           new _A_($Graphics.$UI.$WXCore.$Types.$rectLeft,[$rect]);
          var $__13=
           new _A_($UHC.$Base.$fromIntegral,[$UHC.$Base.$Integral__DCT74__110__0,$UHC.$Base.$Num__DCT74__177__0]);
          var $__14=
           new _A_($UHC.$Base.$_24,[$__13,$__12]);
          var $__15=
           new _A_($Language.$UHC.$JS.$HTML5.$CanvasRenderingContext2D.$clearRect,[$dc,$__14,$__11,$__8,$__5]);
          var $__16=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__17=
           new _A_($UHC.$Base.$fromInteger,[$UHC.$Base.$Num__DCT74__177__0,$__16]);
          var $__18=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__19=
           new _A_($UHC.$Base.$fromInteger,[$UHC.$Base.$Num__DCT74__177__0,$__18]);
          var $__20=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__21=
           new _A_($UHC.$Base.$fromInteger,[$UHC.$Base.$Num__DCT74__177__0,$__20]);
          var $__22=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__23=
           new _A_($UHC.$Base.$fromInteger,[$UHC.$Base.$Num__DCT74__177__0,$__22]);
          var $__24=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__25=
           new _A_($UHC.$Base.$fromInteger,[$UHC.$Base.$Num__DCT74__177__0,$__24]);
          var $__26=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__27=
           new _A_($UHC.$Base.$fromInteger,[$UHC.$Base.$Num__DCT74__177__0,$__26]);
          var $__28=
           new _A_($Language.$UHC.$JS.$HTML5.$CanvasRenderingContext2D.$setTransform,[$dc,$__27,$__25,$__23,$__21,$__19,$__17]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__28,$__15]);});
$Language.$UHC.$JS.$HTML5.$CanvasRenderingContext2D.$__fillRect=
 new _F_(function($__,$__2,$__3,$__4,$__5,$__6)
         {var $__7=
           _e_($__);
          var $__8=
           _e_($__2);
          var $__9=
           _e_($__3);
          var $__10=
           _e_($__4);
          var $__11=
           _e_($__5);
          var $__12=
           _e_($__7.fillRect($__8,$__9,$__10,$__11));
          var $__13=
           _e_([]);
          return [$__6,$__13];});
$Language.$UHC.$JS.$HTML5.$CanvasRenderingContext2D.$fillRect=
 new _F_(function($a,$b,$c,$d,$dc)
         {return new _A_($Language.$UHC.$JS.$HTML5.$CanvasRenderingContext2D.$__fillRect,[$dc,$a,$b,$c,$d]);});
$Graphics.$UI.$WXCore.$GraphicsContext.$__149__92__0UNQ62=
 new _F_(function($dc,$x,$y,$w,$h)
         {var $__=
           new _A_($Language.$UHC.$JS.$HTML5.$CanvasRenderingContext2D.$fillRect,[$x,$y,$w,$h]);
          return new _A_($LightOO.$Core.$_23,[$dc,$__]);});
$Graphics.$UI.$WXCore.$GraphicsContext.$__151__14NEW2=
 new _F_(function($dc,$tail)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextTranslate not implemented"]);
          var $__4=
           new _A_($UHC.$Base.$error,[$__]);
          var $__5=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextStrokePath not implemented"]);
          var $__6=
           new _A_($UHC.$Base.$error,[$__5]);
          var $__7=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextStrokeLine not implemented"]);
          var $__8=
           new _A_($UHC.$Base.$error,[$__7]);
          var $__9=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextSetTransform not implemented"]);
          var $__10=
           new _A_($UHC.$Base.$error,[$__9]);
          var $__11=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextSetPen not implemented"]);
          var $__12=
           new _A_($UHC.$Base.$error,[$__11]);
          var $__13=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextSetInterpolationQuality not implemented"]);
          var $__14=
           new _A_($UHC.$Base.$error,[$__13]);
          var $__15=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextSetFont not implemented"]);
          var $__16=
           new _A_($UHC.$Base.$error,[$__15]);
          var $__17=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextSetCompositionMode not implemented"]);
          var $__18=
           new _A_($UHC.$Base.$error,[$__17]);
          var $__19=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextSetBrush not implemented"]);
          var $__20=
           new _A_($UHC.$Base.$error,[$__19]);
          var $__21=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextSetAntialiasMode not implemented"]);
          var $__22=
           new _A_($UHC.$Base.$error,[$__21]);
          var $__23=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextScale not implemented"]);
          var $__24=
           new _A_($UHC.$Base.$error,[$__23]);
          var $__25=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextRotate not implemented"]);
          var $__26=
           new _A_($UHC.$Base.$error,[$__25]);
          var $__27=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextResetClip not implemented"]);
          var $__28=
           new _A_($UHC.$Base.$error,[$__27]);
          var $__29=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextGetTransform not implemented"]);
          var $__30=
           new _A_($UHC.$Base.$error,[$__29]);
          var $__31=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextGetInterpolationQuality not implemented"]);
          var $__32=
           new _A_($UHC.$Base.$error,[$__31]);
          var $__33=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextGetCompositionMode not implemented"]);
          var $__34=
           new _A_($UHC.$Base.$error,[$__33]);
          var $__35=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextGetAntialiasMode not implemented"]);
          var $__36=
           new _A_($UHC.$Base.$error,[$__35]);
          var $__37=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextFillPath not implemented"]);
          var $__38=
           new _A_($UHC.$Base.$error,[$__37]);
          var $__39=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextEndLayer not implemented"]);
          var $__40=
           new _A_($UHC.$Base.$error,[$__39]);
          var $__41=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextEnableOffset not implemented"]);
          var $__42=
           new _A_($UHC.$Base.$error,[$__41]);
          var $__43=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextDrawText not implemented"]);
          var $__44=
           new _A_($UHC.$Base.$error,[$__43]);
          var $__45=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextDrawRoundedRectangle not implemented"]);
          var $__46=
           new _A_($UHC.$Base.$error,[$__45]);
          var $__47=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextDrawPath not implemented"]);
          var $__48=
           new _A_($UHC.$Base.$error,[$__47]);
          var $__49=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextDrawLines not implemented"]);
          var $__50=
           new _A_($UHC.$Base.$error,[$__49]);
          var $__51=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextDrawEllipse not implemented"]);
          var $__52=
           new _A_($UHC.$Base.$error,[$__51]);
          var $__53=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextDisableOffset not implemented"]);
          var $__54=
           new _A_($UHC.$Base.$error,[$__53]);
          var $__55=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextCreateFont not implemented"]);
          var $__56=
           new _A_($UHC.$Base.$error,[$__55]);
          var $__57=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextCreateBrush not implemented"]);
          var $__58=
           new _A_($UHC.$Base.$error,[$__57]);
          var $__59=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextConcatTransform not implemented"]);
          var $__60=
           new _A_($UHC.$Base.$error,[$__59]);
          var $__61=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextClip not implemented"]);
          var $__62=
           new _A_($UHC.$Base.$error,[$__61]);
          var $__63=
           new _A_($UHC.$Base.$packedStringToString,["_graphicsContextBeginLayer not implemented"]);
          var $__64=
           new _A_($UHC.$Base.$error,[$__63]);
          var $__65=
           new _A_($Graphics.$UI.$WXCore.$GraphicsContext.$__149__68__0UNQ27,[$dc]);
          var $__66=
           new _A_($Graphics.$UI.$WXCore.$GraphicsContext.$__149__92__0UNQ62,[$dc]);
          var $__67=
           new _A_($Graphics.$UI.$WXCore.$GraphicsContext.$__149__89__0UNQ49,[$dc]);
          return {_tag_:0,__graphicsContextBeginLayer:$__64,__graphicsContextClip:$__62,__graphicsContextConcatTransform:$__60,__graphicsContextCreateBrush:$__58,__graphicsContextCreateFont:$__56,__graphicsContextDisableOffset:$__54,__graphicsContextDrawBitmap:$__67,__graphicsContextDrawEllipse:$__52,__graphicsContextDrawLines:$__50,__graphicsContextDrawPath:$__48,__graphicsContextDrawRectangle:$__66,__graphicsContextDrawRoundedRectangle:$__46,__graphicsContextDrawText:$__44,__graphicsContextEnableOffset:$__42,__graphicsContextEndLayer:$__40,__graphicsContextFillPath:$__38,__graphicsContextGetAntialiasMode:$__36,__graphicsContextGetCompositionMode:$__34,__graphicsContextGetInterpolationQuality:$__32,__graphicsContextGetTransform:$__30,__graphicsContextResetClip:$__28,__graphicsContextRotate:$__26,__graphicsContextScale:$__24,__graphicsContextSetAntialiasMode:$__22,__graphicsContextSetBrush:$__20,__graphicsContextSetCompositionMode:$__18,__graphicsContextSetFont:$__16,__graphicsContextSetInterpolationQuality:$__14,__graphicsContextSetPen:$__12,__graphicsContextSetTransform:$__10,__graphicsContextStrokeLine:$__8,__graphicsContextStrokePath:$__6,__graphicsContextTranslate:$__4,__graphicsContextClear:$__65,__graphicsContextTail:$tail};});
$Graphics.$UI.$WXCore.$GraphicsContext.$canvasGraphicsContext_27UNQ20=
 new _F_(function($dc,$tail,$super,$__,$self)
         {var $__6=
           new _A_($Graphics.$UI.$WXCore.$GraphicsContext.$__151__14NEW2,[$dc,$tail]);
          return new _A_($UHC.$Base.$return,[$__,$__6]);});
$Graphics.$UI.$WXCore.$GraphicsContext.$__151__243__0=
 new _F_(function($dc,$__,$__3)
         {return new _A_($Graphics.$UI.$WXCore.$GraphicsContext.$canvasGraphicsContext_27UNQ20,[$dc,$__,$__3,$UHC.$Base.$Monad__DCT74__339__0]);});
$Graphics.$UI.$WXCore.$GraphicsContext.$canvasGraphicsContext=
 new _F_(function($dc)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$GraphicsContext.$__151__243__0,[$dc]);
          return new _A_($LightOO.$Core.$extends,[$__,$Graphics.$UI.$WXCore.$GraphicsObject.$graphicsObject,$LightOO.$Core.$noOverride,$Graphics.$UI.$WXCore.$GraphicsObjectClass.$set__GraphicsObject__Tail]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ491=
 new _F_(function($self,$_24x,$_24x3)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$self,$Graphics.$UI.$WXCore.$WindowClass.$windowGetGraphicsContext]);
          var $__5=
           new _A_($UHC.$Base.$Just__,[$_24x3]);
          var $__6=
           new _A_($UHC.$IOBase.$writeIORef,[$_24x,$__5]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$__199__620NEW258=
 new _F_(function($self,$_24x,$_24x3)
         {var $__=
           new _A_($Data.$Maybe.$fromJust,[$_24x3]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$GraphicsContext.$canvasGraphicsContext,[$__]);
          var $__6=
           new _A_($LightOO.$Core.$new,[$Control.$Monad.$Fix.$MonadFix__DCT97__4__0]);
          var $__7=
           new _A_($UHC.$Base.$_24,[$__6,$__5]);
          var $__8=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ491,[$self,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__7,$__8]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ485=
 new _F_(function($self,$_24x,$_24x3)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__620NEW258,[$self,$_24x,$_24x3]);
          var $__5=
           new _A_($UHC.$Base.$packedStringToString,["failed to obtain 2d context"]);
          var $__6=
           new _A_($UHC.$Base.$error,[$__5]);
          var $__7=
           new _A_($Data.$Maybe.$isJust,[$_24x3]);
          var $__8=
           new _A_($UHC.$Base.$_24,[$UHC.$Base.$not,$__7]);
          var $__9=
           new _A_($Control.$Monad.$when,[$UHC.$Base.$Monad__DCT74__339__0,$__8,$__6]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__9,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$__199__606NEW253=
 new _F_(function($self,$_24x,$canvas)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$canvas,$Language.$UHC.$JS.$HTML5.$HTMLCanvasElement.$get2DContext]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ485,[$self,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__5]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ474=
 new _F_(function($self,$_24x,$canvas,$_24x4)
         {var $h=
           new _A_($Graphics.$UI.$WXCore.$Types.$sizeH,[$_24x4]);
          var $w=
           new _A_($Graphics.$UI.$WXCore.$Types.$sizeW,[$_24x4]);
          var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__606NEW253,[$self,$_24x,$canvas]);
          var $__8=
           new _A_($UHC.$Base.$packedStringToString,["height"]);
          var $__9=
           new _A_($Language.$UHC.$JS.$Prelude.$setAttr__,[$__8,$h,$canvas]);
          var $__10=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__9,$__]);
          var $__11=
           new _A_($UHC.$Base.$packedStringToString,["width"]);
          var $__12=
           new _A_($Language.$UHC.$JS.$Prelude.$setAttr__,[$__11,$w,$canvas]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__12,$__10]);});
$Graphics.$UI.$WXCore.$WebWindow.$__199__583NEW245=
 new _F_(function($self,$_24x,$mbCanvas)
         {var $canvas=
           new _A_($Data.$Maybe.$fromJust,[$mbCanvas]);
          var $__=
           new _A_($LightOO.$Core.$_23,[$self,$Graphics.$UI.$WXCore.$WindowClass.$windowGetSize]);
          var $__6=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ474,[$self,$_24x,$canvas]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__6]);});
$Graphics.$UI.$WXCore.$WebWindow.$__199__569NEW240=
 new _F_(function($self,$_24x,$_24x3)
         {var $mbCanvas=
           new _A_($Language.$UHC.$JS.$Prelude.$cast,[$Language.$UHC.$JS.$HTML5.$Types.$GetObjectRef__DCT99__1__0,$_24x3]);
          var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__583NEW245,[$self,$_24x,$mbCanvas]);
          var $__6=
           new _A_($UHC.$Base.$packedStringToString,["failed to cast to canvas element"]);
          var $__7=
           new _A_($UHC.$Base.$error,[$__6]);
          var $__8=
           new _A_($Data.$Maybe.$isJust,[$mbCanvas]);
          var $__9=
           new _A_($UHC.$Base.$_24,[$UHC.$Base.$not,$__8]);
          var $__10=
           new _A_($Control.$Monad.$when,[$UHC.$Base.$Monad__DCT74__339__0,$__9,$__7]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__10,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ463=
 new _F_(function($self,$_24x,$_24x3,$_24x4,$_24x5)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__569NEW240,[$self,$_24x,$_24x4]);
          var $__7=
           new _A_($LightOO.$Core.$_23,[$_24x3,$Language.$UHC.$JS.$HTML5.$Node.$insertBefore,$_24x4,$_24x5]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__7,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$__199__559NEW234=
 new _F_(function($self,$_24x,$_24x3,$_24x4)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$_24x3,$Language.$UHC.$JS.$HTML5.$Node.$firstChild]);
          var $__6=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ463,[$self,$_24x,$_24x3,$_24x4]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__6]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ453=
 new _F_(function($self,$_24x,$_24x3,$_24x4,$_24x5)
         {var $__=
           new _A_($UHC.$Base.$show,[$UHC.$Base.$Show__DCT74__128__0,$_24x5]);
          var $__7=
           new _A_($UHC.$Base.$packedStringToString,["c-"]);
          var $__8=
           new _A_($UHC.$Base.$_2b_2b,[$__7,$__]);
          var $cid=
           new _A_($Language.$UHC.$JS.$Marshal.$str,[$__8]);
          var $__10=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__559NEW234,[$self,$_24x,$_24x3,$_24x4]);
          var $__11=
           new _A_($UHC.$Base.$packedStringToString,["id"]);
          var $__12=
           new _A_($Language.$UHC.$JS.$Prelude.$setAttr__,[$__11,$cid,$_24x4]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__12,$__10]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ446=
 new _F_(function($self,$_24x,$_24x3,$_24x4,$_24x5)
         {var $__=
           new _A_($UHC.$IOBase.$readIORef,[$_24x]);
          var $__7=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ453,[$self,$_24x3,$_24x4,$_24x5]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__7]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ439=
 new _F_(function($self,$_24x,$_24x3,$_24x4,$_24x5)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["canvas"]);
          var $__7=
           new _A_($Language.$UHC.$JS.$Marshal.$str,[$__]);
          var $__8=
           new _A_($Language.$UHC.$JS.$HTML5.$HTMLDocument.$createElement,[$_24x5,$__7]);
          var $__9=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ446,[$self,$_24x,$_24x3,$_24x4]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__8,$__9]);});
$Graphics.$UI.$WXCore.$WebWindow.$contNEW222UNQ434=
 new _F_(function($self,$_24x,$_24x3,$_24x4)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ439,[$self,$_24x,$_24x3,$_24x4]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Language.$UHC.$JS.$HTML5.$HTMLDocument.$document,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$__197__4017__0NEW217UNQ432=
 new _F_(function($self,$_24x,$_24x3,$_24x4)
         {var $cont=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$contNEW222UNQ434,[$self,$_24x,$_24x3,$_24x4]);
          var $getDC=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$getDCNEW295UNQ436,[$_24x3,$cont]);
          return $getDC;});
$Graphics.$UI.$WXCore.$WebWindow.$__199__406NEW145=
 new _F_(function($p,$tail,$self,$_24x,$_24x5,$_24x6,$_24x7,$_24x8,$__,$_24x10,$onkeydownProp,$heightProp,$widthProp)
         {var $__14=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
          var $__15=
           new _A_($UHC.$Base.$const,[$__14]);
          var $__16=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
          var $__17=
           new _A_($UHC.$Base.$_24,[$__16,$__15]);
          var $__18=
           new _A_($UHC.$Base.$packedStringToString,["getonkeychar"]);
          var $__19=
           new _A_($Language.$UHC.$JS.$Marshal.$toJS,[$Language.$UHC.$JS.$Marshal.$ToJS__DCT81__13__0,$__18]);
          var $__20=
           new _A_($Language.$UHC.$JS.$Prelude.$__trace,[$__19]);
          var $__21=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__20,$__17]);
          var $__22=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__197__4017__0NEW217UNQ432,[$self,$_24x6,$_24x8,$_24x10]);
          var $__23=
           new _A_($Graphics.$UI.$WXCore.$Types.$sizeZero,[$UHC.$Base.$Num__DCT74__101__0]);
          var $__24=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__23]);
          var $__25=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$UHC.$Base.$True__]);
          var $__26=
           new _A_($UHC.$Base.$packedStringToString,["_windowEnable not implemented"]);
          var $__27=
           new _A_($UHC.$Base.$error,[$__26]);
          var $__28=
           new _A_($UHC.$Base.$packedStringToString,["_windowDisable not implemented"]);
          var $__29=
           new _A_($UHC.$Base.$error,[$__28]);
          var $__30=
           new _A_($UHC.$Base.$packedStringToString,["_windowIsShownOnScreen not implemented"]);
          var $__31=
           new _A_($UHC.$Base.$error,[$__30]);
          var $__32=
           new _A_($UHC.$Base.$packedStringToString,["_windowIsShown not implemented"]);
          var $__33=
           new _A_($UHC.$Base.$error,[$__32]);
          var $__34=
           new _A_($UHC.$Base.$packedStringToString,["_windowIsExposed not implemented"]);
          var $__35=
           new _A_($UHC.$Base.$error,[$__34]);
          var $__36=
           new _A_($UHC.$Base.$packedStringToString,["_windowIsEnabled not implemented"]);
          var $__37=
           new _A_($UHC.$Base.$error,[$__36]);
          var $__38=
           new _A_($UHC.$Base.$packedStringToString,["_windowHide not implemented"]);
          var $__39=
           new _A_($UHC.$Base.$error,[$__38]);
          var $__40=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$UHC.$Base.$False__]);
          var $__41=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__197__4029__0NEW343UNQ550,[$_24x6]);
          var $__42=
           new _A_($UHC.$Base.$packedStringToString,["_windowClose not implemented"]);
          var $__43=
           new _A_($UHC.$Base.$error,[$__42]);
          var $__44=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$p]);
          var $__45=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$withSiblingsUNQ155,[$self]);
          var $__46=
           new _A_($UHC.$Base.$_24,[$__45,$Graphics.$UI.$WXCore.$WebWindow.$__199__867__0]);
          var $__47=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$withSiblingsUNQ155,[$self]);
          var $__48=
           new _A_($UHC.$Base.$_24,[$__47,$Graphics.$UI.$WXCore.$WebWindow.$__199__884__0]);
          var $__49=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__197__4036__0NEW392UNQ628,[$self]);
          var $__50=
           new _A_($UHC.$IOBase.$readIORef,[$_24x]);
          var $__51=
           new _A_($UHC.$Base.$packedStringToString,["_windowFindWindowByName not implemented"]);
          var $__52=
           new _A_($UHC.$Base.$error,[$__51]);
          var $__53=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$findWindow_27UNQ153,[$self]);
          var $__54=
           new _A_($UHC.$Base.$mapM__,[$UHC.$Base.$Monad__DCT74__339__0,$Graphics.$UI.$WXCore.$WindowClass.$windowDestroy]);
          var $__55=
           new _A_($UHC.$IOBase.$readIORef,[$_24x]);
          var $__56=
           new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__55,$__54]);
          var $__57=
           new _A_($UHC.$Base.$packedStringToString,["Window"]);
          var $__58=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__57]);
          var $__59=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__197__4044__0NEW424UNQ682,[$heightProp,$widthProp]);
          var $__60=
           new _A_($UHC.$IOBase.$readIORef,[$_24x7]);
          var $__61=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__197__4047__0NEW442UNQ701,[$self]);
          var $__62=
           new _A_($UHC.$IOBase.$readIORef,[$_24x5]);
          var $__63=
           new _A_($UHC.$IOBase.$readIORef,[$_24x6]);
          var $__64=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__197__4016__0UNQ382,[$self,$__,$onkeydownProp]);
          var $__65=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__197__4019__0UNQ502,[$self]);
          var $__66=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__197__4031__0UNQ559,[$self]);
          var $__67=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__197__4032__0UNQ571,[$self]);
          var $__68=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__197__4037__0UNQ635,[$_24x]);
          var $__69=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__197__4042__0UNQ678,[$_24x]);
          var $__70=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__197__4045__0UNQ696,[$heightProp,$widthProp]);
          return {_tag_:0,__windowGetId:$__63,__windowSetId:$UHC.$Base.$undefined,__windowGetStyle:$__62,__windowGetRect:$__61,__windowGetPosition:$__60,__windowGetSize:$__59,__windowSetSize:$__70,__windowAddChild:$__69,__windowDestroyChildren:$__56,__windowFindWindow:$__53,__windowFindWindowByName:$__52,__windowGetChildren:$__50,__windowRemoveChild:$__68,__windowGetGrandParent:$__49,__windowGetNextSibling:$__48,__windowGetParent:$__44,__windowGetPrevSibling:$__46,__windowIsDescendant:$__67,__windowReparent:$__66,__windowClose:$__43,__windowDestroy:$__41,__windowIsBeingDeleted:$__40,__windowHide:$__39,__windowIsEnabled:$__37,__windowIsExposed:$__35,__windowIsShown:$__33,__windowIsShownOnScreen:$__31,__windowDisable:$__29,__windowEnable:$__27,__windowShow:$__25,__windowRefresh:$__65,__windowGetClientSize:$__24,__windowGetLabel:$__58,__windowGetGraphicsContext:$__22,__windowOnKeyChar:$__64,__windowGetOnKeyChar:$__21,__windowTail:$tail};});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ346=
 new _F_(function($p,$tail,$self,$_24x,$_24x5,$_24x6,$_24x7,$_24x8,$_24x9,$cssRef,$__,$_24x12,$_24x13)
         {var $__14=
           new _A_($UHC.$Base.$packedStringToString,["width"]);
          var $__15=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$settUNQ305,[$__14,$cssRef]);
          var $__16=
           new _A_($UHC.$Base.$packedStringToString,["width"]);
          var $__17=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$gettUNQ307,[$__16,$cssRef]);
          var $widthProp=
           new _A_($Language.$UHC.$JS.$JSRef.$newJSRef,[$__17,$__15]);
          var $__19=
           new _A_($UHC.$Base.$packedStringToString,["height"]);
          var $__20=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$settUNQ305,[$__19,$cssRef]);
          var $__21=
           new _A_($UHC.$Base.$packedStringToString,["height"]);
          var $__22=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$gettUNQ307,[$__21,$cssRef]);
          var $heightProp=
           new _A_($Language.$UHC.$JS.$JSRef.$newJSRef,[$__22,$__20]);
          var $onkeydownProp=
           new _A_($Language.$UHC.$JS.$HTML5.$HTMLElement.$onkeydown,[$_24x13]);
          var $__25=
           new _A_($UHC.$Base.$packedStringToString,["id"]);
          var $__26=
           new _A_($Language.$UHC.$JS.$Prelude.$getAttr,[$__25,$_24x12]);
          var $__27=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__393__0,[$_24x12]);
          var $idProp=
           new _A_($Language.$UHC.$JS.$JSRef.$newJSRef,[$__26,$__27]);
          var $__29=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__406NEW145,[$p,$tail,$self,$_24x,$_24x5,$_24x7,$_24x8,$_24x9,$__,$_24x12,$onkeydownProp,$heightProp,$widthProp]);
          var $__30=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__29]);
          var $__31=
           new _A_($Language.$UHC.$JS.$JSRef.$writeJSRef,[$idProp,$_24x6]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__31,$__30]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ278=
 new _F_(function($p,$tail,$self,$_24x,$_24x5,$_24x6,$_24x7,$_24x8,$_24x9,$_24x10)
         {var $__=
           new _A_($LightOO.$Core.$Sup__DCT69__26__0,[$LightOO.$Core.$Sup__DCT69__22__0,$Graphics.$UI.$WXCore.$KeyEventClass.$Widen__DCT93__2__0]);
          var $__12=
           new _A_($UHC.$Base.$packedStringToString,["style"]);
          var $__13=
           new _A_($Language.$UHC.$JS.$Prelude.$getAttr,[$__12,$_24x10]);
          var $cssRef=
           new _A_($Language.$UHC.$JS.$JSRef.$newReadOnlyJSRef,[$__13]);
          var $__15=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ346,[$p,$tail,$self,$_24x,$_24x5,$_24x6,$_24x7,$_24x8,$_24x9,$cssRef,$__,$_24x10]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Language.$UHC.$JS.$HTML5.$Window.$window,$__15]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ270=
 new _F_(function($p,$tail,$self,$_24x,$_24x5,$_24x6,$_24x7,$_24x8,$_24x9)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ278,[$p,$tail,$self,$_24x,$_24x5,$_24x6,$_24x7,$_24x8,$_24x9]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Graphics.$UI.$WXCore.$WebWindow.$initializeUNQ275,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ264=
 new _F_(function($p,$tail,$self,$_24x,$_24x5,$_24x6,$_24x7,$_24x8)
         {var $__=
           new _A_($UHC.$IOBase.$newIORef,[$UHC.$Base.$Nothing__]);
          var $__10=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ270,[$p,$tail,$self,$_24x,$_24x5,$_24x6,$_24x7,$_24x8]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__10]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ258=
 new _F_(function($p,$pos,$tail,$self,$_24x,$_24x6,$_24x7,$_24x8)
         {var $__=
           new _A_($UHC.$IOBase.$newIORef,[$pos]);
          var $__10=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ264,[$p,$tail,$self,$_24x,$_24x6,$_24x7,$_24x8]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__10]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ252=
 new _F_(function($p,$pos,$tail,$self,$_24x,$_24x6,$_24x7)
         {var $__=
           new _A_($UHC.$IOBase.$newIORef,[$_24x7]);
          var $__9=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ258,[$p,$pos,$tail,$self,$_24x,$_24x6,$_24x7]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__9]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ246=
 new _F_(function($p,$ident,$pos,$tail,$self,$_24x,$_24x7)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__1114NEW456,[$ident]);
          var $__9=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ252,[$p,$pos,$tail,$self,$_24x,$_24x7]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__9]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ237=
 new _F_(function($p,$ident,$pos,$style_27,$tail,$self,$_24x)
         {var $__=
           new _A_($UHC.$IOBase.$newIORef,[$style_27]);
          var $__9=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ246,[$p,$ident,$pos,$tail,$self,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__9]);});
$Graphics.$UI.$WXCore.$WebWindow.$wrapperUNQ157=
 new _F_(function($p,$ident,$pos,$style_27,$tail,$super,$self)
         {var $__=
           new _A_($UHC.$IOBase.$newIORef,[$UHC.$Base.$_5b_5d]);
          var $__9=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ237,[$p,$ident,$pos,$style_27,$tail,$self]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__9]);});
$Graphics.$UI.$WXCore.$WebWindow.$window_27=
 new _F_(function($p,$ident,$pos,$size,$style_27)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$wrapperUNQ157,[$p,$ident,$pos,$style_27]);
          return new _A_($LightOO.$Core.$extends,[$__,$Graphics.$UI.$WXCore.$EvtHandler.$evthandler,$LightOO.$Core.$noOverride,$Graphics.$UI.$WXCore.$EvtHandlerClass.$set__EvtHandler__Tail]);});
$Graphics.$UI.$WXCore.$WindowClass.$__windowAddChild=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__windowAddChild;});
$Graphics.$UI.$WXCore.$WindowClass.$windowAddChild=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$WindowClass.$__windowAddChild,$Graphics.$UI.$WXCore.$WindowClass.$window__Methods]);}),[]);
$Graphics.$UI.$WXCore.$WebWindow.$__199__1159__0=
 new _F_(function($_24x,$p)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$p,$Graphics.$UI.$WXCore.$WindowClass.$windowAddChild]);
          return new _A_($UHC.$Base.$_24,[$__,$_24x]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ730=
 new _F_(function($p,$_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$_24x]);
          var $__4=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__1159__0,[$_24x]);
          var $__6=
           new _A_($UHC.$Base.$maybe,[$__4,$__5,$p]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$window=
 new _F_(function($p,$id,$rect,$style,$tail,$self)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Types.$rectSize,[$UHC.$Base.$Num__DCT74__101__0,$rect]);
          var $__8=
           new _A_($Graphics.$UI.$WXCore.$Types.$rectTopLeft,[$UHC.$Base.$Num__DCT74__101__0,$rect]);
          var $__9=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$window_27,[$p,$id,$__8,$__,$style,$tail,$self]);
          var $__10=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ730,[$p]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__9,$__10]);});
$Graphics.$UI.$WXCore.$WebWindow.$windowCreate=
 new _F_(function($p,$id,$rect,$style)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$window,[$p,$id,$rect,$style]);
          var $__6=
           new _A_($LightOO.$Core.$new,[$Control.$Monad.$Fix.$MonadFix__DCT97__4__0]);
          return new _A_($UHC.$Base.$_24,[$__6,$__]);});
$Graphics.$UI.$WX.$Window.$__229__507__0=
 new _F_(function($parent,$id,$rect,$props,$flags)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$windowCreate,[$parent,$id,$rect,$flags]);
          var $__7=
           new _A_($Graphics.$UI.$WX.$Window.$_24okUNQ144,[$props]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__7]);});
$Graphics.$UI.$WX.$Classes.$Identity__CLS207__29__0=
 new _F_(function($Identity__)
         {var $Identity__2=
           {_tag_:0,_1:$UHC.$Base.$undefined};
          return $Identity__2;});
$Graphics.$UI.$WX.$Window.$Identity__NEW42UNQ182EVLDCT225__28__0RDC=
 new _F_(function($Identity__,$Identity__2)
         {var $Identity__3=
           _e_(new _A_($Graphics.$UI.$WX.$Classes.$Identity__CLS207__29__0,[$Identity__]));
          var $__5=
           {_tag_:0,_1:$Identity__2};
          return $__5;});
$Graphics.$UI.$WX.$Window.$Identity__NEW39UNQ180DCT225__28__0RDC=
 new _F_(function($Identity__,$Identity__2)
         {var $Identity__3=
           new _A_($Graphics.$UI.$WX.$Window.$Identity__NEW42UNQ182EVLDCT225__28__0RDC,[$Identity__,$Identity__2]);
          return $Identity__3;});
$Graphics.$UI.$WX.$Window.$__229__69=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["identity"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__windowGetId=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__windowGetId;});
$Graphics.$UI.$WXCore.$WindowClass.$windowGetId=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$WindowClass.$__windowGetId,$Graphics.$UI.$WXCore.$WindowClass.$window__Methods]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__windowSetId=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__windowSetId;});
$Graphics.$UI.$WXCore.$WindowClass.$windowSetId=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$WindowClass.$__windowSetId,$Graphics.$UI.$WXCore.$WindowClass.$window__Methods]);}),[]);
$Graphics.$UI.$WX.$Window.$Identity__DCT225__28__0DFLGraphics_2eUI_2eWX_2eClasses_2eidentity=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Attributes.$reflectiveAttr,[$Data.$Typeable.$Typeable__DCT320__46__0,$Graphics.$UI.$WX.$Window.$__229__69,$Graphics.$UI.$WXCore.$WindowClass.$windowGetId,$Graphics.$UI.$WXCore.$WindowClass.$windowSetId]);}),[]);
$Graphics.$UI.$WX.$Window.$Identity__UNQ180DCT225__28__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Window.$Identity__NEW39UNQ180DCT225__28__0RDC,[$Graphics.$UI.$WX.$Window.$Identity__UNQ180DCT225__28__0RDC,$Graphics.$UI.$WX.$Window.$Identity__DCT225__28__0DFLGraphics_2eUI_2eWX_2eClasses_2eidentity]);}),[]);
$Graphics.$UI.$WX.$Window.$Identity__DCT225__28__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WX.$Window.$Identity__UNQ180DCT225__28__0RDC;}),[]);
$Graphics.$UI.$WX.$Classes.$identity=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$Graphics.$UI.$WX.$Window.$initialIdentity=
 new _F_(function($__)
         {var $__2=
           new _A_($Graphics.$UI.$WX.$Classes.$identity,[$__]);
          return new _A_($Graphics.$UI.$WX.$Attributes.$withProperty,[$Data.$Typeable.$Typeable__DCT320__46__0,$__2,$Graphics.$UI.$WXCore.$Types.$idAny]);});
$Graphics.$UI.$WXCore.$Types.$_24D__Rect2DDFLUHC_2eBase_2emoduleNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Graphics.UI.WXCore.Types"]);});
$Graphics.$UI.$WXCore.$Types.$_24D__Rect2DDFLUHC_2eBase_2edatatypeNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Rect2D"]);});
$Graphics.$UI.$WXCore.$Types.$_24D__Rect2DNEW132UNQ696EVLSDCGENDatatype=
 new _F_(function($_24D__Rect2D)
         {var $Datatype__=
           _e_(new _A_($UHC.$Base.$Datatype__CLS74__350__0,[$_24D__Rect2D]));
          var $__5=
           {_tag_:0,_1:$Graphics.$UI.$WXCore.$Types.$_24D__Rect2DDFLUHC_2eBase_2edatatypeNameGENDatatype,_2:$Graphics.$UI.$WXCore.$Types.$_24D__Rect2DDFLUHC_2eBase_2emoduleNameGENDatatype};
          return $__5;});
$Graphics.$UI.$WXCore.$Types.$_24D__Rect2DNEW130UNQ695SDCGENDatatype=
 new _F_(function($_24D__Rect2D)
         {var $_24D__Rect2D2=
           new _A_($Graphics.$UI.$WXCore.$Types.$_24D__Rect2DNEW132UNQ696EVLSDCGENDatatype,[$_24D__Rect2D]);
          return $_24D__Rect2D2;});
$Graphics.$UI.$WXCore.$Types.$_24D__Rect2DUNQ695SDCGENDatatype=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$Types.$_24D__Rect2DNEW130UNQ695SDCGENDatatype,[$Graphics.$UI.$WXCore.$Types.$_24D__Rect2DUNQ695SDCGENDatatype]);}),[]);
$Graphics.$UI.$WXCore.$Types.$_24D__Rect2DGENDatatype=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$Types.$_24D__Rect2DUNQ695SDCGENDatatype;}),[]);
$Graphics.$UI.$WXCore.$Types.$__Rep0Rect2DDFLUHC_2eBase_2eto0GENRepresentable0=
 new _F_(function($proj__1)
         {var $proj__3=
           _e_($proj__1);
          var $proj__45=
           _e_($proj__3._1);
          var $proj__98=
           _e_($proj__3._2);
          var $__=
           new _A_($Graphics.$UI.$WXCore.$Types.$Rect__,[$proj__45._1,$proj__45._2,$proj__98._1,$proj__98._2]);
          return $__;});
$Graphics.$UI.$WXCore.$Types.$__Rep0Rect2DDFLUHC_2eBase_2efrom0GENRepresentable0=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          var $__7=
           new _A_($UHC.$Base.$K1__,[$x2.rectHeight]);
          var $__8=
           new _A_($UHC.$Base.$M1__,[$__7]);
          var $__9=
           new _A_($UHC.$Base.$K1__,[$x2.rectWidth]);
          var $__10=
           new _A_($UHC.$Base.$M1__,[$__9]);
          var $__11=
           new _A_($UHC.$Base.$_3a_2a_3a,[$__10,$__8]);
          var $__12=
           new _A_($UHC.$Base.$K1__,[$x2.rectTop]);
          var $__13=
           new _A_($UHC.$Base.$M1__,[$__12]);
          var $__14=
           new _A_($UHC.$Base.$K1__,[$x2.rectLeft]);
          var $__15=
           new _A_($UHC.$Base.$M1__,[$__14]);
          var $__16=
           new _A_($UHC.$Base.$_3a_2a_3a,[$__15,$__13]);
          var $__17=
           new _A_($UHC.$Base.$_3a_2a_3a,[$__16,$__11]);
          var $__18=
           new _A_($UHC.$Base.$M1__,[$__17]);
          var $__19=
           new _A_($UHC.$Base.$M1__,[$__18]);
          return $__19;});
$Graphics.$UI.$WXCore.$Types.$__Rep0Rect2DNEW896UNQ614EVLSDCGENRepresentable0=
 new _F_(function($__)
         {var $Representable0__=
           _e_(new _A_($UHC.$Base.$Representable0__CLS74__369__0,[$__]));
          var $__5=
           {_tag_:0,_1:$Graphics.$UI.$WXCore.$Types.$__Rep0Rect2DDFLUHC_2eBase_2efrom0GENRepresentable0,_2:$Graphics.$UI.$WXCore.$Types.$__Rep0Rect2DDFLUHC_2eBase_2eto0GENRepresentable0};
          return $__5;});
$Graphics.$UI.$WXCore.$Types.$__Rep0Rect2DNEW894UNQ613SDCGENRepresentable0=
 new _F_(function($__)
         {var $__2=
           new _A_($Graphics.$UI.$WXCore.$Types.$__Rep0Rect2DNEW896UNQ614EVLSDCGENRepresentable0,[$__]);
          return $__2;});
$Graphics.$UI.$WXCore.$Types.$__Rep0Rect2DUNQ613SDCGENRepresentable0=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$Types.$__Rep0Rect2DNEW894UNQ613SDCGENRepresentable0,[$Graphics.$UI.$WXCore.$Types.$__Rep0Rect2DUNQ613SDCGENRepresentable0]);}),[]);
$Graphics.$UI.$WXCore.$Types.$__Rep0Rect2DGENRepresentable0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$Types.$__Rep0Rect2DUNQ613SDCGENRepresentable0;}),[]);
$Graphics.$UI.$WXCore.$Types.$__51__27__0NEW906UNQ2372EVLRDC=
 new _F_(function($__,$__2)
         {var $Typeable__=
           _e_(new _A_($Data.$Typeable.$Typeable__CLS320__7__0,[$__]));
          var $__5=
           {_tag_:0,_1:$__2};
          return $__5;});
$Graphics.$UI.$WXCore.$Types.$__51__27__0NEW903UNQ2368RDC=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($Graphics.$UI.$WXCore.$Types.$__51__27__0NEW906UNQ2372EVLRDC,[$__,$__2]);
          return $__3;});
$Graphics.$UI.$WXCore.$Types.$__51__27__0=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$Typeable.$Typeable0_27__DCT320__23__0,[$Graphics.$UI.$WXCore.$Types.$_24D__Rect2DGENDatatype]);
          var $__51__27__0DFLData_2eTypeable_2etypeOf=
           new _A_($Data.$Typeable.$typeOf0default,[$Graphics.$UI.$WXCore.$Types.$__Rep0Rect2DGENRepresentable0,$__2,$UHC.$Base.$undefined]);
          var $__3=
           _i_();
          _i_set_($__3,new _A_($Graphics.$UI.$WXCore.$Types.$__51__27__0NEW903UNQ2368RDC,[$__3,$__51__27__0DFLData_2eTypeable_2etypeOf]));
          return $__3;});
$Graphics.$UI.$WX.$Window.$__227__118__2__0=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$Types.$__51__27__0,[$Data.$Typeable.$Typeable__DCT320__46__0]);}),[]);
$Graphics.$UI.$WXCore.$Types.$sizeZero=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__3=
           new _A_($UHC.$Base.$fromInteger,[$__,$__2]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__5=
           new _A_($UHC.$Base.$fromInteger,[$__,$__4]);
          return new _A_($Graphics.$UI.$WXCore.$Types.$Size__,[$__5,$__3]);});
$Graphics.$UI.$WX.$Attributes.$findProperty=
 new _F_(function($__,$attr,$def,$props)
         {var $__5=
           new _A_($Graphics.$UI.$WX.$Attributes.$filterProperty,[$__,$attr,$props]);
          var $__6=
           _e_($__5);
          var $__9=
           _e_($__6[0]);
          var $__swJSW217__0;
          switch($__9._tag_)
           {case 0:
             var $__11=
              new _A_($__9._1,[$def]);
             var $__12=
              [$__11,$__6[1]];
             var $__13=
              new _A_($UHC.$Base.$Just__,[$__12]);
             $__swJSW217__0=
              $__13;
             break;
            case 1:
             $__swJSW217__0=
              $UHC.$Base.$Nothing__;
             break;
            case 2:
             var $__15=
              [$__9._1,$__6[1]];
             var $__16=
              new _A_($UHC.$Base.$Just__,[$__15]);
             $__swJSW217__0=
              $__16;
             break;}
          return $__swJSW217__0;});
$Graphics.$UI.$WXCore.$Types.$__51__8__0NEW1302UNQ2688EVLRDC=
 new _F_(function($__,$__2)
         {var $Typeable__=
           _e_(new _A_($Data.$Typeable.$Typeable__CLS320__7__0,[$__2]));
          var $__5=
           {_tag_:0,_1:$__};
          return $__5;});
$Graphics.$UI.$WXCore.$Types.$__51__8__0NEW1299UNQ2684RDC=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($Graphics.$UI.$WXCore.$Types.$__51__8__0NEW1302UNQ2688EVLRDC,[$__,$__2]);
          return $__3;});
$Graphics.$UI.$WXCore.$Types.$__Rep0Point2DFLUHC_2eBase_2eto0GENRepresentable0=
 new _F_(function($proj__1)
         {var $proj__3=
           _e_($proj__1);
          var $__=
           new _A_($Graphics.$UI.$WXCore.$Types.$Point__,[$proj__3._1,$proj__3._2]);
          return $__;});
$Graphics.$UI.$WXCore.$Types.$__Rep0Point2DFLUHC_2eBase_2efrom0GENRepresentable0=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          var $__5=
           new _A_($UHC.$Base.$K1__,[$x2.pointY]);
          var $__6=
           new _A_($UHC.$Base.$M1__,[$__5]);
          var $__7=
           new _A_($UHC.$Base.$K1__,[$x2.pointX]);
          var $__8=
           new _A_($UHC.$Base.$M1__,[$__7]);
          var $__9=
           new _A_($UHC.$Base.$_3a_2a_3a,[$__8,$__6]);
          var $__10=
           new _A_($UHC.$Base.$M1__,[$__9]);
          var $__11=
           new _A_($UHC.$Base.$M1__,[$__10]);
          return $__11;});
$Graphics.$UI.$WXCore.$Types.$__Rep0Point2NEW409UNQ801EVLSDCGENRepresentable0=
 new _F_(function($__)
         {var $Representable0__=
           _e_(new _A_($UHC.$Base.$Representable0__CLS74__369__0,[$__]));
          var $__5=
           {_tag_:0,_1:$Graphics.$UI.$WXCore.$Types.$__Rep0Point2DFLUHC_2eBase_2efrom0GENRepresentable0,_2:$Graphics.$UI.$WXCore.$Types.$__Rep0Point2DFLUHC_2eBase_2eto0GENRepresentable0};
          return $__5;});
$Graphics.$UI.$WXCore.$Types.$__Rep0Point2NEW407UNQ800SDCGENRepresentable0=
 new _F_(function($__)
         {var $__2=
           new _A_($Graphics.$UI.$WXCore.$Types.$__Rep0Point2NEW409UNQ801EVLSDCGENRepresentable0,[$__]);
          return $__2;});
$Graphics.$UI.$WXCore.$Types.$__Rep0Point2UNQ800SDCGENRepresentable0=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$Types.$__Rep0Point2NEW407UNQ800SDCGENRepresentable0,[$Graphics.$UI.$WXCore.$Types.$__Rep0Point2UNQ800SDCGENRepresentable0]);}),[]);
$Graphics.$UI.$WXCore.$Types.$__Rep0Point2GENRepresentable0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$Types.$__Rep0Point2UNQ800SDCGENRepresentable0;}),[]);
$Graphics.$UI.$WXCore.$Types.$_24D__Point2DFLUHC_2eBase_2edatatypeNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Point2"]);});
$Graphics.$UI.$WXCore.$Types.$_24D__Point2DFLUHC_2eBase_2emoduleNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Graphics.UI.WXCore.Types"]);});
$Graphics.$UI.$WXCore.$Types.$_24D__Point2NEW1292UNQ851EVLSDCGENDatatype=
 new _F_(function($_24D__Point2)
         {var $Datatype__=
           _e_(new _A_($UHC.$Base.$Datatype__CLS74__350__0,[$_24D__Point2]));
          var $__5=
           {_tag_:0,_1:$Graphics.$UI.$WXCore.$Types.$_24D__Point2DFLUHC_2eBase_2edatatypeNameGENDatatype,_2:$Graphics.$UI.$WXCore.$Types.$_24D__Point2DFLUHC_2eBase_2emoduleNameGENDatatype};
          return $__5;});
$Graphics.$UI.$WXCore.$Types.$_24D__Point2NEW1290UNQ850SDCGENDatatype=
 new _F_(function($_24D__Point2)
         {var $_24D__Point22=
           new _A_($Graphics.$UI.$WXCore.$Types.$_24D__Point2NEW1292UNQ851EVLSDCGENDatatype,[$_24D__Point2]);
          return $_24D__Point22;});
$Graphics.$UI.$WXCore.$Types.$_24D__Point2UNQ850SDCGENDatatype=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$Types.$_24D__Point2NEW1290UNQ850SDCGENDatatype,[$Graphics.$UI.$WXCore.$Types.$_24D__Point2UNQ850SDCGENDatatype]);}),[]);
$Graphics.$UI.$WXCore.$Types.$_24D__Point2GENDatatype=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$Types.$_24D__Point2UNQ850SDCGENDatatype;}),[]);
$Graphics.$UI.$WXCore.$Types.$__51__8__0=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$Typeable.$Typeable0_27__DCT320__23__0,[$Graphics.$UI.$WXCore.$Types.$_24D__Point2GENDatatype]);
          var $__51__8__0DFLData_2eTypeable_2etypeOf=
           new _A_($Data.$Typeable.$typeOf0default,[$Graphics.$UI.$WXCore.$Types.$__Rep0Point2GENRepresentable0,$__2,$UHC.$Base.$undefined]);
          var $__3=
           _i_();
          _i_set_($__3,new _A_($Graphics.$UI.$WXCore.$Types.$__51__8__0NEW1299UNQ2684RDC,[$__51__8__0DFLData_2eTypeable_2etypeOf,$__3]));
          return $__3;});
$Graphics.$UI.$WX.$Window.$__227__177__2__0=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$Types.$__51__8__0,[$Data.$Typeable.$Typeable__DCT320__46__0]);}),[]);
$Graphics.$UI.$WXCore.$Types.$__Rep0Size2DDFLUHC_2eBase_2eto0GENRepresentable0=
 new _F_(function($proj__1)
         {var $proj__3=
           _e_($proj__1);
          var $__=
           new _A_($Graphics.$UI.$WXCore.$Types.$Size__,[$proj__3._1,$proj__3._2]);
          return $__;});
$Graphics.$UI.$WXCore.$Types.$__Rep0Size2DDFLUHC_2eBase_2efrom0GENRepresentable0=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          var $__5=
           new _A_($UHC.$Base.$K1__,[$x2.sizeH]);
          var $__6=
           new _A_($UHC.$Base.$M1__,[$__5]);
          var $__7=
           new _A_($UHC.$Base.$K1__,[$x2.sizeW]);
          var $__8=
           new _A_($UHC.$Base.$M1__,[$__7]);
          var $__9=
           new _A_($UHC.$Base.$_3a_2a_3a,[$__8,$__6]);
          var $__10=
           new _A_($UHC.$Base.$M1__,[$__9]);
          var $__11=
           new _A_($UHC.$Base.$M1__,[$__10]);
          return $__11;});
$Graphics.$UI.$WXCore.$Types.$__Rep0Size2DNEW299UNQ440EVLSDCGENRepresentable0=
 new _F_(function($__)
         {var $Representable0__=
           _e_(new _A_($UHC.$Base.$Representable0__CLS74__369__0,[$__]));
          var $__5=
           {_tag_:0,_1:$Graphics.$UI.$WXCore.$Types.$__Rep0Size2DDFLUHC_2eBase_2efrom0GENRepresentable0,_2:$Graphics.$UI.$WXCore.$Types.$__Rep0Size2DDFLUHC_2eBase_2eto0GENRepresentable0};
          return $__5;});
$Graphics.$UI.$WXCore.$Types.$__Rep0Size2DNEW297UNQ439SDCGENRepresentable0=
 new _F_(function($__)
         {var $__2=
           new _A_($Graphics.$UI.$WXCore.$Types.$__Rep0Size2DNEW299UNQ440EVLSDCGENRepresentable0,[$__]);
          return $__2;});
$Graphics.$UI.$WXCore.$Types.$__Rep0Size2DUNQ439SDCGENRepresentable0=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$Types.$__Rep0Size2DNEW297UNQ439SDCGENRepresentable0,[$Graphics.$UI.$WXCore.$Types.$__Rep0Size2DUNQ439SDCGENRepresentable0]);}),[]);
$Graphics.$UI.$WXCore.$Types.$__Rep0Size2DGENRepresentable0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$Types.$__Rep0Size2DUNQ439SDCGENRepresentable0;}),[]);
$Graphics.$UI.$WXCore.$Types.$_24D__Size2DDFLUHC_2eBase_2edatatypeNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Size2D"]);});
$Graphics.$UI.$WXCore.$Types.$_24D__Size2DDFLUHC_2eBase_2emoduleNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Graphics.UI.WXCore.Types"]);});
$Graphics.$UI.$WXCore.$Types.$_24D__Size2DNEW1200UNQ490EVLSDCGENDatatype=
 new _F_(function($_24D__Size2D)
         {var $Datatype__=
           _e_(new _A_($UHC.$Base.$Datatype__CLS74__350__0,[$_24D__Size2D]));
          var $__5=
           {_tag_:0,_1:$Graphics.$UI.$WXCore.$Types.$_24D__Size2DDFLUHC_2eBase_2edatatypeNameGENDatatype,_2:$Graphics.$UI.$WXCore.$Types.$_24D__Size2DDFLUHC_2eBase_2emoduleNameGENDatatype};
          return $__5;});
$Graphics.$UI.$WXCore.$Types.$_24D__Size2DNEW1198UNQ489SDCGENDatatype=
 new _F_(function($_24D__Size2D)
         {var $_24D__Size2D2=
           new _A_($Graphics.$UI.$WXCore.$Types.$_24D__Size2DNEW1200UNQ490EVLSDCGENDatatype,[$_24D__Size2D]);
          return $_24D__Size2D2;});
$Graphics.$UI.$WXCore.$Types.$_24D__Size2DUNQ489SDCGENDatatype=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$Types.$_24D__Size2DNEW1198UNQ489SDCGENDatatype,[$Graphics.$UI.$WXCore.$Types.$_24D__Size2DUNQ489SDCGENDatatype]);}),[]);
$Graphics.$UI.$WXCore.$Types.$_24D__Size2DGENDatatype=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$Types.$_24D__Size2DUNQ489SDCGENDatatype;}),[]);
$Graphics.$UI.$WXCore.$Types.$__51__17__0NEW1210UNQ2595EVLRDC=
 new _F_(function($__,$__2)
         {var $Typeable__=
           _e_(new _A_($Data.$Typeable.$Typeable__CLS320__7__0,[$__]));
          var $__5=
           {_tag_:0,_1:$__2};
          return $__5;});
$Graphics.$UI.$WXCore.$Types.$__51__17__0NEW1207UNQ2591RDC=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($Graphics.$UI.$WXCore.$Types.$__51__17__0NEW1210UNQ2595EVLRDC,[$__,$__2]);
          return $__3;});
$Graphics.$UI.$WXCore.$Types.$__51__17__0=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$Typeable.$Typeable0_27__DCT320__23__0,[$Graphics.$UI.$WXCore.$Types.$_24D__Size2DGENDatatype]);
          var $__51__17__0DFLData_2eTypeable_2etypeOf=
           new _A_($Data.$Typeable.$typeOf0default,[$Graphics.$UI.$WXCore.$Types.$__Rep0Size2DGENRepresentable0,$__2,$UHC.$Base.$undefined]);
          var $__3=
           _i_();
          _i_set_($__3,new _A_($Graphics.$UI.$WXCore.$Types.$__51__17__0NEW1207UNQ2591RDC,[$__3,$__51__17__0DFLData_2eTypeable_2etypeOf]));
          return $__3;});
$Graphics.$UI.$WX.$Window.$__227__217__2__0=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$Types.$__51__17__0,[$Data.$Typeable.$Typeable__DCT320__46__0]);}),[]);
$Graphics.$UI.$WX.$Window.$initialArea=
 new _F_(function($__,$cont,$props)
         {var $__4=
           new _A_($Graphics.$UI.$WXCore.$Types.$rectZero,[$UHC.$Base.$Num__DCT74__101__0]);
          var $__5=
           new _A_($Graphics.$UI.$WX.$Classes.$area,[$__]);
          var $__6=
           new _A_($Graphics.$UI.$WX.$Attributes.$findProperty,[$Graphics.$UI.$WX.$Window.$__227__118__2__0,$__5,$__4,$props]);
          var $__7=
           _e_($__6);
          var $__swJSW228__0;
          switch($__7._tag_)
           {case 0:
             var $__9=
              _e_($__7._1);
             var $__12=
              new _A_($cont,[$__9[0],$__9[1]]);
             $__swJSW228__0=
              $__12;
             break;
            case 1:
             var $__13=
              new _A_($Graphics.$UI.$WXCore.$Types.$pointZero,[$UHC.$Base.$Num__DCT74__101__0]);
             var $__14=
              new _A_($Graphics.$UI.$WX.$Classes.$position,[$__]);
             var $__15=
              new _A_($Graphics.$UI.$WX.$Attributes.$findProperty,[$Graphics.$UI.$WX.$Window.$__227__177__2__0,$__14,$__13,$props]);
             var $__16=
              _e_($__15);
             var $__swJSW230__0;
             switch($__16._tag_)
              {case 0:
                var $__18=
                 _e_($__16._1);
                var $__21=
                 new _A_($Graphics.$UI.$WXCore.$Types.$sizeZero,[$UHC.$Base.$Num__DCT74__101__0]);
                var $__22=
                 new _A_($Graphics.$UI.$WX.$Classes.$outerSize,[$__]);
                var $__23=
                 new _A_($Graphics.$UI.$WX.$Attributes.$findProperty,[$Graphics.$UI.$WX.$Window.$__227__217__2__0,$__22,$__21,$props]);
                var $__24=
                 _e_($__23);
                var $__swJSW232__0;
                switch($__24._tag_)
                 {case 0:
                   var $__26=
                    _e_($__24._1);
                   var $__29=
                    new _A_($Graphics.$UI.$WXCore.$Types.$rect,[$UHC.$Base.$Num__DCT74__101__0,$__18[0],$__26[0]]);
                   var $__30=
                    new _A_($cont,[$__29,$__26[1]]);
                   $__swJSW232__0=
                    $__30;
                   break;
                  case 1:
                   var $__31=
                    new _A_($Graphics.$UI.$WXCore.$Types.$sizeZero,[$UHC.$Base.$Num__DCT74__101__0]);
                   var $__32=
                    new _A_($Graphics.$UI.$WXCore.$Types.$rect,[$UHC.$Base.$Num__DCT74__101__0,$__18[0],$__31]);
                   var $__33=
                    new _A_($cont,[$__32,$__18[1]]);
                   $__swJSW232__0=
                    $__33;
                   break;}
                $__swJSW230__0=
                 $__swJSW232__0;
                break;
               case 1:
                var $__34=
                 new _A_($Graphics.$UI.$WXCore.$Types.$sizeZero,[$UHC.$Base.$Num__DCT74__101__0]);
                var $__35=
                 new _A_($Graphics.$UI.$WX.$Classes.$outerSize,[$__]);
                var $__36=
                 new _A_($Graphics.$UI.$WX.$Attributes.$findProperty,[$Graphics.$UI.$WX.$Window.$__227__217__2__0,$__35,$__34,$props]);
                var $__37=
                 _e_($__36);
                var $__swJSW234__0;
                switch($__37._tag_)
                 {case 0:
                   var $__39=
                    _e_($__37._1);
                   var $__42=
                    new _A_($Graphics.$UI.$WXCore.$Types.$pointZero,[$UHC.$Base.$Num__DCT74__101__0]);
                   var $__43=
                    new _A_($Graphics.$UI.$WXCore.$Types.$rect,[$UHC.$Base.$Num__DCT74__101__0,$__42,$__39[0]]);
                   var $__44=
                    new _A_($cont,[$__43,$__39[1]]);
                   $__swJSW234__0=
                    $__44;
                   break;
                  case 1:
                   var $__45=
                    new _A_($Graphics.$UI.$WXCore.$Types.$rectZero,[$UHC.$Base.$Num__DCT74__101__0]);
                   var $__46=
                    new _A_($cont,[$__45,$props]);
                   $__swJSW234__0=
                    $__46;
                   break;}
                $__swJSW230__0=
                 $__swJSW234__0;
                break;}
             $__swJSW228__0=
              $__swJSW230__0;
             break;}
          return $__swJSW228__0;});
$Graphics.$UI.$WX.$Window.$__229__56=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["style"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__163__129=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["windowSetWindowStyleFlag not implemented"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowSetWindowStyleFlag=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WXCore.$WindowClass.$__163__129]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__163__210=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["windowGetWindowStyleFlag not implemented"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowGetWindowStyleFlag=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WXCore.$WindowClass.$__163__210]);}),[]);
$Graphics.$UI.$WX.$Attributes.$_24okUNQ536=
 new _F_(function($setter,$w,$f,$_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$_24x]);
          var $__6=
           new _A_($f,[$_24x]);
          var $__7=
           new _A_($setter,[$w,$__6]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__7,$__]);});
$Graphics.$UI.$WX.$Attributes.$updaterUNQ530=
 new _F_(function($getter,$setter,$w,$f)
         {var $__=
           new _A_($getter,[$w]);
          var $__6=
           new _A_($Graphics.$UI.$WX.$Attributes.$_24okUNQ536,[$setter,$w,$f]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__6]);});
$Graphics.$UI.$WX.$Attributes.$reflectiveAttr=
 new _F_(function($__,$name,$getter,$setter)
         {var $__5=
           new _A_($Data.$Dynamic.$fromDynamic,[$__]);
          var $__6=
           new _A_($Data.$Dynamic.$toDyn,[$__]);
          var $__7=
           [$__6,$__5];
          var $__8=
           new _A_($UHC.$Base.$Just__,[$__7]);
          var $__9=
           new _A_($Graphics.$UI.$WX.$Attributes.$updaterUNQ530,[$getter,$setter]);
          return new _A_($Graphics.$UI.$WX.$Attributes.$Attr__,[$name,$__8,$getter,$setter,$__9]);});
$Graphics.$UI.$WX.$Window.$Styled__DCT225__31__0DFLGraphics_2eUI_2eWX_2eClasses_2estyle=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Attributes.$reflectiveAttr,[$Data.$Typeable.$Typeable__DCT320__46__0,$Graphics.$UI.$WX.$Window.$__229__56,$Graphics.$UI.$WXCore.$WindowClass.$windowGetWindowStyleFlag,$Graphics.$UI.$WXCore.$WindowClass.$windowSetWindowStyleFlag]);}),[]);
$Graphics.$UI.$WX.$Classes.$Styled__CLS207__30__0=
 new _F_(function($Styled__)
         {var $Styled__2=
           {_tag_:0,_1:$UHC.$Base.$undefined};
          return $Styled__2;});
$Graphics.$UI.$WX.$Window.$Styled__NEW31UNQ172EVLDCT225__31__0RDC=
 new _F_(function($Styled__,$Styled__2)
         {var $Styled__3=
           _e_(new _A_($Graphics.$UI.$WX.$Classes.$Styled__CLS207__30__0,[$Styled__2]));
          var $__5=
           {_tag_:0,_1:$Styled__};
          return $__5;});
$Graphics.$UI.$WX.$Window.$Styled__NEW28UNQ170DCT225__31__0RDC=
 new _F_(function($Styled__,$Styled__2)
         {var $Styled__3=
           new _A_($Graphics.$UI.$WX.$Window.$Styled__NEW31UNQ172EVLDCT225__31__0RDC,[$Styled__,$Styled__2]);
          return $Styled__3;});
$Graphics.$UI.$WX.$Window.$Styled__UNQ170DCT225__31__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Window.$Styled__NEW28UNQ170DCT225__31__0RDC,[$Graphics.$UI.$WX.$Window.$Styled__DCT225__31__0DFLGraphics_2eUI_2eWX_2eClasses_2estyle,$Graphics.$UI.$WX.$Window.$Styled__UNQ170DCT225__31__0RDC]);}),[]);
$Graphics.$UI.$WX.$Window.$Styled__DCT225__31__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WX.$Window.$Styled__UNQ170DCT225__31__0RDC;}),[]);
$Graphics.$UI.$WX.$Attributes.$__83__4488__0NEW433UNQ825CCN=
 new _F_(function($name,$__,$stop,$__4,$attr)
         {var $__6=
           new _A_($UHC.$Base.$_3d_3d,[$__,$name,$attr]);
          var $__7=
           _e_($__6);
          var $__swJSW237__0;
          switch($__7._tag_)
           {case 0:
             $__swJSW237__0=
              $__4;
             break;
            case 1:
             $__swJSW237__0=
              $stop;
             break;}
          return $__swJSW237__0;});
$Graphics.$UI.$WX.$Attributes.$__83__4657__0NEW452UNQ856CCN=
 new _F_(function($name,$__,$stop,$__4,$attr)
         {var $__6=
           new _A_($UHC.$Base.$_3d_3d,[$__,$name,$attr]);
          var $__7=
           _e_($__6);
          var $__swJSW238__0;
          switch($__7._tag_)
           {case 0:
             $__swJSW238__0=
              $__4;
             break;
            case 1:
             $__swJSW238__0=
              $stop;
             break;}
          return $__swJSW238__0;});
$Graphics.$UI.$WX.$Attributes.$PropValue__=
 new _F_(function($x1)
         {return {_tag_:2,_1:$x1};});
$Graphics.$UI.$WX.$Attributes.$PropModify__=
 new _F_(function($x1)
         {return {_tag_:0,_1:$x1};});
$UHC.$Base.$__78__4417=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$flip,[$UHC.$Base.$_3a]);}),[]);
$UHC.$Base.$reverse=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$foldl,[$UHC.$Base.$__78__4417,$UHC.$Base.$_5b_5d]);}),[]);
$Graphics.$UI.$WX.$Attributes.$dynfUNQ875=
 new _F_(function($f,$fromdyn,$todyn,$__,$x)
         {var $__6=
           new _A_($Data.$Dynamic.$toDyn,[$__,$x]);
          var $__7=
           new _A_($fromdyn,[$__6]);
          var $__8=
           _e_($__7);
          var $__swJSW239__0;
          switch($__8._tag_)
           {case 0:
             var $__10=
              new _A_($f,[$__8._1]);
             var $__11=
              new _A_($todyn,[$__10]);
             var $__12=
              new _A_($Data.$Dynamic.$fromDynamic,[$__,$__11]);
             var $__13=
              _e_($__12);
             var $__swJSW240__0;
             switch($__13._tag_)
              {case 0:
                $__swJSW240__0=
                 $__13._1;
                break;
               case 1:
                $__swJSW240__0=
                 $x;
                break;}
             $__swJSW239__0=
              $__swJSW240__0;
             break;
            case 1:
             $__swJSW239__0=
              $x;
             break;}
          return $__swJSW239__0;});
$Graphics.$UI.$WX.$Attributes.$walkUNQ760=
 new _F_(function($name,$__,$__3,$acc,$res,$props)
         {var $__7=
           new _A_($UHC.$Base.$reverse,[$acc]);
          var $__8=
           new _A_($UHC.$Base.$_2b_2b,[$__7,$props]);
          var $stop=
           [$res,$__8];
          var $__10=
           _e_($props);
          var $__swJSW241__0;
          switch($__10._tag_)
           {case 0:
             var $__13=
              new _A_($UHC.$Base.$_3a,[$__10._1,$acc]);
             var $__14=
              new _A_($Graphics.$UI.$WX.$Attributes.$walkUNQ760,[$name,$__,$__3,$__13,$res,$__10._2]);
             var $__15=
              _e_($__10._1);
             var $__swJSW242__0;
             switch($__15._tag_)
              {case 0:
                var $__18=
                 _e_($__15._1);
                var $__24=
                 new _A_($UHC.$Base.$_3d_3d,[$__,$name,$__18._1]);
                var $__25=
                 _e_($__24);
                var $__swJSW244__0;
                switch($__25._tag_)
                 {case 0:
                   $__swJSW244__0=
                    $__14;
                   break;
                  case 1:
                   $__swJSW244__0=
                    $stop;
                   break;}
                $__swJSW242__0=
                 $__swJSW244__0;
                break;
               case 1:
                var $__28=
                 _e_($__15._1);
                var $__34=
                 new _A_($UHC.$Base.$_3d_3d,[$__,$name,$__28._1]);
                var $__35=
                 _e_($__34);
                var $__swJSW246__0;
                switch($__35._tag_)
                 {case 0:
                   $__swJSW246__0=
                    $__14;
                   break;
                  case 1:
                   $__swJSW246__0=
                    $stop;
                   break;}
                $__swJSW242__0=
                 $__swJSW246__0;
                break;
               case 2:
                var $__38=
                 _e_($__15._1);
                var $__44=
                 new _A_($Graphics.$UI.$WX.$Attributes.$__83__4488__0NEW433UNQ825CCN,[$name,$__,$stop,$__14,$__38._1]);
                var $__45=
                 _e_($__38._2);
                var $__swJSW248__0;
                switch($__45._tag_)
                 {case 0:
                   var $__47=
                    _e_($__45._1);
                   var $__50=
                    new _A_($UHC.$Base.$_3d_3d,[$__,$name,$__38._1]);
                   var $__51=
                    _e_($__50);
                   var $__swJSW250__0;
                   switch($__51._tag_)
                    {case 0:
                      $__swJSW250__0=
                       $__44;
                      break;
                     case 1:
                      var $__52=
                       new _A_($__47[0],[$__15._2]);
                      var $__53=
                       new _A_($Data.$Dynamic.$fromDynamic,[$__3,$__52]);
                      var $__54=
                       _e_($__53);
                      var $__swJSW251__0;
                      switch($__54._tag_)
                       {case 0:
                         var $__56=
                          new _A_($Graphics.$UI.$WX.$Attributes.$PropValue__,[$__54._1]);
                         var $__57=
                          new _A_($Graphics.$UI.$WX.$Attributes.$walkUNQ760,[$name,$__,$__3,$acc,$__56,$__10._2]);
                         $__swJSW251__0=
                          $__57;
                         break;
                        case 1:
                         var $__58=
                          new _A_($Graphics.$UI.$WX.$Attributes.$walkUNQ760,[$name,$__,$__3,$acc,$res,$props]);
                         $__swJSW251__0=
                          $__58;
                         break;}
                      $__swJSW250__0=
                       $__swJSW251__0;
                      break;}
                   $__swJSW248__0=
                    $__swJSW250__0;
                   break;
                  case 1:
                   $__swJSW248__0=
                    $__44;
                   break;}
                $__swJSW242__0=
                 $__swJSW248__0;
                break;
               case 3:
                var $__61=
                 _e_($__15._1);
                var $__67=
                 new _A_($Graphics.$UI.$WX.$Attributes.$__83__4657__0NEW452UNQ856CCN,[$name,$__,$stop,$__14,$__61._1]);
                var $__68=
                 _e_($__61._2);
                var $__swJSW253__0;
                switch($__68._tag_)
                 {case 0:
                   var $__70=
                    _e_($__68._1);
                   var $__73=
                    new _A_($UHC.$Base.$_3d_3d,[$__,$name,$__61._1]);
                   var $__74=
                    _e_($__73);
                   var $__swJSW255__0;
                   switch($__74._tag_)
                    {case 0:
                      $__swJSW255__0=
                       $__67;
                      break;
                     case 1:
                      var $__75=
                       _e_($res);
                      var $__swJSW256__0;
                      switch($__75._tag_)
                       {case 0:
                         var $__77=
                          new _A_($Graphics.$UI.$WX.$Attributes.$dynfUNQ875,[$__15._2,$__70[1],$__70[0],$__3]);
                         var $__78=
                          new _A_($UHC.$Base.$_2e,[$__77,$__75._1]);
                         var $__79=
                          new _A_($Graphics.$UI.$WX.$Attributes.$PropModify__,[$__78]);
                         var $__80=
                          new _A_($Graphics.$UI.$WX.$Attributes.$walkUNQ760,[$name,$__,$__3,$acc,$__79,$__10._2]);
                         $__swJSW256__0=
                          $__80;
                         break;
                        case 1:
                         var $__81=
                          new _A_($Graphics.$UI.$WX.$Attributes.$dynfUNQ875,[$__15._2,$__70[1],$__70[0],$__3]);
                         var $__82=
                          new _A_($Graphics.$UI.$WX.$Attributes.$PropModify__,[$__81]);
                         var $__83=
                          new _A_($Graphics.$UI.$WX.$Attributes.$walkUNQ760,[$name,$__,$__3,$acc,$__82,$__10._2]);
                         $__swJSW256__0=
                          $__83;
                         break;
                        case 2:
                         var $__85=
                          new _A_($Graphics.$UI.$WX.$Attributes.$dynfUNQ875,[$__15._2,$__70[1],$__70[0],$__3,$__75._1]);
                         var $__86=
                          new _A_($Graphics.$UI.$WX.$Attributes.$PropValue__,[$__85]);
                         var $__87=
                          new _A_($Graphics.$UI.$WX.$Attributes.$walkUNQ760,[$name,$__,$__3,$acc,$__86,$__10._2]);
                         $__swJSW256__0=
                          $__87;
                         break;}
                      $__swJSW255__0=
                       $__swJSW256__0;
                      break;}
                   $__swJSW253__0=
                    $__swJSW255__0;
                   break;
                  case 1:
                   $__swJSW253__0=
                    $__67;
                   break;}
                $__swJSW242__0=
                 $__swJSW253__0;
                break;}
             $__swJSW241__0=
              $__swJSW242__0;
             break;
            case 1:
             $__swJSW241__0=
              $stop;
             break;}
          return $__swJSW241__0;});
$Graphics.$UI.$WX.$Attributes.$PropNone__=
 new _A_(new _F_(function()
                 {return {_tag_:1};}),[]);
$Graphics.$UI.$WX.$Attributes.$__85__866__0=
 new _F_(function($__,$name,$props)
         {var $__4=
           new _A_($UHC.$Base.$Eq__DCT74__394__0,[$UHC.$Base.$Eq__DCT74__56__0]);
          return new _A_($Graphics.$UI.$WX.$Attributes.$walkUNQ760,[$name,$__4,$__,$UHC.$Base.$_5b_5d,$Graphics.$UI.$WX.$Attributes.$PropNone__,$props]);});
$Graphics.$UI.$WX.$Attributes.$filterProperty=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          return new _A_($Graphics.$UI.$WX.$Attributes.$__85__866__0,[$__,$__3._1]);});
$Graphics.$UI.$WX.$Attributes.$withProperty=
 new _F_(function($__,$attr,$def,$cont,$props)
         {var $__6=
           new _A_($Graphics.$UI.$WX.$Attributes.$filterProperty,[$__,$attr,$props]);
          var $__7=
           _e_($__6);
          var $__10=
           _e_($__7[0]);
          var $__swJSW259__0;
          switch($__10._tag_)
           {case 0:
             var $__12=
              new _A_($__10._1,[$def]);
             var $__13=
              new _A_($cont,[$__12,$__7[1]]);
             $__swJSW259__0=
              $__13;
             break;
            case 1:
             var $__14=
              new _A_($cont,[$def,$__7[1]]);
             $__swJSW259__0=
              $__14;
             break;
            case 2:
             var $__16=
              new _A_($cont,[$__10._1,$__7[1]]);
             $__swJSW259__0=
              $__16;
             break;}
          return $__swJSW259__0;});
$Graphics.$UI.$WX.$Window.$__229__459__0=
 new _F_(function($cont,$stl_27,$props_27)
         {return new _A_($cont,[$props_27,$stl_27]);});
$Graphics.$UI.$WX.$Classes.$style=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$Data.$Typeable.$__324__421=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["Int"]);}),[]);
$Data.$Typeable.$intTc=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Typeable.$mkTyCon,[$Data.$Typeable.$__324__421]);}),[]);
$Data.$Typeable.$Typeable__DCT320__46__0DFLData_2eTypeable_2etypeOf=
 new _F_(function($__)
         {return new _A_($Data.$Typeable.$mkTyConApp,[$Data.$Typeable.$intTc,$UHC.$Base.$_5b_5d]);});
$Data.$Typeable.$Typeable__NEW274UNQ672EVLDCT320__46__0RDC=
 new _F_(function($Typeable__)
         {var $Typeable__2=
           _e_(new _A_($Data.$Typeable.$Typeable__CLS320__7__0,[$Typeable__]));
          var $__4=
           {_tag_:0,_1:$Data.$Typeable.$Typeable__DCT320__46__0DFLData_2eTypeable_2etypeOf};
          return $__4;});
$Data.$Typeable.$Typeable__NEW272UNQ671DCT320__46__0RDC=
 new _F_(function($Typeable__)
         {var $Typeable__2=
           new _A_($Data.$Typeable.$Typeable__NEW274UNQ672EVLDCT320__46__0RDC,[$Typeable__]);
          return $Typeable__2;});
$Data.$Typeable.$Typeable__UNQ671DCT320__46__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Typeable.$Typeable__NEW272UNQ671DCT320__46__0RDC,[$Data.$Typeable.$Typeable__UNQ671DCT320__46__0RDC]);}),[]);
$Data.$Typeable.$Typeable__DCT320__46__0=
 new _A_(new _F_(function()
                 {return $Data.$Typeable.$Typeable__UNQ671DCT320__46__0RDC;}),[]);
$Graphics.$UI.$WX.$Window.$initialStyle=
 new _F_(function($__,$cont,$props,$stl)
         {var $__5=
           new _A_($Graphics.$UI.$WX.$Classes.$style,[$__]);
          var $__6=
           new _A_($Graphics.$UI.$WX.$Window.$__229__459__0,[$cont]);
          return new _A_($Graphics.$UI.$WX.$Attributes.$withProperty,[$Data.$Typeable.$Typeable__DCT320__46__0,$__5,$stl,$__6,$props]);});
$Graphics.$UI.$WX.$Window.$__229__489__0=
 new _F_(function($cont,$id,$rect)
         {var $__=
           new _A_($cont,[$id,$rect]);
          var $__5=
           new _A_($Graphics.$UI.$WX.$Window.$initialStyle,[$Graphics.$UI.$WX.$Window.$Styled__DCT225__31__0]);
          return new _A_($UHC.$Base.$_24,[$__5,$__]);});
$Graphics.$UI.$WX.$Window.$__229__484__0=
 new _F_(function($cont,$id)
         {var $__=
           new _A_($Graphics.$UI.$WX.$Window.$initialArea,[$Graphics.$UI.$WX.$Window.$Dimensions__DCT225__4__0]);
          var $__4=
           new _A_($Graphics.$UI.$WX.$Window.$__229__489__0,[$cont,$id]);
          return new _A_($UHC.$Base.$_24,[$__,$__4]);});
$Graphics.$UI.$WX.$Window.$initialWindow=
 new _F_(function($cont)
         {var $__=
           new _A_($Graphics.$UI.$WX.$Window.$initialIdentity,[$Graphics.$UI.$WX.$Window.$Identity__DCT225__28__0]);
          var $__3=
           new _A_($Graphics.$UI.$WX.$Window.$__229__484__0,[$cont]);
          return new _A_($UHC.$Base.$_24,[$__,$__3]);});
$Graphics.$UI.$WX.$Types.$feed2=
 new _F_(function($x,$y,$f)
         {return new _A_($f,[$x,$y]);});
$Graphics.$UI.$WX.$Window.$window=
 new _F_(function($parent,$props)
         {var $__=
           new _A_($Graphics.$UI.$WX.$Window.$__229__507__0,[$parent]);
          var $__4=
           new _A_($UHC.$Base.$_24,[$Graphics.$UI.$WX.$Window.$initialWindow,$__]);
          var $__5=
           new _A_($Graphics.$UI.$WX.$Types.$feed2,[$props,0]);
          return new _A_($UHC.$Base.$_24,[$__5,$__4]);});
$Graphics.$UI.$WX.$Window.$__229__81=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["clientSize"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__163__110=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["windowSetClientSize not implemented"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowSetClientSize=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WXCore.$WindowClass.$__163__110]);}),[]);
$Graphics.$UI.$WX.$Window.$Dimensions__DCT225__4__0DFLGraphics_2eUI_2eWX_2eClasses_2eclientSize=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Attributes.$newAttr,[$Graphics.$UI.$WX.$Window.$__229__81,$Graphics.$UI.$WXCore.$WindowClass.$windowGetClientSize,$Graphics.$UI.$WXCore.$WindowClass.$windowSetClientSize]);}),[]);
$Graphics.$UI.$WX.$Window.$__229__86=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["area"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__windowGetRect=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__windowGetRect;});
$Graphics.$UI.$WXCore.$WindowClass.$windowGetRect=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$WindowClass.$__windowGetRect,$Graphics.$UI.$WXCore.$WindowClass.$window__Methods]);}),[]);
$Graphics.$UI.$WX.$Window.$Dimensions__DCT225__4__0DFLGraphics_2eUI_2eWX_2eClasses_2earea=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Attributes.$newAttr,[$Graphics.$UI.$WX.$Window.$__229__86,$Graphics.$UI.$WXCore.$WindowClass.$windowGetRect,$Graphics.$UI.$WXCore.$WindowClass.$windowSetSize]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__windowSetSize=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__windowSetSize;});
$Graphics.$UI.$WXCore.$WindowClass.$windowSetSize=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$WindowClass.$__windowSetSize,$Graphics.$UI.$WXCore.$WindowClass.$window__Methods]);}),[]);
$Graphics.$UI.$WX.$Window.$setSizeUNQ201=
 new _F_(function($w,$sz)
         {var $__=
           new _A_($UHC.$Base.$negate,[$UHC.$Base.$Num__DCT74__101__0,1]);
          var $__4=
           new _A_($UHC.$Base.$negate,[$UHC.$Base.$Num__DCT74__101__0,1]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$Types.$pt,[$UHC.$Base.$Num__DCT74__101__0,$__4,$__]);
          var $__6=
           new _A_($Graphics.$UI.$WXCore.$Types.$rect,[$UHC.$Base.$Num__DCT74__101__0,$__5,$sz]);
          return new _A_($Graphics.$UI.$WXCore.$WindowClass.$windowSetSize,[$w,$__6]);});
$Graphics.$UI.$WX.$Window.$__229__108=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["size"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__windowGetSize=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__windowGetSize;});
$Graphics.$UI.$WXCore.$WindowClass.$windowGetSize=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$WindowClass.$__windowGetSize,$Graphics.$UI.$WXCore.$WindowClass.$window__Methods]);}),[]);
$Graphics.$UI.$WX.$Window.$Dimensions__DCT225__4__0DFLGraphics_2eUI_2eWX_2eClasses_2eouterSize=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Attributes.$newAttr,[$Graphics.$UI.$WX.$Window.$__229__108,$Graphics.$UI.$WXCore.$WindowClass.$windowGetSize,$Graphics.$UI.$WX.$Window.$setSizeUNQ201]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__163__289=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["windowGetEffectiveMinSize not implemented"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowGetEffectiveMinSize=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WXCore.$WindowClass.$__163__289]);}),[]);
$Graphics.$UI.$WX.$Window.$__229__112=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["bestSize"]);}),[]);
$Graphics.$UI.$WX.$Attributes.$__85__755__0=
 new _F_(function($name,$w,$x)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["' is read-only."]);
          var $__5=
           new _A_($UHC.$Base.$_2b_2b,[$name,$__]);
          var $__6=
           new _A_($UHC.$Base.$packedStringToString,["attribute '"]);
          var $__7=
           new _A_($UHC.$Base.$_2b_2b,[$__6,$__5]);
          var $__8=
           new _A_($UHC.$IOBase.$userError,[$__7]);
          return new _A_($UHC.$IOBase.$ioError,[$__8]);});
$Graphics.$UI.$WX.$Attributes.$readAttr=
 new _F_(function($name,$getter)
         {var $__=
           new _A_($Graphics.$UI.$WX.$Attributes.$__85__755__0,[$name]);
          return new _A_($Graphics.$UI.$WX.$Attributes.$newAttr,[$name,$getter,$__]);});
$Graphics.$UI.$WX.$Window.$Dimensions__DCT225__4__0DFLGraphics_2eUI_2eWX_2eClasses_2ebestSize=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Attributes.$readAttr,[$Graphics.$UI.$WX.$Window.$__229__112,$Graphics.$UI.$WXCore.$WindowClass.$windowGetEffectiveMinSize]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__163__893=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["windowSetVirtualSize not implemented"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowSetVirtualSize=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WXCore.$WindowClass.$__163__893]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__163__948=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["windowGetVirtualSize not implemented"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowGetVirtualSize=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WXCore.$WindowClass.$__163__948]);}),[]);
$Graphics.$UI.$WX.$Window.$__229__117=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["virtualSize"]);}),[]);
$Graphics.$UI.$WX.$Window.$Dimensions__DCT225__4__0DFLGraphics_2eUI_2eWX_2eClasses_2evirtualSize=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Attributes.$newAttr,[$Graphics.$UI.$WX.$Window.$__229__117,$Graphics.$UI.$WXCore.$WindowClass.$windowGetVirtualSize,$Graphics.$UI.$WXCore.$WindowClass.$windowSetVirtualSize]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__windowGetPosition=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__windowGetPosition;});
$Graphics.$UI.$WXCore.$WindowClass.$windowGetPosition=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$WindowClass.$__windowGetPosition,$Graphics.$UI.$WXCore.$WindowClass.$window__Methods]);}),[]);
$Graphics.$UI.$WX.$Window.$__229__122=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["position"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__163__1477=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["windowMove not implemented"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowMove=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WXCore.$WindowClass.$__163__1477]);}),[]);
$Graphics.$UI.$WX.$Window.$Dimensions__DCT225__4__0DFLGraphics_2eUI_2eWX_2eClasses_2eposition=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Attributes.$newAttr,[$Graphics.$UI.$WX.$Window.$__229__122,$Graphics.$UI.$WXCore.$WindowClass.$windowGetPosition,$Graphics.$UI.$WXCore.$WindowClass.$windowMove]);}),[]);
$Graphics.$UI.$WX.$Classes.$__211__490__0=
 new _F_(function($r)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Types.$rectTopLeft,[$UHC.$Base.$Num__DCT74__101__0,$r]);
          return new _A_($Graphics.$UI.$WXCore.$Types.$rect,[$UHC.$Base.$Num__DCT74__101__0,$__]);});
$Graphics.$UI.$WX.$Classes.$clientSize=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._3;});
$Graphics.$UI.$WX.$Classes.$__211__475__0=
 new _F_(function($r,$pt)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Types.$rectSize,[$UHC.$Base.$Num__DCT74__101__0,$r]);
          return new _A_($Graphics.$UI.$WXCore.$Types.$rect,[$UHC.$Base.$Num__DCT74__101__0,$pt,$__]);});
$Graphics.$UI.$WXCore.$Types.$rectTopLeft=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          var $__8=
           new _A_($Graphics.$UI.$WXCore.$Types.$Point__,[$__3.rectLeft,$__3.rectTop]);
          return $__8;});
$Graphics.$UI.$WXCore.$Types.$rectSize=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          var $__8=
           new _A_($Graphics.$UI.$WXCore.$Types.$Size__,[$__3.rectWidth,$__3.rectHeight]);
          return $__8;});
$Graphics.$UI.$WX.$Classes.$setAreaUNQ270=
 new _F_(function($__,$w,$rect)
         {var $__4=
           new _A_($Graphics.$UI.$WXCore.$Types.$rectTopLeft,[$UHC.$Base.$Num__DCT74__101__0,$rect]);
          var $__5=
           new _A_($Graphics.$UI.$WX.$Classes.$position,[$__]);
          var $__6=
           new _A_($Graphics.$UI.$WX.$Attributes.$_3a_3d,[$__5,$__4]);
          var $__7=
           new _A_($UHC.$Base.$_3a,[$__6,$UHC.$Base.$_5b_5d]);
          var $__8=
           new _A_($Graphics.$UI.$WXCore.$Types.$rectSize,[$UHC.$Base.$Num__DCT74__101__0,$rect]);
          var $__9=
           new _A_($Graphics.$UI.$WX.$Classes.$outerSize,[$__]);
          var $__10=
           new _A_($Graphics.$UI.$WX.$Attributes.$_3a_3d,[$__9,$__8]);
          var $__11=
           new _A_($UHC.$Base.$_3a,[$__10,$__7]);
          return new _A_($Graphics.$UI.$WX.$Attributes.$set,[$w,$__11]);});
$Graphics.$UI.$WX.$Classes.$_24okUNQ291=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Types.$rect,[$UHC.$Base.$Num__DCT74__101__0,$_24x2,$_24x]);
          return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__]);});
$Graphics.$UI.$WX.$Classes.$position=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._5;});
$Graphics.$UI.$WX.$Classes.$_24okUNQ287=
 new _F_(function($__,$w,$_24x)
         {var $__4=
           new _A_($Graphics.$UI.$WX.$Classes.$position,[$__]);
          var $__5=
           new _A_($Graphics.$UI.$WX.$Attributes.$get,[$w,$__4]);
          var $__6=
           new _A_($Graphics.$UI.$WX.$Classes.$_24okUNQ291,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__5,$__6]);});
$Graphics.$UI.$WX.$Classes.$outerSize=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._4;});
$Graphics.$UI.$WX.$Attributes.$get=
 new _F_(function($w,$__)
         {var $__3=
           _e_($__);
          var $__9=
           new _A_($__3._3,[$w]);
          return $__9;});
$Graphics.$UI.$WX.$Classes.$getAreaUNQ272=
 new _F_(function($__,$w)
         {var $__3=
           new _A_($Graphics.$UI.$WX.$Classes.$outerSize,[$__]);
          var $__4=
           new _A_($Graphics.$UI.$WX.$Attributes.$get,[$w,$__3]);
          var $__5=
           new _A_($Graphics.$UI.$WX.$Classes.$_24okUNQ287,[$__,$w]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__5]);});
$Graphics.$UI.$WX.$Classes.$Dimensions__NEW138CLS207__1__0DFLGraphics_2eUI_2eWX_2eClasses_2earea=
 new _F_(function($Dimensions__)
         {var $__=
           new _A_($Graphics.$UI.$WX.$Classes.$setAreaUNQ270,[$Dimensions__]);
          var $__3=
           new _A_($Graphics.$UI.$WX.$Classes.$getAreaUNQ272,[$Dimensions__]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToString,["area"]);
          return new _A_($Graphics.$UI.$WX.$Attributes.$newAttr,[$__4,$__3,$__]);});
$Graphics.$UI.$WX.$Classes.$Dimensions__CLS207__1__0=
 new _F_(function($Dimensions__)
         {var $Dimensions__CLS207__1__0DFLGraphics_2eUI_2eWX_2eClasses_2ebestSize=
           new _A_($Graphics.$UI.$WX.$Classes.$outerSize,[$Dimensions__]);
          var $Dimensions__CLS207__1__0DFLGraphics_2eUI_2eWX_2eClasses_2evirtualSize=
           new _A_($Graphics.$UI.$WX.$Classes.$clientSize,[$Dimensions__]);
          var $Dimensions__CLS207__1__0DFLGraphics_2eUI_2eWX_2eClasses_2earea=
           new _A_($Graphics.$UI.$WX.$Classes.$Dimensions__NEW138CLS207__1__0DFLGraphics_2eUI_2eWX_2eClasses_2earea,[$Dimensions__]);
          var $Dimensions__CLS207__1__0DFLGraphics_2eUI_2eWX_2eClasses_2eclientSize=
           new _A_($Graphics.$UI.$WX.$Classes.$outerSize,[$Dimensions__]);
          var $__=
           new _A_($Graphics.$UI.$WX.$Classes.$area,[$Dimensions__]);
          var $__3=
           new _A_($Graphics.$UI.$WXCore.$Types.$rectTopLeft,[$UHC.$Base.$Num__DCT74__101__0]);
          var $Dimensions__CLS207__1__0DFLGraphics_2eUI_2eWX_2eClasses_2eposition=
           new _A_($Graphics.$UI.$WX.$Attributes.$mapAttr,[$__3,$Graphics.$UI.$WX.$Classes.$__211__475__0,$__]);
          var $__4=
           new _A_($Graphics.$UI.$WX.$Classes.$area,[$Dimensions__]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$Types.$rectSize,[$UHC.$Base.$Num__DCT74__101__0]);
          var $Dimensions__CLS207__1__0DFLGraphics_2eUI_2eWX_2eClasses_2eouterSize=
           new _A_($Graphics.$UI.$WX.$Attributes.$mapAttr,[$__5,$Graphics.$UI.$WX.$Classes.$__211__490__0,$__4]);
          var $Dimensions__6=
           {_tag_:0,_1:$Dimensions__CLS207__1__0DFLGraphics_2eUI_2eWX_2eClasses_2earea,_2:$Dimensions__CLS207__1__0DFLGraphics_2eUI_2eWX_2eClasses_2ebestSize,_3:$Dimensions__CLS207__1__0DFLGraphics_2eUI_2eWX_2eClasses_2eclientSize,_4:$Dimensions__CLS207__1__0DFLGraphics_2eUI_2eWX_2eClasses_2eouterSize,_5:$Dimensions__CLS207__1__0DFLGraphics_2eUI_2eWX_2eClasses_2eposition,_6:$Dimensions__CLS207__1__0DFLGraphics_2eUI_2eWX_2eClasses_2evirtualSize};
          return $Dimensions__6;});
$Graphics.$UI.$WX.$Window.$Dimensions__NEW73UNQ186EVLDCT225__4__0RDC=
 new _F_(function($Dimensions__,$Dimensions__2,$Dimensions__3,$Dimensions__4,$Dimensions__5,$Dimensions__6,$Dimensions__7)
         {var $Dimensions__8=
           _e_(new _A_($Graphics.$UI.$WX.$Classes.$Dimensions__CLS207__1__0,[$Dimensions__]));
          var $__15=
           {_tag_:0,_1:$Dimensions__6,_2:$Dimensions__4,_3:$Dimensions__7,_4:$Dimensions__5,_5:$Dimensions__2,_6:$Dimensions__3};
          return $__15;});
$Graphics.$UI.$WX.$Window.$Dimensions__NEW65UNQ185DCT225__4__0RDC=
 new _F_(function($Dimensions__,$Dimensions__2,$Dimensions__3,$Dimensions__4,$Dimensions__5,$Dimensions__6,$Dimensions__7)
         {var $Dimensions__8=
           new _A_($Graphics.$UI.$WX.$Window.$Dimensions__NEW73UNQ186EVLDCT225__4__0RDC,[$Dimensions__,$Dimensions__2,$Dimensions__3,$Dimensions__4,$Dimensions__5,$Dimensions__6,$Dimensions__7]);
          return $Dimensions__8;});
$Graphics.$UI.$WX.$Window.$Dimensions__UNQ185DCT225__4__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Window.$Dimensions__NEW65UNQ185DCT225__4__0RDC,[$Graphics.$UI.$WX.$Window.$Dimensions__UNQ185DCT225__4__0RDC,$Graphics.$UI.$WX.$Window.$Dimensions__DCT225__4__0DFLGraphics_2eUI_2eWX_2eClasses_2eposition,$Graphics.$UI.$WX.$Window.$Dimensions__DCT225__4__0DFLGraphics_2eUI_2eWX_2eClasses_2evirtualSize,$Graphics.$UI.$WX.$Window.$Dimensions__DCT225__4__0DFLGraphics_2eUI_2eWX_2eClasses_2ebestSize,$Graphics.$UI.$WX.$Window.$Dimensions__DCT225__4__0DFLGraphics_2eUI_2eWX_2eClasses_2eouterSize,$Graphics.$UI.$WX.$Window.$Dimensions__DCT225__4__0DFLGraphics_2eUI_2eWX_2eClasses_2earea,$Graphics.$UI.$WX.$Window.$Dimensions__DCT225__4__0DFLGraphics_2eUI_2eWX_2eClasses_2eclientSize]);}),[]);
$Graphics.$UI.$WX.$Window.$Dimensions__DCT225__4__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WX.$Window.$Dimensions__UNQ185DCT225__4__0RDC;}),[]);
$Graphics.$UI.$WX.$Timer.$__241__29=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["timer-interval"]);}),[]);
$Graphics.$UI.$WX.$Timer.$_24okUNQ33=
 new _F_(function($t,$i,$_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$TimerClass.$timerStart,[$t,$i,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__5,$__]);});
$Graphics.$UI.$WXCore.$TimerClass.$__timerIsOneShot=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__timerIsOneShot;});
$Graphics.$UI.$WXCore.$TimerClass.$timerIsOneShot=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$TimerClass.$__timerIsOneShot,$Graphics.$UI.$WXCore.$TimerClass.$timer__Methods]);}),[]);
$Graphics.$UI.$WX.$Timer.$__241__50NEW13=
 new _F_(function($t,$i)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$TimerClass.$timerIsOneShot,[$t]);
          var $__4=
           new _A_($Graphics.$UI.$WX.$Timer.$_24okUNQ33,[$t,$i]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Graphics.$UI.$WX.$Timer.$_24okUNQ24=
 new _F_(function($t,$i,$_24x)
         {var $__=
           _e_($_24x);
          var $__swJSW274__0;
          switch($__._tag_)
           {case 0:
             var $__5=
              new _A_($Graphics.$UI.$WXCore.$TimerClass.$timerStop,[$t]);
             var $__6=
              new _A_($Graphics.$UI.$WXCore.$TimerClass.$timerStart,[$t,$i,$UHC.$Base.$True__]);
             var $__7=
              new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__5]);
             $__swJSW274__0=
              $__7;
             break;
            case 1:
             var $__8=
              new _A_($Graphics.$UI.$WX.$Timer.$__241__50NEW13,[$t,$i]);
             var $__9=
              new _A_($Graphics.$UI.$WXCore.$TimerClass.$timerStop,[$t]);
             var $__10=
              new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__9,$__8]);
             $__swJSW274__0=
              $__10;
             break;}
          return $__swJSW274__0;});
$Graphics.$UI.$WXCore.$TimerClass.$__timerIsRunning=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__timerIsRunning;});
$Graphics.$UI.$WXCore.$TimerClass.$timerIsRunning=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$TimerClass.$__timerIsRunning,$Graphics.$UI.$WXCore.$TimerClass.$timer__Methods]);}),[]);
$Graphics.$UI.$WX.$Timer.$__241__30__0=
 new _F_(function($t,$i)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$TimerClass.$timerIsRunning,[$t]);
          var $__4=
           new _A_($Graphics.$UI.$WX.$Timer.$_24okUNQ24,[$t,$i]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Graphics.$UI.$WX.$Timer.$interval=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Attributes.$newAttr,[$Graphics.$UI.$WX.$Timer.$__241__29,$Graphics.$UI.$WXCore.$TimerClass.$timerGetInterval,$Graphics.$UI.$WX.$Timer.$__241__30__0]);}),[]);
$Asteroids.$__55__348__0=
 new _F_(function($i)
         {var $__=
           new _A_($UHC.$Base.$div,[$UHC.$Base.$Integral__DCT74__110__0,$i,2]);
          return new _A_($UHC.$Base.$max,[$UHC.$Base.$Ord__DCT74__91__0,10,$__]);});
$Language.$UHC.$JS.$Prelude.$__trace=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_(console.log($__3));
          var $__5=
           _e_([]);
          return [$__2,$__5];});
$UHC.$Base.$min=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._8;});
$Asteroids.$__55__301__0=
 new _F_(function($x)
         {var $__=
           new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$x,5]);
          return new _A_($UHC.$Base.$min,[$UHC.$Base.$Ord__DCT74__91__0,$Asteroids.$width,$__]);});
$UHC.$Base.$zipWith=
 new _F_(function($x1,$x2,$x3)
         {var $x24=
           _e_($x2);
          var $__swJSW277__0;
          switch($x24._tag_)
           {case 0:
             var $x37=
              _e_($x3);
             var $__swJSW278__0;
             switch($x37._tag_)
              {case 0:
                var $__=
                 new _A_($UHC.$Base.$zipWith,[$x1,$x24._2,$x37._2]);
                var $__11=
                 new _A_($x1,[$x24._1,$x37._1]);
                var $__12=
                 new _A_($UHC.$Base.$_3a,[$__11,$__]);
                $__swJSW278__0=
                 $__12;
                break;
               case 1:
                $__swJSW278__0=
                 $UHC.$Base.$_5b_5d;
                break;}
             $__swJSW277__0=
              $__swJSW278__0;
             break;
            case 1:
             $__swJSW277__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW277__0;});
$UHC.$Base.$__78__3116__0=
 new _F_(function($a,$b)
         {return [$a,$b];});
$UHC.$Base.$zip=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$zipWith,[$UHC.$Base.$__78__3116__0]);}),[]);
$UHC.$Base.$or=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$foldr,[$UHC.$Base.$_7c_7c,$UHC.$Base.$False__]);}),[]);
$Asteroids.$__55__35=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["../resources/ship.ico"]);}),[]);
$Asteroids.$ship=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Media.$bitmap,[$Asteroids.$__55__35]);}),[]);
$Asteroids.$drawShip=
 new _F_(function($dc,$pos)
         {return new _A_($Graphics.$UI.$WX.$Draw.$drawBitmap,[$dc,$Asteroids.$ship,$pos,$UHC.$Base.$True__,$UHC.$Base.$_5b_5d]);});
$UHC.$Base.$_24okUNQ4316=
 new _F_(function($__,$cs,$_24x)
         {var $__4=
           new _A_($UHC.$Base.$sequence,[$__,$cs]);
          var $__5=
           new _A_($UHC.$Base.$_24okUNQ4320,[$__,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$__4,$__5]);});
$UHC.$Base.$_24okUNQ4320=
 new _F_(function($__,$_24x,$_24x3)
         {var $__4=
           new _A_($UHC.$Base.$_3a,[$_24x,$_24x3]);
          return new _A_($UHC.$Base.$return,[$__,$__4]);});
$UHC.$Base.$sequence=
 new _F_(function($__,$x1)
         {var $__3=
           _e_($x1);
          var $__swJSW279__0;
          switch($__3._tag_)
           {case 0:
             var $__6=
              new _A_($UHC.$Base.$_24okUNQ4316,[$__,$__3._2]);
             $__swJSW279__0=
              new _A_($UHC.$Base.$_3e_3e_3d,[$__,$__3._1,$__6]);
             break;
            case 1:
             var $__7=
              new _A_($UHC.$Base.$return,[$__,$UHC.$Base.$_5b_5d]);
             $__swJSW279__0=
              $__7;
             break;}
          return $__swJSW279__0;});
$UHC.$Base.$mapM=
 new _F_(function($__,$f)
         {var $__3=
           new _A_($UHC.$Base.$map,[$f]);
          var $__4=
           new _A_($UHC.$Base.$sequence,[$__]);
          return new _A_($UHC.$Base.$_2e,[$__4,$__3]);});
$Asteroids.$__55__59=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["../resources/rock.ico"]);}),[]);
$Asteroids.$rock=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Media.$bitmap,[$Asteroids.$__55__59]);}),[]);
$Graphics.$UI.$WXCore.$GraphicsObjectClass.$__91__186__0=
 new _F_(function($v,$o)
         {var $__=
           new _A_($LightOO.$Core.$unRecord,[$o]);
          var $__4=
           new _A_($LightOO.$Core.$setTail,[$Graphics.$UI.$WXCore.$GraphicsObjectClass.$ModTail__DCT87__3__0,$__,$v]);
          return new _A_($UHC.$Base.$_24,[$LightOO.$Core.$record,$__4]);});
$Graphics.$UI.$WXCore.$GraphicsObjectClass.$set__GraphicsObject__Tail=
 new _F_(function($o,$v)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$GraphicsObjectClass.$__91__186__0,[$v]);
          return new _A_($LightOO.$modify__Object__Tail,[$__,$o]);});
$Language.$UHC.$JS.$HTML5.$HTMLImageElement.$src=
 new _F_(function($e,$s)
         {var $__=
           new _A_($Language.$UHC.$JS.$Marshal.$str,[$s]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToString,["src"]);
          return new _A_($Language.$UHC.$JS.$Prelude.$setAttr__,[$__4,$__,$e]);});
$Graphics.$UI.$WXCore.$GraphicsBitmap.$_24okUNQ16=
 new _F_(function($source,$_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$_24x]);
          var $__4=
           new _A_($Language.$UHC.$JS.$HTML5.$HTMLImageElement.$src,[$_24x,$source]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__]);});
$Language.$UHC.$JS.$HTML5.$HTMLImageElement.$newImage=
 new _F_(function($__)
         {var $__2=
           _e_(new Image());
          return [$__,$__2];});
$Graphics.$UI.$WXCore.$GraphicsBitmap.$__131__68__0NEW5UNQ12=
 new _F_(function($source)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$GraphicsBitmap.$_24okUNQ16,[$source]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Language.$UHC.$JS.$HTML5.$HTMLImageElement.$newImage,$__]);});
$Graphics.$UI.$WXCore.$GraphicsBitmap.$__133__14NEW2=
 new _F_(function($source,$tail)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$GraphicsBitmap.$__131__68__0NEW5UNQ12,[$source]);
          return {_tag_:0,__graphicsBitmapGetNativeBitmap:$__,__graphicsBitmapTail:$tail};});
$Graphics.$UI.$WXCore.$GraphicsBitmap.$bitmap_27UNQ5=
 new _F_(function($source,$tail,$super,$__,$self)
         {var $__6=
           new _A_($Graphics.$UI.$WXCore.$GraphicsBitmap.$__133__14NEW2,[$source,$tail]);
          return new _A_($UHC.$Base.$return,[$__,$__6]);});
$Graphics.$UI.$WXCore.$GraphicsBitmap.$__133__34__0=
 new _F_(function($source,$__,$__3)
         {return new _A_($Graphics.$UI.$WXCore.$GraphicsBitmap.$bitmap_27UNQ5,[$source,$__,$__3,$UHC.$Base.$Monad__DCT74__339__0]);});
$Graphics.$UI.$WXCore.$GraphicsObject.$graphicsObject_27UNQ3=
 new _F_(function($tail,$super,$__,$self)
         {var $__5=
           {_tag_:0,__graphicsObjectTail:$tail};
          return new _A_($UHC.$Base.$return,[$__,$__5]);});
$Graphics.$UI.$WXCore.$GraphicsObject.$__109__17__0=
 new _F_(function($__,$__2)
         {return new _A_($Graphics.$UI.$WXCore.$GraphicsObject.$graphicsObject_27UNQ3,[$__,$__2,$UHC.$Base.$Monad__DCT74__339__0]);});
$Graphics.$UI.$WXCore.$GraphicsObject.$graphicsObject=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$extends,[$Graphics.$UI.$WXCore.$GraphicsObject.$__109__17__0,$LightOO.$object,$LightOO.$Core.$noOverride,$LightOO.$set__Object__Tail]);}),[]);
$Graphics.$UI.$WXCore.$GraphicsBitmap.$bitmap=
 new _F_(function($source)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$GraphicsBitmap.$__133__34__0,[$source]);
          return new _A_($LightOO.$Core.$extends,[$__,$Graphics.$UI.$WXCore.$GraphicsObject.$graphicsObject,$LightOO.$Core.$noOverride,$Graphics.$UI.$WXCore.$GraphicsObjectClass.$set__GraphicsObject__Tail]);});
$Graphics.$UI.$WX.$Media.$bitmap=
 new _F_(function($fname)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$GraphicsBitmap.$bitmap,[$fname]);
          var $__3=
           new _A_($LightOO.$Core.$new,[$Control.$Monad.$Fix.$MonadFix__DCT97__4__0]);
          var $__4=
           new _A_($UHC.$Base.$_24,[$__3,$__]);
          return new _A_($UHC.$IOBase.$unsafePerformIO,[$__4]);});
$Asteroids.$__55__115=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["../resources/burning.ico"]);}),[]);
$Asteroids.$burning=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Media.$bitmap,[$Asteroids.$__55__115]);}),[]);
$Asteroids.$rockPictureNEW43UNQ115=
 new _F_(function($collides)
         {var $__=
           _e_($collides);
          var $__swJSW280__0;
          switch($__._tag_)
           {case 0:
             $__swJSW280__0=
              $Asteroids.$rock;
             break;
            case 1:
             $__swJSW280__0=
              $Asteroids.$burning;
             break;}
          return $__swJSW280__0;});
$Graphics.$UI.$WXCore.$GraphicsBitmapClass.$graphicsBitmap__Methods=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$LightOO.$Core.$unRecord,$Graphics.$UI.$WXCore.$GraphicsObjectClass.$get__GraphicsObject__Tail]);}),[]);
$Graphics.$UI.$WXCore.$GraphicsBitmapClass.$__graphicsBitmapGetNativeBitmap=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__graphicsBitmapGetNativeBitmap;});
$Graphics.$UI.$WXCore.$GraphicsBitmapClass.$graphicsBitmapGetNativeBitmap=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$GraphicsBitmapClass.$__graphicsBitmapGetNativeBitmap,$Graphics.$UI.$WXCore.$GraphicsBitmapClass.$graphicsBitmap__Methods]);}),[]);
$Language.$UHC.$JS.$HTML5.$HTMLImageElement.$__115__8=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["width"]);}),[]);
$Language.$UHC.$JS.$HTML5.$HTMLImageElement.$getWidth=
 new _A_(new _F_(function()
                 {return new _A_($Language.$UHC.$JS.$Prelude.$getAttr,[$Language.$UHC.$JS.$HTML5.$HTMLImageElement.$__115__8]);}),[]);
$Language.$UHC.$JS.$HTML5.$HTMLImageElement.$__115__30=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["height"]);}),[]);
$Language.$UHC.$JS.$Primitives.$__primGetAttr=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__);
          var $__5=
           _e_($__2);
          var $__6=
           _e_(primGetAttr($__4,$__5));
          return [$__3,$__6];});
$Language.$UHC.$JS.$Prelude.$getAttr=
 new _F_(function($s)
         {var $__=
           new _A_($Language.$UHC.$JS.$Marshal.$toJS,[$Language.$UHC.$JS.$Marshal.$ToJS__DCT81__13__0,$s]);
          return new _A_($Language.$UHC.$JS.$Primitives.$__primGetAttr,[$__]);});
$Language.$UHC.$JS.$HTML5.$HTMLImageElement.$getHeight=
 new _A_(new _F_(function()
                 {return new _A_($Language.$UHC.$JS.$Prelude.$getAttr,[$Language.$UHC.$JS.$HTML5.$HTMLImageElement.$__115__30]);}),[]);
$Graphics.$UI.$WXCore.$GraphicsContextClass.$__graphicsContextDrawBitmap=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__graphicsContextDrawBitmap;});
$Graphics.$UI.$WXCore.$GraphicsObjectClass.$ModTail__DCT87__3__0DFLLightOO_2eCore_2esetTail=
 new _F_(function($o,$v)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["has no field update '[Graphics.UI.WXCore.GraphicsObjectClass._graphicsObjectTail]'"]);
          var $__4=
           new _A_($UHC.$Base.$error,[$__]);
          var $__5=
           _e_($o);
          var $__7=
           {_tag_:0,__graphicsObjectTail:$v};
          return $__7;});
$Graphics.$UI.$WXCore.$GraphicsObjectClass.$__graphicsObjectTail=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__graphicsObjectTail;});
$Graphics.$UI.$WXCore.$GraphicsObjectClass.$ModTail__NEW109UNQ83EVLDCT87__3__0RDC=
 new _F_(function($ModTail__)
         {var $ModTail__2=
           _e_(new _A_($LightOO.$Core.$ModTail__CLS69__7__0,[$ModTail__]));
          var $__6=
           {_tag_:0,_1:$Graphics.$UI.$WXCore.$GraphicsObjectClass.$__graphicsObjectTail,_2:$ModTail__2._2,_3:$Graphics.$UI.$WXCore.$GraphicsObjectClass.$ModTail__DCT87__3__0DFLLightOO_2eCore_2esetTail};
          return $__6;});
$Graphics.$UI.$WXCore.$GraphicsObjectClass.$ModTail__NEW107UNQ82DCT87__3__0RDC=
 new _F_(function($ModTail__)
         {var $ModTail__2=
           new _A_($Graphics.$UI.$WXCore.$GraphicsObjectClass.$ModTail__NEW109UNQ83EVLDCT87__3__0RDC,[$ModTail__]);
          return $ModTail__2;});
$Graphics.$UI.$WXCore.$GraphicsObjectClass.$ModTail__UNQ82DCT87__3__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$GraphicsObjectClass.$ModTail__NEW107UNQ82DCT87__3__0RDC,[$Graphics.$UI.$WXCore.$GraphicsObjectClass.$ModTail__UNQ82DCT87__3__0RDC]);}),[]);
$Graphics.$UI.$WXCore.$GraphicsObjectClass.$ModTail__DCT87__3__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$GraphicsObjectClass.$ModTail__UNQ82DCT87__3__0RDC;}),[]);
$Graphics.$UI.$WXCore.$GraphicsObjectClass.$__91__176=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$getTail,[$Graphics.$UI.$WXCore.$GraphicsObjectClass.$ModTail__DCT87__3__0]);}),[]);
$Graphics.$UI.$WXCore.$GraphicsObjectClass.$__91__177=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$LightOO.$Core.$unRecord,$LightOO.$get__Object__Tail]);}),[]);
$Graphics.$UI.$WXCore.$GraphicsObjectClass.$get__GraphicsObject__Tail=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$GraphicsObjectClass.$__91__176,$Graphics.$UI.$WXCore.$GraphicsObjectClass.$__91__177]);}),[]);
$Graphics.$UI.$WXCore.$GraphicsContextClass.$graphicsContext__Methods=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$LightOO.$Core.$unRecord,$Graphics.$UI.$WXCore.$GraphicsObjectClass.$get__GraphicsObject__Tail]);}),[]);
$Graphics.$UI.$WXCore.$GraphicsContextClass.$graphicsContextDrawBitmap=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$GraphicsContextClass.$__graphicsContextDrawBitmap,$Graphics.$UI.$WXCore.$GraphicsContextClass.$graphicsContext__Methods]);}),[]);
$Graphics.$UI.$WXCore.$Types.$pointY=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.pointY;});
$Graphics.$UI.$WXCore.$Types.$pointX=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.pointX;});
$Graphics.$UI.$WX.$Draw.$_24okUNQ28=
 new _F_(function($dc,$bitmap,$point,$_24x,$_24x5)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Types.$pointY,[$point]);
          var $__7=
           new _A_($UHC.$Base.$fromIntegral,[$UHC.$Base.$Integral__DCT74__110__0,$UHC.$Base.$Num__DCT74__177__0]);
          var $__8=
           new _A_($UHC.$Base.$_24,[$__7,$__]);
          var $__9=
           new _A_($Graphics.$UI.$WXCore.$Types.$pointX,[$point]);
          var $__10=
           new _A_($UHC.$Base.$fromIntegral,[$UHC.$Base.$Integral__DCT74__110__0,$UHC.$Base.$Num__DCT74__177__0]);
          var $__11=
           new _A_($UHC.$Base.$_24,[$__10,$__9]);
          return new _A_($Graphics.$UI.$WXCore.$GraphicsContextClass.$graphicsContextDrawBitmap,[$dc,$bitmap,$__11,$__8,$_24x,$_24x5]);});
$Graphics.$UI.$WX.$Draw.$_24okUNQ19=
 new _F_(function($dc,$bitmap,$point,$_24x,$_24x5)
         {var $__=
           new _A_($Language.$UHC.$JS.$HTML5.$HTMLImageElement.$getHeight,[$_24x]);
          var $__7=
           new _A_($Graphics.$UI.$WX.$Draw.$_24okUNQ28,[$dc,$bitmap,$point,$_24x5]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__7]);});
$Graphics.$UI.$WX.$Draw.$_24okUNQ13=
 new _F_(function($dc,$bitmap,$point,$_24x)
         {var $__=
           new _A_($Language.$UHC.$JS.$HTML5.$HTMLImageElement.$getWidth,[$_24x]);
          var $__6=
           new _A_($Graphics.$UI.$WX.$Draw.$_24okUNQ19,[$dc,$bitmap,$point,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__6]);});
$Graphics.$UI.$WX.$Draw.$drawBitmap=
 new _F_(function($dc,$bitmap,$point,$transparent,$__)
         {var $__6=
           _e_($__);
          var $__swJSW288__0;
          switch($__6._tag_)
           {case 0:
             $__swJSW288__0=
              $UHC.$Base.$undefined;
             break;
            case 1:
             var $__9=
              new _A_($Graphics.$UI.$WXCore.$GraphicsBitmapClass.$graphicsBitmapGetNativeBitmap,[$bitmap]);
             var $__10=
              new _A_($Graphics.$UI.$WX.$Draw.$_24okUNQ13,[$dc,$bitmap,$point]);
             $__swJSW288__0=
              new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__9,$__10]);
             break;}
          return $__swJSW288__0;});
$Asteroids.$drawRock=
 new _F_(function($dc,$__)
         {var $__3=
           _e_($__);
          var $rockPicture=
           new _A_($Asteroids.$rockPictureNEW43UNQ115,[$__3[1]]);
          return new _A_($Graphics.$UI.$WX.$Draw.$drawBitmap,[$dc,$rockPicture,$__3[0],$UHC.$Base.$True__,$UHC.$Base.$_5b_5d]);});
$Graphics.$UI.$WXCore.$Types.$Vector__=
 new _F_(function($x1,$x2)
         {return {_tag_:0,vecX:$x1,vecY:$x2};});
$Graphics.$UI.$WXCore.$Types.$__55__2736__0=
 new _F_(function($__,$x1,$y1,$__4)
         {var $__5=
           _e_($__4);
          var $__8=
           new _A_($UHC.$Base.$_2d,[$__,$__5.pointY,$y1]);
          var $__9=
           new _A_($UHC.$Base.$_2d,[$__,$__5.pointX,$x1]);
          var $__10=
           new _A_($Graphics.$UI.$WXCore.$Types.$Vector__,[$__9,$__8]);
          return $__10;});
$Graphics.$UI.$WXCore.$Types.$vecBetween=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          return new _A_($Graphics.$UI.$WXCore.$Types.$__55__2736__0,[$__,$__3.pointX,$__3.pointY]);});
$Graphics.$UI.$WXCore.$Types.$vecLength=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          var $__5=
           new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__101__0,$__2.vecY,$__2.vecY]);
          var $__6=
           new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__101__0,$__2.vecX,$__2.vecX]);
          var $__7=
           new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$__6,$__5]);
          var $__8=
           new _A_($UHC.$Base.$fromIntegral,[$UHC.$Base.$Integral__DCT74__110__0,$UHC.$Base.$Num__DCT74__177__0,$__7]);
          var $__9=
           new _A_($UHC.$Base.$sqrt,[$UHC.$Base.$Floating__DCT74__212__0,$__8]);
          return $__9;});
$Asteroids.$collide=
 new _F_(function($pos0,$pos1)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Types.$vecBetween,[$UHC.$Base.$Num__DCT74__101__0,$pos0,$pos1]);
          var $distance=
           new _A_($Graphics.$UI.$WXCore.$Types.$vecLength,[$__]);
          var $__5=
           new _A_($UHC.$Base.$fromIntegral,[$UHC.$Base.$Integral__DCT74__110__0,$UHC.$Base.$Num__DCT74__177__0,$Asteroids.$diameter]);
          return new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__166__0,$distance,$__5]);});
$Asteroids.$_24okUNQ134=
 new _F_(function($dc,$_24x,$_24x3)
         {var $positions=
           new _A_($UHC.$Base.$head,[$_24x]);
          var $__=
           new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__101__0,2,$Asteroids.$diameter]);
          var $__6=
           new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$Asteroids.$height,$__]);
          var $shipLocation=
           new _A_($Graphics.$UI.$WXCore.$Types.$point,[$UHC.$Base.$Num__DCT74__101__0,$_24x3,$__6]);
          var $__8=
           new _A_($Asteroids.$collide,[$shipLocation]);
          var $collisions=
           new _A_($UHC.$Base.$map,[$__8,$positions]);
          var $__10=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
          var $__11=
           new _A_($UHC.$Base.$or,[$collisions]);
          var $__12=
           new _A_($Control.$Monad.$when,[$UHC.$Base.$Monad__DCT74__339__0,$__11,$__10]);
          var $__13=
           new _A_($UHC.$Base.$zip,[$positions,$collisions]);
          var $__14=
           new _A_($Asteroids.$drawRock,[$dc]);
          var $__15=
           new _A_($UHC.$Base.$mapM,[$UHC.$Base.$Monad__DCT74__339__0,$__14,$__13]);
          var $__16=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__15,$__12]);
          var $__17=
           new _A_($Asteroids.$drawShip,[$dc,$shipLocation]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__17,$__16]);});
$Asteroids.$_24okUNQ124=
 new _F_(function($vship,$dc,$_24x)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Types.$varGet,[$vship]);
          var $__5=
           new _A_($Asteroids.$_24okUNQ134,[$dc,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__5]);});
$Asteroids.$draw=
 new _F_(function($vrocks,$vship,$dc,$__)
         {var $__5=
           new _A_($Graphics.$UI.$WXCore.$Types.$varGet,[$vrocks]);
          var $__6=
           new _A_($Asteroids.$_24okUNQ124,[$vship,$dc]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__5,$__6]);});
$Language.$UHC.$JS.$Marshal.$str=
 new _A_(new _F_(function()
                 {return new _A_($Language.$UHC.$JS.$Marshal.$toJS,[$Language.$UHC.$JS.$Marshal.$ToJS__DCT81__13__0]);}),[]);
$Graphics.$UI.$WX.$Attributes.$_3a_7e=
 new _F_(function($x1,$x2)
         {return {_tag_:3,_1:$x1,_2:$x2};});
$Graphics.$UI.$WXCore.$WindowClass.$__163__467=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["windowGetOnActivate not implemented"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowGetOnActivate=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WXCore.$WindowClass.$__163__467]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__163__896=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["windowOnActivate not implemented"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowOnActivate=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WXCore.$WindowClass.$__163__896]);}),[]);
$Graphics.$UI.$WX.$Window.$__229__227=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["activate"]);}),[]);
$Graphics.$UI.$WX.$Window.$Reactive__DCT225__33__0DFLGraphics_2eUI_2eWX_2eEvents_2eactivate=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Events.$newEvent,[$Graphics.$UI.$WX.$Window.$__229__227,$Graphics.$UI.$WXCore.$WindowClass.$windowGetOnActivate,$Graphics.$UI.$WXCore.$WindowClass.$windowOnActivate]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__163__165=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["windowGetOnMouse not implemented"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowGetOnMouse=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WXCore.$WindowClass.$__163__165]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__163__1284=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["windowOnMouse not implemented"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowOnMouse=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WXCore.$WindowClass.$__163__1284]);}),[]);
$Graphics.$UI.$WX.$Window.$__229__233__0=
 new _F_(function($w)
         {return new _A_($Graphics.$UI.$WXCore.$WindowClass.$windowOnMouse,[$w,$UHC.$Base.$True__]);});
$Graphics.$UI.$WX.$Window.$__229__232=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["mouse"]);}),[]);
$Graphics.$UI.$WX.$Window.$Reactive__DCT225__33__0DFLGraphics_2eUI_2eWX_2eEvents_2emouse=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Events.$newEvent,[$Graphics.$UI.$WX.$Window.$__229__232,$Graphics.$UI.$WXCore.$WindowClass.$windowGetOnMouse,$Graphics.$UI.$WX.$Window.$__229__233__0]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__163__235=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["windowGetOnFocus not implemented"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowGetOnFocus=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WXCore.$WindowClass.$__163__235]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__163__1410=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["windowOnFocus not implemented"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowOnFocus=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WXCore.$WindowClass.$__163__1410]);}),[]);
$Graphics.$UI.$WX.$Window.$__229__241=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["focus"]);}),[]);
$Graphics.$UI.$WX.$Window.$Reactive__DCT225__33__0DFLGraphics_2eUI_2eWX_2eEvents_2efocus=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Events.$newEvent,[$Graphics.$UI.$WX.$Window.$__229__241,$Graphics.$UI.$WXCore.$WindowClass.$windowGetOnFocus,$Graphics.$UI.$WXCore.$WindowClass.$windowOnFocus]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__163__487=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["windowGetOnClose not implemented"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowGetOnClose=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WXCore.$WindowClass.$__163__487]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__163__899=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["windowOnClose not implemented"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowOnClose=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WXCore.$WindowClass.$__163__899]);}),[]);
$Graphics.$UI.$WX.$Window.$__229__246=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["closing"]);}),[]);
$Graphics.$UI.$WX.$Window.$Reactive__DCT225__33__0DFLGraphics_2eUI_2eWX_2eEvents_2eclosing=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Events.$newEvent,[$Graphics.$UI.$WX.$Window.$__229__246,$Graphics.$UI.$WXCore.$WindowClass.$windowGetOnClose,$Graphics.$UI.$WXCore.$WindowClass.$windowOnClose]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__windowOnKeyChar=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__windowOnKeyChar;});
$Graphics.$UI.$WXCore.$WindowClass.$windowOnKeyChar=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$WindowClass.$__windowOnKeyChar,$Graphics.$UI.$WXCore.$WindowClass.$window__Methods]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__windowGetOnKeyChar=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__windowGetOnKeyChar;});
$Graphics.$UI.$WXCore.$WindowClass.$windowGetOnKeyChar=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$WindowClass.$__windowGetOnKeyChar,$Graphics.$UI.$WXCore.$WindowClass.$window__Methods]);}),[]);
$Graphics.$UI.$WX.$Window.$__229__251=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["keyboard"]);}),[]);
$Graphics.$UI.$WX.$Window.$Reactive__DCT225__33__0DFLGraphics_2eUI_2eWX_2eEvents_2ekeyboard=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Events.$newEvent,[$Graphics.$UI.$WX.$Window.$__229__251,$Graphics.$UI.$WXCore.$WindowClass.$windowGetOnKeyChar,$Graphics.$UI.$WXCore.$WindowClass.$windowOnKeyChar]);}),[]);
$Graphics.$UI.$WX.$Events.$Reactive__CLS213__2__0=
 new _F_(function($Reactive__)
         {var $Reactive__2=
           {_tag_:0,_1:$UHC.$Base.$undefined,_2:$UHC.$Base.$undefined,_3:$UHC.$Base.$undefined,_4:$UHC.$Base.$undefined,_5:$UHC.$Base.$undefined,_6:$UHC.$Base.$undefined,_7:$UHC.$Base.$undefined};
          return $Reactive__2;});
$Graphics.$UI.$WX.$Window.$Reactive__NEW173UNQ227EVLDCT225__33__0RDC=
 new _F_(function($Reactive__,$Reactive__2,$Reactive__3,$Reactive__4,$Reactive__5,$Reactive__6,$Reactive__7,$Reactive__8)
         {var $Reactive__9=
           _e_(new _A_($Graphics.$UI.$WX.$Events.$Reactive__CLS213__2__0,[$Reactive__3]));
          var $__17=
           {_tag_:0,_1:$Reactive__8,_2:$Reactive__5,_3:$Reactive__6,_4:$Reactive__,_5:$Reactive__4,_6:$Reactive__7,_7:$Reactive__2};
          return $__17;});
$Graphics.$UI.$WX.$Window.$Reactive__NEW164UNQ226DCT225__33__0RDC=
 new _F_(function($Reactive__,$Reactive__2,$Reactive__3,$Reactive__4,$Reactive__5,$Reactive__6,$Reactive__7,$Reactive__8)
         {var $Reactive__9=
           new _A_($Graphics.$UI.$WX.$Window.$Reactive__NEW173UNQ227EVLDCT225__33__0RDC,[$Reactive__,$Reactive__2,$Reactive__3,$Reactive__4,$Reactive__5,$Reactive__6,$Reactive__7,$Reactive__8]);
          return $Reactive__9;});
$Graphics.$UI.$WXCore.$WindowClass.$__163__146=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["windowGetOnSize not implemented"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowGetOnSize=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WXCore.$WindowClass.$__163__146]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__163__968=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["windowOnSize not implemented"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowOnSize=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WXCore.$WindowClass.$__163__968]);}),[]);
$Graphics.$UI.$WX.$Window.$__229__256=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["resize"]);}),[]);
$Graphics.$UI.$WX.$Window.$Reactive__DCT225__33__0DFLGraphics_2eUI_2eWX_2eEvents_2eresize=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Events.$newEvent,[$Graphics.$UI.$WX.$Window.$__229__256,$Graphics.$UI.$WXCore.$WindowClass.$windowGetOnSize,$Graphics.$UI.$WXCore.$WindowClass.$windowOnSize]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__163__484=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["windowGetOnIdle not implemented"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowGetOnIdle=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WXCore.$WindowClass.$__163__484]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__163__1396=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["windowOnIdle not implemented"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowOnIdle=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WXCore.$WindowClass.$__163__1396]);}),[]);
$Graphics.$UI.$WX.$Window.$__229__261=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["idle"]);}),[]);
$Graphics.$UI.$WX.$Window.$Reactive__DCT225__33__0DFLGraphics_2eUI_2eWX_2eEvents_2eidle=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Events.$newEvent,[$Graphics.$UI.$WX.$Window.$__229__261,$Graphics.$UI.$WXCore.$WindowClass.$windowGetOnIdle,$Graphics.$UI.$WXCore.$WindowClass.$windowOnIdle]);}),[]);
$Graphics.$UI.$WX.$Window.$Reactive__UNQ226DCT225__33__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Window.$Reactive__NEW164UNQ226DCT225__33__0RDC,[$Graphics.$UI.$WX.$Window.$Reactive__DCT225__33__0DFLGraphics_2eUI_2eWX_2eEvents_2eidle,$Graphics.$UI.$WX.$Window.$Reactive__DCT225__33__0DFLGraphics_2eUI_2eWX_2eEvents_2eresize,$Graphics.$UI.$WX.$Window.$Reactive__UNQ226DCT225__33__0RDC,$Graphics.$UI.$WX.$Window.$Reactive__DCT225__33__0DFLGraphics_2eUI_2eWX_2eEvents_2ekeyboard,$Graphics.$UI.$WX.$Window.$Reactive__DCT225__33__0DFLGraphics_2eUI_2eWX_2eEvents_2eclosing,$Graphics.$UI.$WX.$Window.$Reactive__DCT225__33__0DFLGraphics_2eUI_2eWX_2eEvents_2efocus,$Graphics.$UI.$WX.$Window.$Reactive__DCT225__33__0DFLGraphics_2eUI_2eWX_2eEvents_2emouse,$Graphics.$UI.$WX.$Window.$Reactive__DCT225__33__0DFLGraphics_2eUI_2eWX_2eEvents_2eactivate]);}),[]);
$Graphics.$UI.$WX.$Window.$Reactive__DCT225__33__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WX.$Window.$Reactive__UNQ226DCT225__33__0RDC;}),[]);
$Graphics.$UI.$WX.$Events.$rightKey=
 new _F_(function($__)
         {return new _A_($Graphics.$UI.$WX.$Events.$key,[$__,$Graphics.$UI.$WXCore.$Events.$KeyRight__]);});
$Asteroids.$__55__324__0=
 new _F_(function($i)
         {return new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__101__0,$i,2]);});
$UHC.$Base.$max=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._7;});
$Asteroids.$__55__280__0=
 new _F_(function($x)
         {var $__=
           new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$x,5]);
          return new _A_($UHC.$Base.$max,[$UHC.$Base.$Ord__DCT74__91__0,0,$__]);});
$Graphics.$UI.$WX.$Events.$paint=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$Data.$Char.$toUpper=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primCharToUpper($__2);});
$Graphics.$UI.$WX.$Events.$charKey=
 new _F_(function($__,$c)
         {var $__3=
           new _A_($Data.$Char.$toUpper,[$c]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$Events.$KeyChar__,[$__3]);
          return new _A_($Graphics.$UI.$WX.$Events.$key,[$__,$__4]);});
$UHC.$IOBase.$UserError__=
 new _A_(new _F_(function()
                 {return {_tag_:15};}),[]);
$UHC.$IOBase.$IOError__=
 new _F_(function($x1,$x2,$x3,$x4,$x5)
         {return {_tag_:0,ioe__handle:$x1,ioe__type:$x2,ioe__location:$x3,ioe__description:$x4,ioe__filename:$x5};});
$UHC.$IOBase.$userError=
 new _F_(function($str)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,[""]);
          return new _A_($UHC.$IOBase.$IOError__,[$UHC.$Base.$Nothing__,$UHC.$IOBase.$UserError__,$__,$str,$UHC.$Base.$Nothing__]);});
$UHC.$Base.$IOException__=
 new _F_(function($x1)
         {return {_tag_:8,_1:$x1};});
$UHC.$IOBase.$throwIOError=
 new _F_(function($e)
         {var $__=
           new _A_($UHC.$Base.$IOException__,[$e]);
          return new _A_($UHC.$Base.$throw,[$__]);});
$UHC.$IOBase.$ioError=
 new _F_(function($e,$s)
         {return new _A_($UHC.$IOBase.$throwIOError,[$e]);});
$Graphics.$UI.$WX.$Events.$getUNQ259=
 new _F_(function($name,$prev)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,[" event is write-only."]);
          var $__4=
           new _A_($UHC.$Base.$_2b_2b,[$name,$__]);
          var $__5=
           new _A_($UHC.$Base.$packedStringToString,["WX.Events: the "]);
          var $__6=
           new _A_($UHC.$Base.$_2b_2b,[$__5,$__4]);
          var $__7=
           new _A_($UHC.$IOBase.$userError,[$__6]);
          return new _A_($UHC.$IOBase.$ioError,[$__7]);});
$Graphics.$UI.$WX.$Events.$setUNQ258=
 new _F_(function($filter,$__,$prev,$new,$keyboardEvent)
         {var $__6=
           new _A_($prev,[$keyboardEvent]);
          var $__7=
           new _A_($filter,[$keyboardEvent]);
          var $__8=
           new _A_($Control.$Monad.$when,[$__,$__7,$new]);
          return new _A_($UHC.$Base.$_3e_3e,[$__,$__8,$__6]);});
$Graphics.$UI.$WX.$Attributes.$_24okUNQ631=
 new _F_(function($get,$_24x)
         {var $__=
           new _A_($get,[$_24x]);
          return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__]);});
$Graphics.$UI.$WX.$Attributes.$__85__680__0=
 new _F_(function($get,$getter,$w)
         {var $__=
           new _A_($getter,[$w]);
          var $__5=
           new _A_($Graphics.$UI.$WX.$Attributes.$_24okUNQ631,[$get]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__5]);});
$Graphics.$UI.$WX.$Attributes.$__85__722__0=
 new _F_(function($get,$set,$f,$a)
         {var $__=
           new _A_($get,[$a]);
          var $__6=
           new _A_($f,[$__]);
          return new _A_($set,[$a,$__6]);});
$Graphics.$UI.$WX.$Attributes.$_24okUNQ644=
 new _F_(function($get,$_24x)
         {var $__=
           new _A_($get,[$_24x]);
          return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__]);});
$Graphics.$UI.$WX.$Attributes.$__85__707__0=
 new _F_(function($get,$set,$updater,$w,$f)
         {var $__=
           new _A_($Graphics.$UI.$WX.$Attributes.$__85__722__0,[$get,$set,$f]);
          var $__7=
           new _A_($updater,[$w,$__]);
          var $__8=
           new _A_($Graphics.$UI.$WX.$Attributes.$_24okUNQ644,[$get]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__7,$__8]);});
$Graphics.$UI.$WX.$Attributes.$_24okUNQ638=
 new _F_(function($set,$setter,$w,$b,$_24x)
         {var $__=
           new _A_($set,[$_24x,$b]);
          return new _A_($setter,[$w,$__]);});
$Graphics.$UI.$WX.$Attributes.$__85__692__0=
 new _F_(function($set,$setter,$getter,$w,$b)
         {var $__=
           new _A_($getter,[$w]);
          var $__7=
           new _A_($Graphics.$UI.$WX.$Attributes.$_24okUNQ638,[$set,$setter,$w,$b]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__7]);});
$Graphics.$UI.$WX.$Attributes.$mapAttr=
 new _F_(function($get,$set,$__)
         {var $__4=
           _e_($__);
          var $__10=
           new _A_($Graphics.$UI.$WX.$Attributes.$__85__707__0,[$get,$set,$__4._5]);
          var $__11=
           new _A_($Graphics.$UI.$WX.$Attributes.$__85__692__0,[$set,$__4._4,$__4._3]);
          var $__12=
           new _A_($Graphics.$UI.$WX.$Attributes.$__85__680__0,[$get,$__4._3]);
          var $__13=
           new _A_($Graphics.$UI.$WX.$Attributes.$Attr__,[$__4._1,$UHC.$Base.$Nothing__,$__12,$__11,$__10]);
          return $__13;});
$Graphics.$UI.$WX.$Events.$mapEvent=
 new _F_(function($get,$set,$__)
         {var $__4=
           _e_($__);
          var $__6=
           new _A_($Graphics.$UI.$WX.$Attributes.$mapAttr,[$get,$set,$__4._1]);
          var $__7=
           new _A_($Graphics.$UI.$WX.$Events.$Event__,[$__6]);
          return $__7;});
$Graphics.$UI.$WX.$Events.$keyboard=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._5;});
$Graphics.$UI.$WX.$Events.$keyboardFilter=
 new _F_(function($__,$name,$filter)
         {var $__4=
           new _A_($Graphics.$UI.$WX.$Events.$keyboard,[$__]);
          var $__5=
           new _A_($Graphics.$UI.$WX.$Events.$setUNQ258,[$filter,$UHC.$Base.$Monad__DCT74__339__0]);
          var $__6=
           new _A_($Graphics.$UI.$WX.$Events.$getUNQ259,[$name]);
          return new _A_($Graphics.$UI.$WX.$Events.$mapEvent,[$__6,$__5,$__4]);});
$Graphics.$UI.$WXCore.$Events.$__99__12__0NEW1836UNQ1883EVLRDC=
 new _F_(function($__,$__2)
         {var $Eq__=
           _e_(new _A_($UHC.$Base.$Eq__CLS74__4__0,[$__]));
          var $__6=
           {_tag_:0,_1:$Eq__._1,_2:$__2};
          return $__6;});
$Graphics.$UI.$WXCore.$Events.$__99__12__0NEW1833UNQ1649RDC=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($Graphics.$UI.$WXCore.$Events.$__99__12__0NEW1836UNQ1883EVLRDC,[$__,$__2]);
          return $__3;});
$Graphics.$UI.$WXCore.$Events.$KeyMiddleButton__=
 new _A_(new _F_(function()
                 {return {_tag_:45};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyDivide__=
 new _A_(new _F_(function()
                 {return {_tag_:10};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyScroll__=
 new _A_(new _F_(function()
                 {return {_tag_:66};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeySnapshot__=
 new _A_(new _F_(function()
                 {return {_tag_:70};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyUp__=
 new _A_(new _F_(function()
                 {return {_tag_:75};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF8__=
 new _A_(new _F_(function()
                 {return {_tag_:37};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF9__=
 new _A_(new _F_(function()
                 {return {_tag_:38};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF6__=
 new _A_(new _F_(function()
                 {return {_tag_:35};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF7__=
 new _A_(new _F_(function()
                 {return {_tag_:36};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF4__=
 new _A_(new _F_(function()
                 {return {_tag_:33};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF5__=
 new _A_(new _F_(function()
                 {return {_tag_:34};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF2__=
 new _A_(new _F_(function()
                 {return {_tag_:26};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF3__=
 new _A_(new _F_(function()
                 {return {_tag_:32};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF1__=
 new _A_(new _F_(function()
                 {return {_tag_:15};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyReturn__=
 new _A_(new _F_(function()
                 {return {_tag_:63};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyStart__=
 new _A_(new _F_(function()
                 {return {_tag_:72};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeySeparator__=
 new _A_(new _F_(function()
                 {return {_tag_:68};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyShift__=
 new _A_(new _F_(function()
                 {return {_tag_:69};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyClear__=
 new _A_(new _F_(function()
                 {return {_tag_:6};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyRight__=
 new _A_(new _F_(function()
                 {return {_tag_:64};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyHelp__=
 new _A_(new _F_(function()
                 {return {_tag_:39};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyDelete__=
 new _A_(new _F_(function()
                 {return {_tag_:9};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyDown__=
 new _A_(new _F_(function()
                 {return {_tag_:11};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeySelect__=
 new _A_(new _F_(function()
                 {return {_tag_:67};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyChar__=
 new _F_(function($x1)
         {return {_tag_:5,_1:$x1};});
$Graphics.$UI.$WXCore.$Events.$KeyNum8__=
 new _A_(new _F_(function()
                 {return {_tag_:55};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyNum9__=
 new _A_(new _F_(function()
                 {return {_tag_:56};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyNum0__=
 new _A_(new _F_(function()
                 {return {_tag_:47};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyNum1__=
 new _A_(new _F_(function()
                 {return {_tag_:48};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyNum2__=
 new _A_(new _F_(function()
                 {return {_tag_:49};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyNum3__=
 new _A_(new _F_(function()
                 {return {_tag_:50};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyNum4__=
 new _A_(new _F_(function()
                 {return {_tag_:51};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyNum5__=
 new _A_(new _F_(function()
                 {return {_tag_:52};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyNum6__=
 new _A_(new _F_(function()
                 {return {_tag_:53};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyNum7__=
 new _A_(new _F_(function()
                 {return {_tag_:54};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyHome__=
 new _A_(new _F_(function()
                 {return {_tag_:40};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyPageUp__=
 new _A_(new _F_(function()
                 {return {_tag_:60};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyBack__=
 new _A_(new _F_(function()
                 {return {_tag_:2};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyPause__=
 new _A_(new _F_(function()
                 {return {_tag_:61};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyMultiply__=
 new _A_(new _F_(function()
                 {return {_tag_:46};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeySpace__=
 new _A_(new _F_(function()
                 {return {_tag_:71};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyRightButton__=
 new _A_(new _F_(function()
                 {return {_tag_:65};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyOther__=
 new _F_(function($x1)
         {return {_tag_:58,_1:$x1};});
$Graphics.$UI.$WXCore.$Events.$KeyF15__=
 new _A_(new _F_(function()
                 {return {_tag_:21};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF14__=
 new _A_(new _F_(function()
                 {return {_tag_:20};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF17__=
 new _A_(new _F_(function()
                 {return {_tag_:23};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF16__=
 new _A_(new _F_(function()
                 {return {_tag_:22};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF11__=
 new _A_(new _F_(function()
                 {return {_tag_:17};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF10__=
 new _A_(new _F_(function()
                 {return {_tag_:16};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF13__=
 new _A_(new _F_(function()
                 {return {_tag_:19};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF12__=
 new _A_(new _F_(function()
                 {return {_tag_:18};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF19__=
 new _A_(new _F_(function()
                 {return {_tag_:25};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF18__=
 new _A_(new _F_(function()
                 {return {_tag_:24};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF20__=
 new _A_(new _F_(function()
                 {return {_tag_:27};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF21__=
 new _A_(new _F_(function()
                 {return {_tag_:28};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF22__=
 new _A_(new _F_(function()
                 {return {_tag_:29};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF23__=
 new _A_(new _F_(function()
                 {return {_tag_:30};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyF24__=
 new _A_(new _F_(function()
                 {return {_tag_:31};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyEscape__=
 new _A_(new _F_(function()
                 {return {_tag_:13};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyAdd__=
 new _A_(new _F_(function()
                 {return {_tag_:0};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyAlt__=
 new _A_(new _F_(function()
                 {return {_tag_:1};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyExecute__=
 new _A_(new _F_(function()
                 {return {_tag_:14};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyPrint__=
 new _A_(new _F_(function()
                 {return {_tag_:62};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyEnd__=
 new _A_(new _F_(function()
                 {return {_tag_:12};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyLeftButton__=
 new _A_(new _F_(function()
                 {return {_tag_:43};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyCapital__=
 new _A_(new _F_(function()
                 {return {_tag_:4};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyInsert__=
 new _A_(new _F_(function()
                 {return {_tag_:41};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyDecimal__=
 new _A_(new _F_(function()
                 {return {_tag_:8};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyPageDown__=
 new _A_(new _F_(function()
                 {return {_tag_:59};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeySubtract__=
 new _A_(new _F_(function()
                 {return {_tag_:73};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyMenu__=
 new _A_(new _F_(function()
                 {return {_tag_:44};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyTab__=
 new _A_(new _F_(function()
                 {return {_tag_:74};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyNumLock__=
 new _A_(new _F_(function()
                 {return {_tag_:57};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyControl__=
 new _A_(new _F_(function()
                 {return {_tag_:7};}),[]);
$Graphics.$UI.$WXCore.$Events.$KeyCancel__=
 new _A_(new _F_(function()
                 {return {_tag_:3};}),[]);
$Graphics.$UI.$WXCore.$Events.$__Rep0KeyDFLUHC_2eBase_2eto0GENRepresentable0=
 new _F_(function($proj__1)
         {var $proj__2=
           _e_($proj__1);
          var $__swJSW302__0;
          switch($proj__2._tag_)
           {case 0:
             var $proj__34=
              _e_($proj__2.unL1);
             var $__swJSW303__0;
             switch($proj__34._tag_)
              {case 0:
                var $proj__46=
                 _e_($proj__34.unL1);
                var $__swJSW304__0;
                switch($proj__46._tag_)
                 {case 0:
                   var $proj__58=
                    _e_($proj__46.unL1);
                   var $__swJSW305__0;
                   switch($proj__58._tag_)
                    {case 0:
                      var $proj__610=
                       _e_($proj__58.unL1);
                      var $__swJSW306__0;
                      switch($proj__610._tag_)
                       {case 0:
                         var $proj__712=
                          _e_($proj__610.unL1);
                         var $__swJSW307__0;
                         switch($proj__712._tag_)
                          {case 0:
                            var $__=
                             new _A_($Graphics.$UI.$WXCore.$Events.$KeyChar__,[$proj__712.unL1]);
                            $__swJSW307__0=
                             $__;
                            break;
                           case 1:
                            var $__=
                             new _A_($Graphics.$UI.$WXCore.$Events.$KeyOther__,[$proj__712.unR1]);
                            $__swJSW307__0=
                             $__;
                            break;}
                         $__swJSW306__0=
                          $__swJSW307__0;
                         break;
                        case 1:
                         var $proj__1418=
                          _e_($proj__610.unR1);
                         var $__swJSW308__0;
                         switch($proj__1418._tag_)
                          {case 0:
                            var $proj__16=
                             _e_($proj__1418.unL1);
                            $__swJSW308__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyBack__;
                            break;
                           case 1:
                            var $proj__18=
                             _e_($proj__1418.unR1);
                            $__swJSW308__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyTab__;
                            break;}
                         $__swJSW306__0=
                          $__swJSW308__0;
                         break;}
                      $__swJSW305__0=
                       $__swJSW306__0;
                      break;
                     case 1:
                      var $proj__1924=
                       _e_($proj__58.unR1);
                      var $__swJSW311__0;
                      switch($proj__1924._tag_)
                       {case 0:
                         var $proj__2026=
                          _e_($proj__1924.unL1);
                         var $__swJSW312__0;
                         switch($proj__2026._tag_)
                          {case 0:
                            var $proj__22=
                             _e_($proj__2026.unL1);
                            $__swJSW312__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyReturn__;
                            break;
                           case 1:
                            var $proj__24=
                             _e_($proj__2026.unR1);
                            $__swJSW312__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyEscape__;
                            break;}
                         $__swJSW311__0=
                          $__swJSW312__0;
                         break;
                        case 1:
                         var $proj__2532=
                          _e_($proj__1924.unR1);
                         var $__swJSW315__0;
                         switch($proj__2532._tag_)
                          {case 0:
                            var $proj__27=
                             _e_($proj__2532.unL1);
                            $__swJSW315__0=
                             $Graphics.$UI.$WXCore.$Events.$KeySpace__;
                            break;
                           case 1:
                            var $proj__2836=
                             _e_($proj__2532.unR1);
                            var $__swJSW317__0;
                            switch($proj__2836._tag_)
                             {case 0:
                               var $proj__30=
                                _e_($proj__2836.unL1);
                               $__swJSW317__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyDelete__;
                               break;
                              case 1:
                               var $proj__32=
                                _e_($proj__2836.unR1);
                               $__swJSW317__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyInsert__;
                               break;}
                            $__swJSW315__0=
                             $__swJSW317__0;
                            break;}
                         $__swJSW311__0=
                          $__swJSW315__0;
                         break;}
                      $__swJSW305__0=
                       $__swJSW311__0;
                      break;}
                   $__swJSW304__0=
                    $__swJSW305__0;
                   break;
                  case 1:
                   var $proj__3342=
                    _e_($proj__46.unR1);
                   var $__swJSW320__0;
                   switch($proj__3342._tag_)
                    {case 0:
                      var $proj__3444=
                       _e_($proj__3342.unL1);
                      var $__swJSW321__0;
                      switch($proj__3444._tag_)
                       {case 0:
                         var $proj__3546=
                          _e_($proj__3444.unL1);
                         var $__swJSW322__0;
                         switch($proj__3546._tag_)
                          {case 0:
                            var $proj__37=
                             _e_($proj__3546.unL1);
                            $__swJSW322__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyEnd__;
                            break;
                           case 1:
                            var $proj__39=
                             _e_($proj__3546.unR1);
                            $__swJSW322__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyHome__;
                            break;}
                         $__swJSW321__0=
                          $__swJSW322__0;
                         break;
                        case 1:
                         var $proj__4052=
                          _e_($proj__3444.unR1);
                         var $__swJSW325__0;
                         switch($proj__4052._tag_)
                          {case 0:
                            var $proj__42=
                             _e_($proj__4052.unL1);
                            $__swJSW325__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyLeft__;
                            break;
                           case 1:
                            var $proj__4356=
                             _e_($proj__4052.unR1);
                            var $__swJSW327__0;
                            switch($proj__4356._tag_)
                             {case 0:
                               var $proj__45=
                                _e_($proj__4356.unL1);
                               $__swJSW327__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyUp__;
                               break;
                              case 1:
                               var $proj__47=
                                _e_($proj__4356.unR1);
                               $__swJSW327__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyRight__;
                               break;}
                            $__swJSW325__0=
                             $__swJSW327__0;
                            break;}
                         $__swJSW321__0=
                          $__swJSW325__0;
                         break;}
                      $__swJSW320__0=
                       $__swJSW321__0;
                      break;
                     case 1:
                      var $proj__4862=
                       _e_($proj__3342.unR1);
                      var $__swJSW330__0;
                      switch($proj__4862._tag_)
                       {case 0:
                         var $proj__4964=
                          _e_($proj__4862.unL1);
                         var $__swJSW331__0;
                         switch($proj__4964._tag_)
                          {case 0:
                            var $proj__51=
                             _e_($proj__4964.unL1);
                            $__swJSW331__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyDown__;
                            break;
                           case 1:
                            var $proj__53=
                             _e_($proj__4964.unR1);
                            $__swJSW331__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyPageUp__;
                            break;}
                         $__swJSW330__0=
                          $__swJSW331__0;
                         break;
                        case 1:
                         var $proj__5470=
                          _e_($proj__4862.unR1);
                         var $__swJSW334__0;
                         switch($proj__5470._tag_)
                          {case 0:
                            var $proj__56=
                             _e_($proj__5470.unL1);
                            $__swJSW334__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyPageDown__;
                            break;
                           case 1:
                            var $proj__5774=
                             _e_($proj__5470.unR1);
                            var $__swJSW336__0;
                            switch($proj__5774._tag_)
                             {case 0:
                               var $proj__59=
                                _e_($proj__5774.unL1);
                               $__swJSW336__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyStart__;
                               break;
                              case 1:
                               var $proj__61=
                                _e_($proj__5774.unR1);
                               $__swJSW336__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyClear__;
                               break;}
                            $__swJSW334__0=
                             $__swJSW336__0;
                            break;}
                         $__swJSW330__0=
                          $__swJSW334__0;
                         break;}
                      $__swJSW320__0=
                       $__swJSW330__0;
                      break;}
                   $__swJSW304__0=
                    $__swJSW320__0;
                   break;}
                $__swJSW303__0=
                 $__swJSW304__0;
                break;
               case 1:
                var $proj__6280=
                 _e_($proj__34.unR1);
                var $__swJSW339__0;
                switch($proj__6280._tag_)
                 {case 0:
                   var $proj__6382=
                    _e_($proj__6280.unL1);
                   var $__swJSW340__0;
                   switch($proj__6382._tag_)
                    {case 0:
                      var $proj__6484=
                       _e_($proj__6382.unL1);
                      var $__swJSW341__0;
                      switch($proj__6484._tag_)
                       {case 0:
                         var $proj__6586=
                          _e_($proj__6484.unL1);
                         var $__swJSW342__0;
                         switch($proj__6586._tag_)
                          {case 0:
                            var $proj__67=
                             _e_($proj__6586.unL1);
                            $__swJSW342__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyShift__;
                            break;
                           case 1:
                            var $proj__69=
                             _e_($proj__6586.unR1);
                            $__swJSW342__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyAlt__;
                            break;}
                         $__swJSW341__0=
                          $__swJSW342__0;
                         break;
                        case 1:
                         var $proj__7092=
                          _e_($proj__6484.unR1);
                         var $__swJSW345__0;
                         switch($proj__7092._tag_)
                          {case 0:
                            var $proj__72=
                             _e_($proj__7092.unL1);
                            $__swJSW345__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyControl__;
                            break;
                           case 1:
                            var $proj__74=
                             _e_($proj__7092.unR1);
                            $__swJSW345__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyMenu__;
                            break;}
                         $__swJSW341__0=
                          $__swJSW345__0;
                         break;}
                      $__swJSW340__0=
                       $__swJSW341__0;
                      break;
                     case 1:
                      var $proj__7598=
                       _e_($proj__6382.unR1);
                      var $__swJSW348__0;
                      switch($proj__7598._tag_)
                       {case 0:
                         var $proj__76100=
                          _e_($proj__7598.unL1);
                         var $__swJSW349__0;
                         switch($proj__76100._tag_)
                          {case 0:
                            var $proj__78=
                             _e_($proj__76100.unL1);
                            $__swJSW349__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyPause__;
                            break;
                           case 1:
                            var $proj__80=
                             _e_($proj__76100.unR1);
                            $__swJSW349__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyCapital__;
                            break;}
                         $__swJSW348__0=
                          $__swJSW349__0;
                         break;
                        case 1:
                         var $proj__81106=
                          _e_($proj__7598.unR1);
                         var $__swJSW352__0;
                         switch($proj__81106._tag_)
                          {case 0:
                            var $proj__83=
                             _e_($proj__81106.unL1);
                            $__swJSW352__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyHelp__;
                            break;
                           case 1:
                            var $proj__84110=
                             _e_($proj__81106.unR1);
                            var $__swJSW354__0;
                            switch($proj__84110._tag_)
                             {case 0:
                               var $proj__86=
                                _e_($proj__84110.unL1);
                               $__swJSW354__0=
                                $Graphics.$UI.$WXCore.$Events.$KeySelect__;
                               break;
                              case 1:
                               var $proj__88=
                                _e_($proj__84110.unR1);
                               $__swJSW354__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyPrint__;
                               break;}
                            $__swJSW352__0=
                             $__swJSW354__0;
                            break;}
                         $__swJSW348__0=
                          $__swJSW352__0;
                         break;}
                      $__swJSW340__0=
                       $__swJSW348__0;
                      break;}
                   $__swJSW339__0=
                    $__swJSW340__0;
                   break;
                  case 1:
                   var $proj__89116=
                    _e_($proj__6280.unR1);
                   var $__swJSW357__0;
                   switch($proj__89116._tag_)
                    {case 0:
                      var $proj__90118=
                       _e_($proj__89116.unL1);
                      var $__swJSW358__0;
                      switch($proj__90118._tag_)
                       {case 0:
                         var $proj__91120=
                          _e_($proj__90118.unL1);
                         var $__swJSW359__0;
                         switch($proj__91120._tag_)
                          {case 0:
                            var $proj__93=
                             _e_($proj__91120.unL1);
                            $__swJSW359__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyExecute__;
                            break;
                           case 1:
                            var $proj__95=
                             _e_($proj__91120.unR1);
                            $__swJSW359__0=
                             $Graphics.$UI.$WXCore.$Events.$KeySnapshot__;
                            break;}
                         $__swJSW358__0=
                          $__swJSW359__0;
                         break;
                        case 1:
                         var $proj__96126=
                          _e_($proj__90118.unR1);
                         var $__swJSW362__0;
                         switch($proj__96126._tag_)
                          {case 0:
                            var $proj__98=
                             _e_($proj__96126.unL1);
                            $__swJSW362__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyCancel__;
                            break;
                           case 1:
                            var $proj__99130=
                             _e_($proj__96126.unR1);
                            var $__swJSW364__0;
                            switch($proj__99130._tag_)
                             {case 0:
                               var $proj__101=
                                _e_($proj__99130.unL1);
                               $__swJSW364__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyLeftButton__;
                               break;
                              case 1:
                               var $proj__103=
                                _e_($proj__99130.unR1);
                               $__swJSW364__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyRightButton__;
                               break;}
                            $__swJSW362__0=
                             $__swJSW364__0;
                            break;}
                         $__swJSW358__0=
                          $__swJSW362__0;
                         break;}
                      $__swJSW357__0=
                       $__swJSW358__0;
                      break;
                     case 1:
                      var $proj__104136=
                       _e_($proj__89116.unR1);
                      var $__swJSW367__0;
                      switch($proj__104136._tag_)
                       {case 0:
                         var $proj__105138=
                          _e_($proj__104136.unL1);
                         var $__swJSW368__0;
                         switch($proj__105138._tag_)
                          {case 0:
                            var $proj__107=
                             _e_($proj__105138.unL1);
                            $__swJSW368__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyMiddleButton__;
                            break;
                           case 1:
                            var $proj__109=
                             _e_($proj__105138.unR1);
                            $__swJSW368__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyNum0__;
                            break;}
                         $__swJSW367__0=
                          $__swJSW368__0;
                         break;
                        case 1:
                         var $proj__110144=
                          _e_($proj__104136.unR1);
                         var $__swJSW371__0;
                         switch($proj__110144._tag_)
                          {case 0:
                            var $proj__112=
                             _e_($proj__110144.unL1);
                            $__swJSW371__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyNum1__;
                            break;
                           case 1:
                            var $proj__113148=
                             _e_($proj__110144.unR1);
                            var $__swJSW373__0;
                            switch($proj__113148._tag_)
                             {case 0:
                               var $proj__115=
                                _e_($proj__113148.unL1);
                               $__swJSW373__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyNum2__;
                               break;
                              case 1:
                               var $proj__117=
                                _e_($proj__113148.unR1);
                               $__swJSW373__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyNum3__;
                               break;}
                            $__swJSW371__0=
                             $__swJSW373__0;
                            break;}
                         $__swJSW367__0=
                          $__swJSW371__0;
                         break;}
                      $__swJSW357__0=
                       $__swJSW367__0;
                      break;}
                   $__swJSW339__0=
                    $__swJSW357__0;
                   break;}
                $__swJSW303__0=
                 $__swJSW339__0;
                break;}
             $__swJSW302__0=
              $__swJSW303__0;
             break;
            case 1:
             var $proj__118154=
              _e_($proj__2.unR1);
             var $__swJSW376__0;
             switch($proj__118154._tag_)
              {case 0:
                var $proj__119156=
                 _e_($proj__118154.unL1);
                var $__swJSW377__0;
                switch($proj__119156._tag_)
                 {case 0:
                   var $proj__120158=
                    _e_($proj__119156.unL1);
                   var $__swJSW378__0;
                   switch($proj__120158._tag_)
                    {case 0:
                      var $proj__121160=
                       _e_($proj__120158.unL1);
                      var $__swJSW379__0;
                      switch($proj__121160._tag_)
                       {case 0:
                         var $proj__122162=
                          _e_($proj__121160.unL1);
                         var $__swJSW380__0;
                         switch($proj__122162._tag_)
                          {case 0:
                            var $proj__124=
                             _e_($proj__122162.unL1);
                            $__swJSW380__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyNum4__;
                            break;
                           case 1:
                            var $proj__126=
                             _e_($proj__122162.unR1);
                            $__swJSW380__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyNum5__;
                            break;}
                         $__swJSW379__0=
                          $__swJSW380__0;
                         break;
                        case 1:
                         var $proj__127168=
                          _e_($proj__121160.unR1);
                         var $__swJSW383__0;
                         switch($proj__127168._tag_)
                          {case 0:
                            var $proj__129=
                             _e_($proj__127168.unL1);
                            $__swJSW383__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyNum6__;
                            break;
                           case 1:
                            var $proj__131=
                             _e_($proj__127168.unR1);
                            $__swJSW383__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyNum7__;
                            break;}
                         $__swJSW379__0=
                          $__swJSW383__0;
                         break;}
                      $__swJSW378__0=
                       $__swJSW379__0;
                      break;
                     case 1:
                      var $proj__132174=
                       _e_($proj__120158.unR1);
                      var $__swJSW386__0;
                      switch($proj__132174._tag_)
                       {case 0:
                         var $proj__133176=
                          _e_($proj__132174.unL1);
                         var $__swJSW387__0;
                         switch($proj__133176._tag_)
                          {case 0:
                            var $proj__135=
                             _e_($proj__133176.unL1);
                            $__swJSW387__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyNum8__;
                            break;
                           case 1:
                            var $proj__137=
                             _e_($proj__133176.unR1);
                            $__swJSW387__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyNum9__;
                            break;}
                         $__swJSW386__0=
                          $__swJSW387__0;
                         break;
                        case 1:
                         var $proj__138182=
                          _e_($proj__132174.unR1);
                         var $__swJSW390__0;
                         switch($proj__138182._tag_)
                          {case 0:
                            var $proj__140=
                             _e_($proj__138182.unL1);
                            $__swJSW390__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyMultiply__;
                            break;
                           case 1:
                            var $proj__141186=
                             _e_($proj__138182.unR1);
                            var $__swJSW392__0;
                            switch($proj__141186._tag_)
                             {case 0:
                               var $proj__143=
                                _e_($proj__141186.unL1);
                               $__swJSW392__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyAdd__;
                               break;
                              case 1:
                               var $proj__145=
                                _e_($proj__141186.unR1);
                               $__swJSW392__0=
                                $Graphics.$UI.$WXCore.$Events.$KeySeparator__;
                               break;}
                            $__swJSW390__0=
                             $__swJSW392__0;
                            break;}
                         $__swJSW386__0=
                          $__swJSW390__0;
                         break;}
                      $__swJSW378__0=
                       $__swJSW386__0;
                      break;}
                   $__swJSW377__0=
                    $__swJSW378__0;
                   break;
                  case 1:
                   var $proj__146192=
                    _e_($proj__119156.unR1);
                   var $__swJSW395__0;
                   switch($proj__146192._tag_)
                    {case 0:
                      var $proj__147194=
                       _e_($proj__146192.unL1);
                      var $__swJSW396__0;
                      switch($proj__147194._tag_)
                       {case 0:
                         var $proj__148196=
                          _e_($proj__147194.unL1);
                         var $__swJSW397__0;
                         switch($proj__148196._tag_)
                          {case 0:
                            var $proj__150=
                             _e_($proj__148196.unL1);
                            $__swJSW397__0=
                             $Graphics.$UI.$WXCore.$Events.$KeySubtract__;
                            break;
                           case 1:
                            var $proj__152=
                             _e_($proj__148196.unR1);
                            $__swJSW397__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyDecimal__;
                            break;}
                         $__swJSW396__0=
                          $__swJSW397__0;
                         break;
                        case 1:
                         var $proj__153202=
                          _e_($proj__147194.unR1);
                         var $__swJSW400__0;
                         switch($proj__153202._tag_)
                          {case 0:
                            var $proj__155=
                             _e_($proj__153202.unL1);
                            $__swJSW400__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyDivide__;
                            break;
                           case 1:
                            var $proj__156206=
                             _e_($proj__153202.unR1);
                            var $__swJSW402__0;
                            switch($proj__156206._tag_)
                             {case 0:
                               var $proj__158=
                                _e_($proj__156206.unL1);
                               $__swJSW402__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyF1__;
                               break;
                              case 1:
                               var $proj__160=
                                _e_($proj__156206.unR1);
                               $__swJSW402__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyF2__;
                               break;}
                            $__swJSW400__0=
                             $__swJSW402__0;
                            break;}
                         $__swJSW396__0=
                          $__swJSW400__0;
                         break;}
                      $__swJSW395__0=
                       $__swJSW396__0;
                      break;
                     case 1:
                      var $proj__161212=
                       _e_($proj__146192.unR1);
                      var $__swJSW405__0;
                      switch($proj__161212._tag_)
                       {case 0:
                         var $proj__162214=
                          _e_($proj__161212.unL1);
                         var $__swJSW406__0;
                         switch($proj__162214._tag_)
                          {case 0:
                            var $proj__164=
                             _e_($proj__162214.unL1);
                            $__swJSW406__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyF3__;
                            break;
                           case 1:
                            var $proj__166=
                             _e_($proj__162214.unR1);
                            $__swJSW406__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyF4__;
                            break;}
                         $__swJSW405__0=
                          $__swJSW406__0;
                         break;
                        case 1:
                         var $proj__167220=
                          _e_($proj__161212.unR1);
                         var $__swJSW409__0;
                         switch($proj__167220._tag_)
                          {case 0:
                            var $proj__169=
                             _e_($proj__167220.unL1);
                            $__swJSW409__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyF5__;
                            break;
                           case 1:
                            var $proj__170224=
                             _e_($proj__167220.unR1);
                            var $__swJSW411__0;
                            switch($proj__170224._tag_)
                             {case 0:
                               var $proj__172=
                                _e_($proj__170224.unL1);
                               $__swJSW411__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyF6__;
                               break;
                              case 1:
                               var $proj__174=
                                _e_($proj__170224.unR1);
                               $__swJSW411__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyF7__;
                               break;}
                            $__swJSW409__0=
                             $__swJSW411__0;
                            break;}
                         $__swJSW405__0=
                          $__swJSW409__0;
                         break;}
                      $__swJSW395__0=
                       $__swJSW405__0;
                      break;}
                   $__swJSW377__0=
                    $__swJSW395__0;
                   break;}
                $__swJSW376__0=
                 $__swJSW377__0;
                break;
               case 1:
                var $proj__175230=
                 _e_($proj__118154.unR1);
                var $__swJSW414__0;
                switch($proj__175230._tag_)
                 {case 0:
                   var $proj__176232=
                    _e_($proj__175230.unL1);
                   var $__swJSW415__0;
                   switch($proj__176232._tag_)
                    {case 0:
                      var $proj__177234=
                       _e_($proj__176232.unL1);
                      var $__swJSW416__0;
                      switch($proj__177234._tag_)
                       {case 0:
                         var $proj__178236=
                          _e_($proj__177234.unL1);
                         var $__swJSW417__0;
                         switch($proj__178236._tag_)
                          {case 0:
                            var $proj__180=
                             _e_($proj__178236.unL1);
                            $__swJSW417__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyF8__;
                            break;
                           case 1:
                            var $proj__182=
                             _e_($proj__178236.unR1);
                            $__swJSW417__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyF9__;
                            break;}
                         $__swJSW416__0=
                          $__swJSW417__0;
                         break;
                        case 1:
                         var $proj__183242=
                          _e_($proj__177234.unR1);
                         var $__swJSW420__0;
                         switch($proj__183242._tag_)
                          {case 0:
                            var $proj__185=
                             _e_($proj__183242.unL1);
                            $__swJSW420__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyF10__;
                            break;
                           case 1:
                            var $proj__187=
                             _e_($proj__183242.unR1);
                            $__swJSW420__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyF11__;
                            break;}
                         $__swJSW416__0=
                          $__swJSW420__0;
                         break;}
                      $__swJSW415__0=
                       $__swJSW416__0;
                      break;
                     case 1:
                      var $proj__188248=
                       _e_($proj__176232.unR1);
                      var $__swJSW423__0;
                      switch($proj__188248._tag_)
                       {case 0:
                         var $proj__189250=
                          _e_($proj__188248.unL1);
                         var $__swJSW424__0;
                         switch($proj__189250._tag_)
                          {case 0:
                            var $proj__191=
                             _e_($proj__189250.unL1);
                            $__swJSW424__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyF12__;
                            break;
                           case 1:
                            var $proj__193=
                             _e_($proj__189250.unR1);
                            $__swJSW424__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyF13__;
                            break;}
                         $__swJSW423__0=
                          $__swJSW424__0;
                         break;
                        case 1:
                         var $proj__194256=
                          _e_($proj__188248.unR1);
                         var $__swJSW427__0;
                         switch($proj__194256._tag_)
                          {case 0:
                            var $proj__196=
                             _e_($proj__194256.unL1);
                            $__swJSW427__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyF14__;
                            break;
                           case 1:
                            var $proj__197260=
                             _e_($proj__194256.unR1);
                            var $__swJSW429__0;
                            switch($proj__197260._tag_)
                             {case 0:
                               var $proj__199=
                                _e_($proj__197260.unL1);
                               $__swJSW429__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyF15__;
                               break;
                              case 1:
                               var $proj__201=
                                _e_($proj__197260.unR1);
                               $__swJSW429__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyF16__;
                               break;}
                            $__swJSW427__0=
                             $__swJSW429__0;
                            break;}
                         $__swJSW423__0=
                          $__swJSW427__0;
                         break;}
                      $__swJSW415__0=
                       $__swJSW423__0;
                      break;}
                   $__swJSW414__0=
                    $__swJSW415__0;
                   break;
                  case 1:
                   var $proj__202266=
                    _e_($proj__175230.unR1);
                   var $__swJSW432__0;
                   switch($proj__202266._tag_)
                    {case 0:
                      var $proj__203268=
                       _e_($proj__202266.unL1);
                      var $__swJSW433__0;
                      switch($proj__203268._tag_)
                       {case 0:
                         var $proj__204270=
                          _e_($proj__203268.unL1);
                         var $__swJSW434__0;
                         switch($proj__204270._tag_)
                          {case 0:
                            var $proj__206=
                             _e_($proj__204270.unL1);
                            $__swJSW434__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyF17__;
                            break;
                           case 1:
                            var $proj__208=
                             _e_($proj__204270.unR1);
                            $__swJSW434__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyF18__;
                            break;}
                         $__swJSW433__0=
                          $__swJSW434__0;
                         break;
                        case 1:
                         var $proj__209276=
                          _e_($proj__203268.unR1);
                         var $__swJSW437__0;
                         switch($proj__209276._tag_)
                          {case 0:
                            var $proj__211=
                             _e_($proj__209276.unL1);
                            $__swJSW437__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyF19__;
                            break;
                           case 1:
                            var $proj__212280=
                             _e_($proj__209276.unR1);
                            var $__swJSW439__0;
                            switch($proj__212280._tag_)
                             {case 0:
                               var $proj__214=
                                _e_($proj__212280.unL1);
                               $__swJSW439__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyF20__;
                               break;
                              case 1:
                               var $proj__216=
                                _e_($proj__212280.unR1);
                               $__swJSW439__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyF21__;
                               break;}
                            $__swJSW437__0=
                             $__swJSW439__0;
                            break;}
                         $__swJSW433__0=
                          $__swJSW437__0;
                         break;}
                      $__swJSW432__0=
                       $__swJSW433__0;
                      break;
                     case 1:
                      var $proj__217286=
                       _e_($proj__202266.unR1);
                      var $__swJSW442__0;
                      switch($proj__217286._tag_)
                       {case 0:
                         var $proj__218288=
                          _e_($proj__217286.unL1);
                         var $__swJSW443__0;
                         switch($proj__218288._tag_)
                          {case 0:
                            var $proj__220=
                             _e_($proj__218288.unL1);
                            $__swJSW443__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyF22__;
                            break;
                           case 1:
                            var $proj__222=
                             _e_($proj__218288.unR1);
                            $__swJSW443__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyF23__;
                            break;}
                         $__swJSW442__0=
                          $__swJSW443__0;
                         break;
                        case 1:
                         var $proj__223294=
                          _e_($proj__217286.unR1);
                         var $__swJSW446__0;
                         switch($proj__223294._tag_)
                          {case 0:
                            var $proj__225=
                             _e_($proj__223294.unL1);
                            $__swJSW446__0=
                             $Graphics.$UI.$WXCore.$Events.$KeyF24__;
                            break;
                           case 1:
                            var $proj__226298=
                             _e_($proj__223294.unR1);
                            var $__swJSW448__0;
                            switch($proj__226298._tag_)
                             {case 0:
                               var $proj__228=
                                _e_($proj__226298.unL1);
                               $__swJSW448__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyNumLock__;
                               break;
                              case 1:
                               var $proj__230=
                                _e_($proj__226298.unR1);
                               $__swJSW448__0=
                                $Graphics.$UI.$WXCore.$Events.$KeyScroll__;
                               break;}
                            $__swJSW446__0=
                             $__swJSW448__0;
                            break;}
                         $__swJSW442__0=
                          $__swJSW446__0;
                         break;}
                      $__swJSW432__0=
                       $__swJSW442__0;
                      break;}
                   $__swJSW414__0=
                    $__swJSW432__0;
                   break;}
                $__swJSW376__0=
                 $__swJSW414__0;
                break;}
             $__swJSW302__0=
              $__swJSW376__0;
             break;}
          return $__swJSW302__0;});
$Graphics.$UI.$WXCore.$Events.$__Rep0KeyDFLUHC_2eBase_2efrom0GENRepresentable0=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          var $__swJSW451__0;
          switch($x2._tag_)
           {case 0:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__4=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__5=
              new _A_($UHC.$Base.$R1__,[$__4]);
             var $__6=
              new _A_($UHC.$Base.$R1__,[$__5]);
             var $__7=
              new _A_($UHC.$Base.$R1__,[$__6]);
             var $__8=
              new _A_($UHC.$Base.$L1__,[$__7]);
             var $__9=
              new _A_($UHC.$Base.$L1__,[$__8]);
             var $__10=
              new _A_($UHC.$Base.$R1__,[$__9]);
             var $__11=
              new _A_($UHC.$Base.$M1__,[$__10]);
             $__swJSW451__0=
              $__11;
             break;
            case 1:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__13=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__14=
              new _A_($UHC.$Base.$L1__,[$__13]);
             var $__15=
              new _A_($UHC.$Base.$L1__,[$__14]);
             var $__16=
              new _A_($UHC.$Base.$L1__,[$__15]);
             var $__17=
              new _A_($UHC.$Base.$R1__,[$__16]);
             var $__18=
              new _A_($UHC.$Base.$L1__,[$__17]);
             var $__19=
              new _A_($UHC.$Base.$M1__,[$__18]);
             $__swJSW451__0=
              $__19;
             break;
            case 2:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__21=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__22=
              new _A_($UHC.$Base.$R1__,[$__21]);
             var $__23=
              new _A_($UHC.$Base.$L1__,[$__22]);
             var $__24=
              new _A_($UHC.$Base.$L1__,[$__23]);
             var $__25=
              new _A_($UHC.$Base.$L1__,[$__24]);
             var $__26=
              new _A_($UHC.$Base.$L1__,[$__25]);
             var $__27=
              new _A_($UHC.$Base.$M1__,[$__26]);
             $__swJSW451__0=
              $__27;
             break;
            case 3:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__29=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__30=
              new _A_($UHC.$Base.$R1__,[$__29]);
             var $__31=
              new _A_($UHC.$Base.$L1__,[$__30]);
             var $__32=
              new _A_($UHC.$Base.$R1__,[$__31]);
             var $__33=
              new _A_($UHC.$Base.$R1__,[$__32]);
             var $__34=
              new _A_($UHC.$Base.$L1__,[$__33]);
             var $__35=
              new _A_($UHC.$Base.$M1__,[$__34]);
             $__swJSW451__0=
              $__35;
             break;
            case 4:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__37=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__38=
              new _A_($UHC.$Base.$L1__,[$__37]);
             var $__39=
              new _A_($UHC.$Base.$R1__,[$__38]);
             var $__40=
              new _A_($UHC.$Base.$L1__,[$__39]);
             var $__41=
              new _A_($UHC.$Base.$R1__,[$__40]);
             var $__42=
              new _A_($UHC.$Base.$L1__,[$__41]);
             var $__43=
              new _A_($UHC.$Base.$M1__,[$__42]);
             $__swJSW451__0=
              $__43;
             break;
            case 5:
             var $__45=
              new _A_($UHC.$Base.$K1__,[$x2._1]);
             var $__46=
              new _A_($UHC.$Base.$M1__,[$__45]);
             var $__47=
              new _A_($UHC.$Base.$M1__,[$__46]);
             var $__48=
              new _A_($UHC.$Base.$L1__,[$__47]);
             var $__49=
              new _A_($UHC.$Base.$L1__,[$__48]);
             var $__50=
              new _A_($UHC.$Base.$L1__,[$__49]);
             var $__51=
              new _A_($UHC.$Base.$L1__,[$__50]);
             var $__52=
              new _A_($UHC.$Base.$L1__,[$__51]);
             var $__53=
              new _A_($UHC.$Base.$L1__,[$__52]);
             var $__54=
              new _A_($UHC.$Base.$M1__,[$__53]);
             $__swJSW451__0=
              $__54;
             break;
            case 6:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__56=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__57=
              new _A_($UHC.$Base.$R1__,[$__56]);
             var $__58=
              new _A_($UHC.$Base.$R1__,[$__57]);
             var $__59=
              new _A_($UHC.$Base.$R1__,[$__58]);
             var $__60=
              new _A_($UHC.$Base.$R1__,[$__59]);
             var $__61=
              new _A_($UHC.$Base.$L1__,[$__60]);
             var $__62=
              new _A_($UHC.$Base.$L1__,[$__61]);
             var $__63=
              new _A_($UHC.$Base.$M1__,[$__62]);
             $__swJSW451__0=
              $__63;
             break;
            case 7:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__65=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__66=
              new _A_($UHC.$Base.$R1__,[$__65]);
             var $__67=
              new _A_($UHC.$Base.$L1__,[$__66]);
             var $__68=
              new _A_($UHC.$Base.$L1__,[$__67]);
             var $__69=
              new _A_($UHC.$Base.$R1__,[$__68]);
             var $__70=
              new _A_($UHC.$Base.$L1__,[$__69]);
             var $__71=
              new _A_($UHC.$Base.$M1__,[$__70]);
             $__swJSW451__0=
              $__71;
             break;
            case 8:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__73=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__74=
              new _A_($UHC.$Base.$L1__,[$__73]);
             var $__75=
              new _A_($UHC.$Base.$L1__,[$__74]);
             var $__76=
              new _A_($UHC.$Base.$R1__,[$__75]);
             var $__77=
              new _A_($UHC.$Base.$L1__,[$__76]);
             var $__78=
              new _A_($UHC.$Base.$R1__,[$__77]);
             var $__79=
              new _A_($UHC.$Base.$M1__,[$__78]);
             $__swJSW451__0=
              $__79;
             break;
            case 9:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__81=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__82=
              new _A_($UHC.$Base.$R1__,[$__81]);
             var $__83=
              new _A_($UHC.$Base.$R1__,[$__82]);
             var $__84=
              new _A_($UHC.$Base.$R1__,[$__83]);
             var $__85=
              new _A_($UHC.$Base.$L1__,[$__84]);
             var $__86=
              new _A_($UHC.$Base.$L1__,[$__85]);
             var $__87=
              new _A_($UHC.$Base.$L1__,[$__86]);
             var $__88=
              new _A_($UHC.$Base.$M1__,[$__87]);
             $__swJSW451__0=
              $__88;
             break;
            case 10:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__90=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__91=
              new _A_($UHC.$Base.$R1__,[$__90]);
             var $__92=
              new _A_($UHC.$Base.$L1__,[$__91]);
             var $__93=
              new _A_($UHC.$Base.$R1__,[$__92]);
             var $__94=
              new _A_($UHC.$Base.$L1__,[$__93]);
             var $__95=
              new _A_($UHC.$Base.$R1__,[$__94]);
             var $__96=
              new _A_($UHC.$Base.$M1__,[$__95]);
             $__swJSW451__0=
              $__96;
             break;
            case 11:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__98=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__99=
              new _A_($UHC.$Base.$L1__,[$__98]);
             var $__100=
              new _A_($UHC.$Base.$R1__,[$__99]);
             var $__101=
              new _A_($UHC.$Base.$R1__,[$__100]);
             var $__102=
              new _A_($UHC.$Base.$L1__,[$__101]);
             var $__103=
              new _A_($UHC.$Base.$L1__,[$__102]);
             var $__104=
              new _A_($UHC.$Base.$M1__,[$__103]);
             $__swJSW451__0=
              $__104;
             break;
            case 12:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__106=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__107=
              new _A_($UHC.$Base.$L1__,[$__106]);
             var $__108=
              new _A_($UHC.$Base.$L1__,[$__107]);
             var $__109=
              new _A_($UHC.$Base.$R1__,[$__108]);
             var $__110=
              new _A_($UHC.$Base.$L1__,[$__109]);
             var $__111=
              new _A_($UHC.$Base.$L1__,[$__110]);
             var $__112=
              new _A_($UHC.$Base.$M1__,[$__111]);
             $__swJSW451__0=
              $__112;
             break;
            case 13:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__114=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__115=
              new _A_($UHC.$Base.$L1__,[$__114]);
             var $__116=
              new _A_($UHC.$Base.$R1__,[$__115]);
             var $__117=
              new _A_($UHC.$Base.$L1__,[$__116]);
             var $__118=
              new _A_($UHC.$Base.$L1__,[$__117]);
             var $__119=
              new _A_($UHC.$Base.$L1__,[$__118]);
             var $__120=
              new _A_($UHC.$Base.$M1__,[$__119]);
             $__swJSW451__0=
              $__120;
             break;
            case 14:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__122=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__123=
              new _A_($UHC.$Base.$L1__,[$__122]);
             var $__124=
              new _A_($UHC.$Base.$L1__,[$__123]);
             var $__125=
              new _A_($UHC.$Base.$R1__,[$__124]);
             var $__126=
              new _A_($UHC.$Base.$R1__,[$__125]);
             var $__127=
              new _A_($UHC.$Base.$L1__,[$__126]);
             var $__128=
              new _A_($UHC.$Base.$M1__,[$__127]);
             $__swJSW451__0=
              $__128;
             break;
            case 15:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__130=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__131=
              new _A_($UHC.$Base.$R1__,[$__130]);
             var $__132=
              new _A_($UHC.$Base.$R1__,[$__131]);
             var $__133=
              new _A_($UHC.$Base.$L1__,[$__132]);
             var $__134=
              new _A_($UHC.$Base.$R1__,[$__133]);
             var $__135=
              new _A_($UHC.$Base.$L1__,[$__134]);
             var $__136=
              new _A_($UHC.$Base.$R1__,[$__135]);
             var $__137=
              new _A_($UHC.$Base.$M1__,[$__136]);
             $__swJSW451__0=
              $__137;
             break;
            case 16:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__139=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__140=
              new _A_($UHC.$Base.$R1__,[$__139]);
             var $__141=
              new _A_($UHC.$Base.$L1__,[$__140]);
             var $__142=
              new _A_($UHC.$Base.$L1__,[$__141]);
             var $__143=
              new _A_($UHC.$Base.$R1__,[$__142]);
             var $__144=
              new _A_($UHC.$Base.$R1__,[$__143]);
             var $__145=
              new _A_($UHC.$Base.$M1__,[$__144]);
             $__swJSW451__0=
              $__145;
             break;
            case 17:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__147=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__148=
              new _A_($UHC.$Base.$R1__,[$__147]);
             var $__149=
              new _A_($UHC.$Base.$L1__,[$__148]);
             var $__150=
              new _A_($UHC.$Base.$L1__,[$__149]);
             var $__151=
              new _A_($UHC.$Base.$R1__,[$__150]);
             var $__152=
              new _A_($UHC.$Base.$R1__,[$__151]);
             var $__153=
              new _A_($UHC.$Base.$M1__,[$__152]);
             $__swJSW451__0=
              $__153;
             break;
            case 18:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__155=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__156=
              new _A_($UHC.$Base.$L1__,[$__155]);
             var $__157=
              new _A_($UHC.$Base.$R1__,[$__156]);
             var $__158=
              new _A_($UHC.$Base.$L1__,[$__157]);
             var $__159=
              new _A_($UHC.$Base.$R1__,[$__158]);
             var $__160=
              new _A_($UHC.$Base.$R1__,[$__159]);
             var $__161=
              new _A_($UHC.$Base.$M1__,[$__160]);
             $__swJSW451__0=
              $__161;
             break;
            case 19:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__163=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__164=
              new _A_($UHC.$Base.$L1__,[$__163]);
             var $__165=
              new _A_($UHC.$Base.$R1__,[$__164]);
             var $__166=
              new _A_($UHC.$Base.$L1__,[$__165]);
             var $__167=
              new _A_($UHC.$Base.$R1__,[$__166]);
             var $__168=
              new _A_($UHC.$Base.$R1__,[$__167]);
             var $__169=
              new _A_($UHC.$Base.$M1__,[$__168]);
             $__swJSW451__0=
              $__169;
             break;
            case 20:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__171=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__172=
              new _A_($UHC.$Base.$R1__,[$__171]);
             var $__173=
              new _A_($UHC.$Base.$R1__,[$__172]);
             var $__174=
              new _A_($UHC.$Base.$L1__,[$__173]);
             var $__175=
              new _A_($UHC.$Base.$R1__,[$__174]);
             var $__176=
              new _A_($UHC.$Base.$R1__,[$__175]);
             var $__177=
              new _A_($UHC.$Base.$M1__,[$__176]);
             $__swJSW451__0=
              $__177;
             break;
            case 21:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__179=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__180=
              new _A_($UHC.$Base.$R1__,[$__179]);
             var $__181=
              new _A_($UHC.$Base.$R1__,[$__180]);
             var $__182=
              new _A_($UHC.$Base.$R1__,[$__181]);
             var $__183=
              new _A_($UHC.$Base.$L1__,[$__182]);
             var $__184=
              new _A_($UHC.$Base.$R1__,[$__183]);
             var $__185=
              new _A_($UHC.$Base.$R1__,[$__184]);
             var $__186=
              new _A_($UHC.$Base.$M1__,[$__185]);
             $__swJSW451__0=
              $__186;
             break;
            case 22:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__188=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__189=
              new _A_($UHC.$Base.$R1__,[$__188]);
             var $__190=
              new _A_($UHC.$Base.$R1__,[$__189]);
             var $__191=
              new _A_($UHC.$Base.$R1__,[$__190]);
             var $__192=
              new _A_($UHC.$Base.$L1__,[$__191]);
             var $__193=
              new _A_($UHC.$Base.$R1__,[$__192]);
             var $__194=
              new _A_($UHC.$Base.$R1__,[$__193]);
             var $__195=
              new _A_($UHC.$Base.$M1__,[$__194]);
             $__swJSW451__0=
              $__195;
             break;
            case 23:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__197=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__198=
              new _A_($UHC.$Base.$L1__,[$__197]);
             var $__199=
              new _A_($UHC.$Base.$L1__,[$__198]);
             var $__200=
              new _A_($UHC.$Base.$R1__,[$__199]);
             var $__201=
              new _A_($UHC.$Base.$R1__,[$__200]);
             var $__202=
              new _A_($UHC.$Base.$R1__,[$__201]);
             var $__203=
              new _A_($UHC.$Base.$M1__,[$__202]);
             $__swJSW451__0=
              $__203;
             break;
            case 24:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__205=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__206=
              new _A_($UHC.$Base.$L1__,[$__205]);
             var $__207=
              new _A_($UHC.$Base.$L1__,[$__206]);
             var $__208=
              new _A_($UHC.$Base.$R1__,[$__207]);
             var $__209=
              new _A_($UHC.$Base.$R1__,[$__208]);
             var $__210=
              new _A_($UHC.$Base.$R1__,[$__209]);
             var $__211=
              new _A_($UHC.$Base.$M1__,[$__210]);
             $__swJSW451__0=
              $__211;
             break;
            case 25:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__213=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__214=
              new _A_($UHC.$Base.$R1__,[$__213]);
             var $__215=
              new _A_($UHC.$Base.$L1__,[$__214]);
             var $__216=
              new _A_($UHC.$Base.$R1__,[$__215]);
             var $__217=
              new _A_($UHC.$Base.$R1__,[$__216]);
             var $__218=
              new _A_($UHC.$Base.$R1__,[$__217]);
             var $__219=
              new _A_($UHC.$Base.$M1__,[$__218]);
             $__swJSW451__0=
              $__219;
             break;
            case 26:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__221=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__222=
              new _A_($UHC.$Base.$R1__,[$__221]);
             var $__223=
              new _A_($UHC.$Base.$R1__,[$__222]);
             var $__224=
              new _A_($UHC.$Base.$L1__,[$__223]);
             var $__225=
              new _A_($UHC.$Base.$R1__,[$__224]);
             var $__226=
              new _A_($UHC.$Base.$L1__,[$__225]);
             var $__227=
              new _A_($UHC.$Base.$R1__,[$__226]);
             var $__228=
              new _A_($UHC.$Base.$M1__,[$__227]);
             $__swJSW451__0=
              $__228;
             break;
            case 27:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__230=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__231=
              new _A_($UHC.$Base.$R1__,[$__230]);
             var $__232=
              new _A_($UHC.$Base.$R1__,[$__231]);
             var $__233=
              new _A_($UHC.$Base.$L1__,[$__232]);
             var $__234=
              new _A_($UHC.$Base.$R1__,[$__233]);
             var $__235=
              new _A_($UHC.$Base.$R1__,[$__234]);
             var $__236=
              new _A_($UHC.$Base.$R1__,[$__235]);
             var $__237=
              new _A_($UHC.$Base.$M1__,[$__236]);
             $__swJSW451__0=
              $__237;
             break;
            case 28:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__239=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__240=
              new _A_($UHC.$Base.$R1__,[$__239]);
             var $__241=
              new _A_($UHC.$Base.$R1__,[$__240]);
             var $__242=
              new _A_($UHC.$Base.$L1__,[$__241]);
             var $__243=
              new _A_($UHC.$Base.$R1__,[$__242]);
             var $__244=
              new _A_($UHC.$Base.$R1__,[$__243]);
             var $__245=
              new _A_($UHC.$Base.$R1__,[$__244]);
             var $__246=
              new _A_($UHC.$Base.$M1__,[$__245]);
             $__swJSW451__0=
              $__246;
             break;
            case 29:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__248=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__249=
              new _A_($UHC.$Base.$L1__,[$__248]);
             var $__250=
              new _A_($UHC.$Base.$R1__,[$__249]);
             var $__251=
              new _A_($UHC.$Base.$R1__,[$__250]);
             var $__252=
              new _A_($UHC.$Base.$R1__,[$__251]);
             var $__253=
              new _A_($UHC.$Base.$R1__,[$__252]);
             var $__254=
              new _A_($UHC.$Base.$M1__,[$__253]);
             $__swJSW451__0=
              $__254;
             break;
            case 30:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__256=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__257=
              new _A_($UHC.$Base.$L1__,[$__256]);
             var $__258=
              new _A_($UHC.$Base.$R1__,[$__257]);
             var $__259=
              new _A_($UHC.$Base.$R1__,[$__258]);
             var $__260=
              new _A_($UHC.$Base.$R1__,[$__259]);
             var $__261=
              new _A_($UHC.$Base.$R1__,[$__260]);
             var $__262=
              new _A_($UHC.$Base.$M1__,[$__261]);
             $__swJSW451__0=
              $__262;
             break;
            case 31:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__264=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__265=
              new _A_($UHC.$Base.$R1__,[$__264]);
             var $__266=
              new _A_($UHC.$Base.$R1__,[$__265]);
             var $__267=
              new _A_($UHC.$Base.$R1__,[$__266]);
             var $__268=
              new _A_($UHC.$Base.$R1__,[$__267]);
             var $__269=
              new _A_($UHC.$Base.$R1__,[$__268]);
             var $__270=
              new _A_($UHC.$Base.$M1__,[$__269]);
             $__swJSW451__0=
              $__270;
             break;
            case 32:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__272=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__273=
              new _A_($UHC.$Base.$L1__,[$__272]);
             var $__274=
              new _A_($UHC.$Base.$R1__,[$__273]);
             var $__275=
              new _A_($UHC.$Base.$R1__,[$__274]);
             var $__276=
              new _A_($UHC.$Base.$L1__,[$__275]);
             var $__277=
              new _A_($UHC.$Base.$R1__,[$__276]);
             var $__278=
              new _A_($UHC.$Base.$M1__,[$__277]);
             $__swJSW451__0=
              $__278;
             break;
            case 33:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__280=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__281=
              new _A_($UHC.$Base.$L1__,[$__280]);
             var $__282=
              new _A_($UHC.$Base.$R1__,[$__281]);
             var $__283=
              new _A_($UHC.$Base.$R1__,[$__282]);
             var $__284=
              new _A_($UHC.$Base.$L1__,[$__283]);
             var $__285=
              new _A_($UHC.$Base.$R1__,[$__284]);
             var $__286=
              new _A_($UHC.$Base.$M1__,[$__285]);
             $__swJSW451__0=
              $__286;
             break;
            case 34:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__288=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__289=
              new _A_($UHC.$Base.$R1__,[$__288]);
             var $__290=
              new _A_($UHC.$Base.$R1__,[$__289]);
             var $__291=
              new _A_($UHC.$Base.$R1__,[$__290]);
             var $__292=
              new _A_($UHC.$Base.$L1__,[$__291]);
             var $__293=
              new _A_($UHC.$Base.$R1__,[$__292]);
             var $__294=
              new _A_($UHC.$Base.$M1__,[$__293]);
             $__swJSW451__0=
              $__294;
             break;
            case 35:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__296=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__297=
              new _A_($UHC.$Base.$R1__,[$__296]);
             var $__298=
              new _A_($UHC.$Base.$R1__,[$__297]);
             var $__299=
              new _A_($UHC.$Base.$R1__,[$__298]);
             var $__300=
              new _A_($UHC.$Base.$R1__,[$__299]);
             var $__301=
              new _A_($UHC.$Base.$L1__,[$__300]);
             var $__302=
              new _A_($UHC.$Base.$R1__,[$__301]);
             var $__303=
              new _A_($UHC.$Base.$M1__,[$__302]);
             $__swJSW451__0=
              $__303;
             break;
            case 36:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__305=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__306=
              new _A_($UHC.$Base.$R1__,[$__305]);
             var $__307=
              new _A_($UHC.$Base.$R1__,[$__306]);
             var $__308=
              new _A_($UHC.$Base.$R1__,[$__307]);
             var $__309=
              new _A_($UHC.$Base.$R1__,[$__308]);
             var $__310=
              new _A_($UHC.$Base.$L1__,[$__309]);
             var $__311=
              new _A_($UHC.$Base.$R1__,[$__310]);
             var $__312=
              new _A_($UHC.$Base.$M1__,[$__311]);
             $__swJSW451__0=
              $__312;
             break;
            case 37:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__314=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__315=
              new _A_($UHC.$Base.$L1__,[$__314]);
             var $__316=
              new _A_($UHC.$Base.$L1__,[$__315]);
             var $__317=
              new _A_($UHC.$Base.$L1__,[$__316]);
             var $__318=
              new _A_($UHC.$Base.$R1__,[$__317]);
             var $__319=
              new _A_($UHC.$Base.$R1__,[$__318]);
             var $__320=
              new _A_($UHC.$Base.$M1__,[$__319]);
             $__swJSW451__0=
              $__320;
             break;
            case 38:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__322=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__323=
              new _A_($UHC.$Base.$L1__,[$__322]);
             var $__324=
              new _A_($UHC.$Base.$L1__,[$__323]);
             var $__325=
              new _A_($UHC.$Base.$L1__,[$__324]);
             var $__326=
              new _A_($UHC.$Base.$R1__,[$__325]);
             var $__327=
              new _A_($UHC.$Base.$R1__,[$__326]);
             var $__328=
              new _A_($UHC.$Base.$M1__,[$__327]);
             $__swJSW451__0=
              $__328;
             break;
            case 39:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__330=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__331=
              new _A_($UHC.$Base.$R1__,[$__330]);
             var $__332=
              new _A_($UHC.$Base.$R1__,[$__331]);
             var $__333=
              new _A_($UHC.$Base.$L1__,[$__332]);
             var $__334=
              new _A_($UHC.$Base.$R1__,[$__333]);
             var $__335=
              new _A_($UHC.$Base.$L1__,[$__334]);
             var $__336=
              new _A_($UHC.$Base.$M1__,[$__335]);
             $__swJSW451__0=
              $__336;
             break;
            case 40:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__338=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__339=
              new _A_($UHC.$Base.$L1__,[$__338]);
             var $__340=
              new _A_($UHC.$Base.$L1__,[$__339]);
             var $__341=
              new _A_($UHC.$Base.$R1__,[$__340]);
             var $__342=
              new _A_($UHC.$Base.$L1__,[$__341]);
             var $__343=
              new _A_($UHC.$Base.$L1__,[$__342]);
             var $__344=
              new _A_($UHC.$Base.$M1__,[$__343]);
             $__swJSW451__0=
              $__344;
             break;
            case 41:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__346=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__347=
              new _A_($UHC.$Base.$R1__,[$__346]);
             var $__348=
              new _A_($UHC.$Base.$R1__,[$__347]);
             var $__349=
              new _A_($UHC.$Base.$R1__,[$__348]);
             var $__350=
              new _A_($UHC.$Base.$L1__,[$__349]);
             var $__351=
              new _A_($UHC.$Base.$L1__,[$__350]);
             var $__352=
              new _A_($UHC.$Base.$L1__,[$__351]);
             var $__353=
              new _A_($UHC.$Base.$M1__,[$__352]);
             $__swJSW451__0=
              $__353;
             break;
            case 42:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__355=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__356=
              new _A_($UHC.$Base.$R1__,[$__355]);
             var $__357=
              new _A_($UHC.$Base.$L1__,[$__356]);
             var $__358=
              new _A_($UHC.$Base.$R1__,[$__357]);
             var $__359=
              new _A_($UHC.$Base.$L1__,[$__358]);
             var $__360=
              new _A_($UHC.$Base.$L1__,[$__359]);
             var $__361=
              new _A_($UHC.$Base.$M1__,[$__360]);
             $__swJSW451__0=
              $__361;
             break;
            case 43:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__363=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__364=
              new _A_($UHC.$Base.$R1__,[$__363]);
             var $__365=
              new _A_($UHC.$Base.$R1__,[$__364]);
             var $__366=
              new _A_($UHC.$Base.$L1__,[$__365]);
             var $__367=
              new _A_($UHC.$Base.$R1__,[$__366]);
             var $__368=
              new _A_($UHC.$Base.$R1__,[$__367]);
             var $__369=
              new _A_($UHC.$Base.$L1__,[$__368]);
             var $__370=
              new _A_($UHC.$Base.$M1__,[$__369]);
             $__swJSW451__0=
              $__370;
             break;
            case 44:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__372=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__373=
              new _A_($UHC.$Base.$R1__,[$__372]);
             var $__374=
              new _A_($UHC.$Base.$L1__,[$__373]);
             var $__375=
              new _A_($UHC.$Base.$L1__,[$__374]);
             var $__376=
              new _A_($UHC.$Base.$R1__,[$__375]);
             var $__377=
              new _A_($UHC.$Base.$L1__,[$__376]);
             var $__378=
              new _A_($UHC.$Base.$M1__,[$__377]);
             $__swJSW451__0=
              $__378;
             break;
            case 45:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__380=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__381=
              new _A_($UHC.$Base.$L1__,[$__380]);
             var $__382=
              new _A_($UHC.$Base.$R1__,[$__381]);
             var $__383=
              new _A_($UHC.$Base.$R1__,[$__382]);
             var $__384=
              new _A_($UHC.$Base.$R1__,[$__383]);
             var $__385=
              new _A_($UHC.$Base.$L1__,[$__384]);
             var $__386=
              new _A_($UHC.$Base.$M1__,[$__385]);
             $__swJSW451__0=
              $__386;
             break;
            case 46:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__388=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__389=
              new _A_($UHC.$Base.$R1__,[$__388]);
             var $__390=
              new _A_($UHC.$Base.$R1__,[$__389]);
             var $__391=
              new _A_($UHC.$Base.$L1__,[$__390]);
             var $__392=
              new _A_($UHC.$Base.$L1__,[$__391]);
             var $__393=
              new _A_($UHC.$Base.$R1__,[$__392]);
             var $__394=
              new _A_($UHC.$Base.$M1__,[$__393]);
             $__swJSW451__0=
              $__394;
             break;
            case 47:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__396=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__397=
              new _A_($UHC.$Base.$L1__,[$__396]);
             var $__398=
              new _A_($UHC.$Base.$R1__,[$__397]);
             var $__399=
              new _A_($UHC.$Base.$R1__,[$__398]);
             var $__400=
              new _A_($UHC.$Base.$R1__,[$__399]);
             var $__401=
              new _A_($UHC.$Base.$L1__,[$__400]);
             var $__402=
              new _A_($UHC.$Base.$M1__,[$__401]);
             $__swJSW451__0=
              $__402;
             break;
            case 48:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__404=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__405=
              new _A_($UHC.$Base.$R1__,[$__404]);
             var $__406=
              new _A_($UHC.$Base.$R1__,[$__405]);
             var $__407=
              new _A_($UHC.$Base.$R1__,[$__406]);
             var $__408=
              new _A_($UHC.$Base.$R1__,[$__407]);
             var $__409=
              new _A_($UHC.$Base.$L1__,[$__408]);
             var $__410=
              new _A_($UHC.$Base.$M1__,[$__409]);
             $__swJSW451__0=
              $__410;
             break;
            case 49:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__412=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__413=
              new _A_($UHC.$Base.$R1__,[$__412]);
             var $__414=
              new _A_($UHC.$Base.$R1__,[$__413]);
             var $__415=
              new _A_($UHC.$Base.$R1__,[$__414]);
             var $__416=
              new _A_($UHC.$Base.$R1__,[$__415]);
             var $__417=
              new _A_($UHC.$Base.$R1__,[$__416]);
             var $__418=
              new _A_($UHC.$Base.$L1__,[$__417]);
             var $__419=
              new _A_($UHC.$Base.$M1__,[$__418]);
             $__swJSW451__0=
              $__419;
             break;
            case 50:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__421=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__422=
              new _A_($UHC.$Base.$R1__,[$__421]);
             var $__423=
              new _A_($UHC.$Base.$R1__,[$__422]);
             var $__424=
              new _A_($UHC.$Base.$R1__,[$__423]);
             var $__425=
              new _A_($UHC.$Base.$R1__,[$__424]);
             var $__426=
              new _A_($UHC.$Base.$R1__,[$__425]);
             var $__427=
              new _A_($UHC.$Base.$L1__,[$__426]);
             var $__428=
              new _A_($UHC.$Base.$M1__,[$__427]);
             $__swJSW451__0=
              $__428;
             break;
            case 51:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__430=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__431=
              new _A_($UHC.$Base.$L1__,[$__430]);
             var $__432=
              new _A_($UHC.$Base.$L1__,[$__431]);
             var $__433=
              new _A_($UHC.$Base.$L1__,[$__432]);
             var $__434=
              new _A_($UHC.$Base.$L1__,[$__433]);
             var $__435=
              new _A_($UHC.$Base.$R1__,[$__434]);
             var $__436=
              new _A_($UHC.$Base.$M1__,[$__435]);
             $__swJSW451__0=
              $__436;
             break;
            case 52:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__438=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__439=
              new _A_($UHC.$Base.$L1__,[$__438]);
             var $__440=
              new _A_($UHC.$Base.$L1__,[$__439]);
             var $__441=
              new _A_($UHC.$Base.$L1__,[$__440]);
             var $__442=
              new _A_($UHC.$Base.$L1__,[$__441]);
             var $__443=
              new _A_($UHC.$Base.$R1__,[$__442]);
             var $__444=
              new _A_($UHC.$Base.$M1__,[$__443]);
             $__swJSW451__0=
              $__444;
             break;
            case 53:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__446=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__447=
              new _A_($UHC.$Base.$R1__,[$__446]);
             var $__448=
              new _A_($UHC.$Base.$L1__,[$__447]);
             var $__449=
              new _A_($UHC.$Base.$L1__,[$__448]);
             var $__450=
              new _A_($UHC.$Base.$L1__,[$__449]);
             var $__451=
              new _A_($UHC.$Base.$R1__,[$__450]);
             var $__452=
              new _A_($UHC.$Base.$M1__,[$__451]);
             $__swJSW451__0=
              $__452;
             break;
            case 54:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__454=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__455=
              new _A_($UHC.$Base.$R1__,[$__454]);
             var $__456=
              new _A_($UHC.$Base.$L1__,[$__455]);
             var $__457=
              new _A_($UHC.$Base.$L1__,[$__456]);
             var $__458=
              new _A_($UHC.$Base.$L1__,[$__457]);
             var $__459=
              new _A_($UHC.$Base.$R1__,[$__458]);
             var $__460=
              new _A_($UHC.$Base.$M1__,[$__459]);
             $__swJSW451__0=
              $__460;
             break;
            case 55:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__462=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__463=
              new _A_($UHC.$Base.$L1__,[$__462]);
             var $__464=
              new _A_($UHC.$Base.$R1__,[$__463]);
             var $__465=
              new _A_($UHC.$Base.$L1__,[$__464]);
             var $__466=
              new _A_($UHC.$Base.$L1__,[$__465]);
             var $__467=
              new _A_($UHC.$Base.$R1__,[$__466]);
             var $__468=
              new _A_($UHC.$Base.$M1__,[$__467]);
             $__swJSW451__0=
              $__468;
             break;
            case 56:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__470=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__471=
              new _A_($UHC.$Base.$L1__,[$__470]);
             var $__472=
              new _A_($UHC.$Base.$R1__,[$__471]);
             var $__473=
              new _A_($UHC.$Base.$L1__,[$__472]);
             var $__474=
              new _A_($UHC.$Base.$L1__,[$__473]);
             var $__475=
              new _A_($UHC.$Base.$R1__,[$__474]);
             var $__476=
              new _A_($UHC.$Base.$M1__,[$__475]);
             $__swJSW451__0=
              $__476;
             break;
            case 57:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__478=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__479=
              new _A_($UHC.$Base.$R1__,[$__478]);
             var $__480=
              new _A_($UHC.$Base.$R1__,[$__479]);
             var $__481=
              new _A_($UHC.$Base.$R1__,[$__480]);
             var $__482=
              new _A_($UHC.$Base.$R1__,[$__481]);
             var $__483=
              new _A_($UHC.$Base.$R1__,[$__482]);
             var $__484=
              new _A_($UHC.$Base.$R1__,[$__483]);
             var $__485=
              new _A_($UHC.$Base.$M1__,[$__484]);
             $__swJSW451__0=
              $__485;
             break;
            case 58:
             var $__487=
              new _A_($UHC.$Base.$K1__,[$x2._1]);
             var $__488=
              new _A_($UHC.$Base.$M1__,[$__487]);
             var $__489=
              new _A_($UHC.$Base.$M1__,[$__488]);
             var $__490=
              new _A_($UHC.$Base.$R1__,[$__489]);
             var $__491=
              new _A_($UHC.$Base.$L1__,[$__490]);
             var $__492=
              new _A_($UHC.$Base.$L1__,[$__491]);
             var $__493=
              new _A_($UHC.$Base.$L1__,[$__492]);
             var $__494=
              new _A_($UHC.$Base.$L1__,[$__493]);
             var $__495=
              new _A_($UHC.$Base.$L1__,[$__494]);
             var $__496=
              new _A_($UHC.$Base.$M1__,[$__495]);
             $__swJSW451__0=
              $__496;
             break;
            case 59:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__498=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__499=
              new _A_($UHC.$Base.$R1__,[$__498]);
             var $__500=
              new _A_($UHC.$Base.$R1__,[$__499]);
             var $__501=
              new _A_($UHC.$Base.$R1__,[$__500]);
             var $__502=
              new _A_($UHC.$Base.$L1__,[$__501]);
             var $__503=
              new _A_($UHC.$Base.$L1__,[$__502]);
             var $__504=
              new _A_($UHC.$Base.$M1__,[$__503]);
             $__swJSW451__0=
              $__504;
             break;
            case 60:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__506=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__507=
              new _A_($UHC.$Base.$L1__,[$__506]);
             var $__508=
              new _A_($UHC.$Base.$R1__,[$__507]);
             var $__509=
              new _A_($UHC.$Base.$R1__,[$__508]);
             var $__510=
              new _A_($UHC.$Base.$L1__,[$__509]);
             var $__511=
              new _A_($UHC.$Base.$L1__,[$__510]);
             var $__512=
              new _A_($UHC.$Base.$M1__,[$__511]);
             $__swJSW451__0=
              $__512;
             break;
            case 61:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__514=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__515=
              new _A_($UHC.$Base.$L1__,[$__514]);
             var $__516=
              new _A_($UHC.$Base.$R1__,[$__515]);
             var $__517=
              new _A_($UHC.$Base.$L1__,[$__516]);
             var $__518=
              new _A_($UHC.$Base.$R1__,[$__517]);
             var $__519=
              new _A_($UHC.$Base.$L1__,[$__518]);
             var $__520=
              new _A_($UHC.$Base.$M1__,[$__519]);
             $__swJSW451__0=
              $__520;
             break;
            case 62:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__522=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__523=
              new _A_($UHC.$Base.$R1__,[$__522]);
             var $__524=
              new _A_($UHC.$Base.$R1__,[$__523]);
             var $__525=
              new _A_($UHC.$Base.$R1__,[$__524]);
             var $__526=
              new _A_($UHC.$Base.$L1__,[$__525]);
             var $__527=
              new _A_($UHC.$Base.$R1__,[$__526]);
             var $__528=
              new _A_($UHC.$Base.$L1__,[$__527]);
             var $__529=
              new _A_($UHC.$Base.$M1__,[$__528]);
             $__swJSW451__0=
              $__529;
             break;
            case 63:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__531=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__532=
              new _A_($UHC.$Base.$L1__,[$__531]);
             var $__533=
              new _A_($UHC.$Base.$R1__,[$__532]);
             var $__534=
              new _A_($UHC.$Base.$L1__,[$__533]);
             var $__535=
              new _A_($UHC.$Base.$L1__,[$__534]);
             var $__536=
              new _A_($UHC.$Base.$L1__,[$__535]);
             var $__537=
              new _A_($UHC.$Base.$M1__,[$__536]);
             $__swJSW451__0=
              $__537;
             break;
            case 64:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__539=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__540=
              new _A_($UHC.$Base.$R1__,[$__539]);
             var $__541=
              new _A_($UHC.$Base.$R1__,[$__540]);
             var $__542=
              new _A_($UHC.$Base.$L1__,[$__541]);
             var $__543=
              new _A_($UHC.$Base.$R1__,[$__542]);
             var $__544=
              new _A_($UHC.$Base.$L1__,[$__543]);
             var $__545=
              new _A_($UHC.$Base.$L1__,[$__544]);
             var $__546=
              new _A_($UHC.$Base.$M1__,[$__545]);
             $__swJSW451__0=
              $__546;
             break;
            case 65:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__548=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__549=
              new _A_($UHC.$Base.$R1__,[$__548]);
             var $__550=
              new _A_($UHC.$Base.$R1__,[$__549]);
             var $__551=
              new _A_($UHC.$Base.$L1__,[$__550]);
             var $__552=
              new _A_($UHC.$Base.$R1__,[$__551]);
             var $__553=
              new _A_($UHC.$Base.$R1__,[$__552]);
             var $__554=
              new _A_($UHC.$Base.$L1__,[$__553]);
             var $__555=
              new _A_($UHC.$Base.$M1__,[$__554]);
             $__swJSW451__0=
              $__555;
             break;
            case 66:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__557=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__558=
              new _A_($UHC.$Base.$R1__,[$__557]);
             var $__559=
              new _A_($UHC.$Base.$R1__,[$__558]);
             var $__560=
              new _A_($UHC.$Base.$R1__,[$__559]);
             var $__561=
              new _A_($UHC.$Base.$R1__,[$__560]);
             var $__562=
              new _A_($UHC.$Base.$R1__,[$__561]);
             var $__563=
              new _A_($UHC.$Base.$R1__,[$__562]);
             var $__564=
              new _A_($UHC.$Base.$M1__,[$__563]);
             $__swJSW451__0=
              $__564;
             break;
            case 67:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__566=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__567=
              new _A_($UHC.$Base.$R1__,[$__566]);
             var $__568=
              new _A_($UHC.$Base.$R1__,[$__567]);
             var $__569=
              new _A_($UHC.$Base.$R1__,[$__568]);
             var $__570=
              new _A_($UHC.$Base.$L1__,[$__569]);
             var $__571=
              new _A_($UHC.$Base.$R1__,[$__570]);
             var $__572=
              new _A_($UHC.$Base.$L1__,[$__571]);
             var $__573=
              new _A_($UHC.$Base.$M1__,[$__572]);
             $__swJSW451__0=
              $__573;
             break;
            case 68:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__575=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__576=
              new _A_($UHC.$Base.$R1__,[$__575]);
             var $__577=
              new _A_($UHC.$Base.$R1__,[$__576]);
             var $__578=
              new _A_($UHC.$Base.$R1__,[$__577]);
             var $__579=
              new _A_($UHC.$Base.$L1__,[$__578]);
             var $__580=
              new _A_($UHC.$Base.$L1__,[$__579]);
             var $__581=
              new _A_($UHC.$Base.$R1__,[$__580]);
             var $__582=
              new _A_($UHC.$Base.$M1__,[$__581]);
             $__swJSW451__0=
              $__582;
             break;
            case 69:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__584=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__585=
              new _A_($UHC.$Base.$L1__,[$__584]);
             var $__586=
              new _A_($UHC.$Base.$L1__,[$__585]);
             var $__587=
              new _A_($UHC.$Base.$L1__,[$__586]);
             var $__588=
              new _A_($UHC.$Base.$R1__,[$__587]);
             var $__589=
              new _A_($UHC.$Base.$L1__,[$__588]);
             var $__590=
              new _A_($UHC.$Base.$M1__,[$__589]);
             $__swJSW451__0=
              $__590;
             break;
            case 70:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__592=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__593=
              new _A_($UHC.$Base.$L1__,[$__592]);
             var $__594=
              new _A_($UHC.$Base.$L1__,[$__593]);
             var $__595=
              new _A_($UHC.$Base.$R1__,[$__594]);
             var $__596=
              new _A_($UHC.$Base.$R1__,[$__595]);
             var $__597=
              new _A_($UHC.$Base.$L1__,[$__596]);
             var $__598=
              new _A_($UHC.$Base.$M1__,[$__597]);
             $__swJSW451__0=
              $__598;
             break;
            case 71:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__600=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__601=
              new _A_($UHC.$Base.$R1__,[$__600]);
             var $__602=
              new _A_($UHC.$Base.$R1__,[$__601]);
             var $__603=
              new _A_($UHC.$Base.$L1__,[$__602]);
             var $__604=
              new _A_($UHC.$Base.$L1__,[$__603]);
             var $__605=
              new _A_($UHC.$Base.$L1__,[$__604]);
             var $__606=
              new _A_($UHC.$Base.$M1__,[$__605]);
             $__swJSW451__0=
              $__606;
             break;
            case 72:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__608=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__609=
              new _A_($UHC.$Base.$R1__,[$__608]);
             var $__610=
              new _A_($UHC.$Base.$R1__,[$__609]);
             var $__611=
              new _A_($UHC.$Base.$R1__,[$__610]);
             var $__612=
              new _A_($UHC.$Base.$R1__,[$__611]);
             var $__613=
              new _A_($UHC.$Base.$L1__,[$__612]);
             var $__614=
              new _A_($UHC.$Base.$L1__,[$__613]);
             var $__615=
              new _A_($UHC.$Base.$M1__,[$__614]);
             $__swJSW451__0=
              $__615;
             break;
            case 73:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__617=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__618=
              new _A_($UHC.$Base.$L1__,[$__617]);
             var $__619=
              new _A_($UHC.$Base.$L1__,[$__618]);
             var $__620=
              new _A_($UHC.$Base.$R1__,[$__619]);
             var $__621=
              new _A_($UHC.$Base.$L1__,[$__620]);
             var $__622=
              new _A_($UHC.$Base.$R1__,[$__621]);
             var $__623=
              new _A_($UHC.$Base.$M1__,[$__622]);
             $__swJSW451__0=
              $__623;
             break;
            case 74:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__625=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__626=
              new _A_($UHC.$Base.$R1__,[$__625]);
             var $__627=
              new _A_($UHC.$Base.$L1__,[$__626]);
             var $__628=
              new _A_($UHC.$Base.$L1__,[$__627]);
             var $__629=
              new _A_($UHC.$Base.$L1__,[$__628]);
             var $__630=
              new _A_($UHC.$Base.$L1__,[$__629]);
             var $__631=
              new _A_($UHC.$Base.$M1__,[$__630]);
             $__swJSW451__0=
              $__631;
             break;
            case 75:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__633=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__634=
              new _A_($UHC.$Base.$R1__,[$__633]);
             var $__635=
              new _A_($UHC.$Base.$R1__,[$__634]);
             var $__636=
              new _A_($UHC.$Base.$L1__,[$__635]);
             var $__637=
              new _A_($UHC.$Base.$R1__,[$__636]);
             var $__638=
              new _A_($UHC.$Base.$L1__,[$__637]);
             var $__639=
              new _A_($UHC.$Base.$L1__,[$__638]);
             var $__640=
              new _A_($UHC.$Base.$M1__,[$__639]);
             $__swJSW451__0=
              $__640;
             break;}
          return $__swJSW451__0;});
$Graphics.$UI.$WXCore.$Events.$__Rep0KeyNEW1805UNQ225EVLSDCGENRepresentable0=
 new _F_(function($__)
         {var $Representable0__=
           _e_(new _A_($UHC.$Base.$Representable0__CLS74__369__0,[$__]));
          var $__5=
           {_tag_:0,_1:$Graphics.$UI.$WXCore.$Events.$__Rep0KeyDFLUHC_2eBase_2efrom0GENRepresentable0,_2:$Graphics.$UI.$WXCore.$Events.$__Rep0KeyDFLUHC_2eBase_2eto0GENRepresentable0};
          return $__5;});
$Graphics.$UI.$WXCore.$Events.$__Rep0KeyNEW1803UNQ224SDCGENRepresentable0=
 new _F_(function($__)
         {var $__2=
           new _A_($Graphics.$UI.$WXCore.$Events.$__Rep0KeyNEW1805UNQ225EVLSDCGENRepresentable0,[$__]);
          return $__2;});
$Graphics.$UI.$WXCore.$Events.$__Rep0KeyUNQ224SDCGENRepresentable0=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$Events.$__Rep0KeyNEW1803UNQ224SDCGENRepresentable0,[$Graphics.$UI.$WXCore.$Events.$__Rep0KeyUNQ224SDCGENRepresentable0]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__Rep0KeyGENRepresentable0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$Events.$__Rep0KeyUNQ224SDCGENRepresentable0;}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__120UNQ1784=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Graphics.$UI.$WXCore.$Events.$__101__3136__2__64UNQ1816,$Graphics.$UI.$WXCore.$Events.$__101__3136__2__64UNQ1816]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__66UNQ1824=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Graphics.$UI.$WXCore.$Events.$__101__3136__2__16UNQ1695,$Graphics.$UI.$WXCore.$Events.$__101__3136__2__16UNQ1695]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__65UNQ1820=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Graphics.$UI.$WXCore.$Events.$__101__3136__2__66UNQ1824,$Graphics.$UI.$WXCore.$Events.$__101__3136__2__21UNQ1715]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__64UNQ1816=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Graphics.$UI.$WXCore.$Events.$__101__3136__2__65UNQ1820,$Graphics.$UI.$WXCore.$Events.$__101__3136__2__35UNQ1733]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__35UNQ1733=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Graphics.$UI.$WXCore.$Events.$__101__3136__2__21UNQ1715,$Graphics.$UI.$WXCore.$Events.$__101__3136__2__21UNQ1715]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__27UNQ1742=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Graphics.$UI.$WXCore.$Events.$__101__3136__2__17UNQ1699,$Graphics.$UI.$WXCore.$Events.$__101__3136__2__16UNQ1695]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__21UNQ1715=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Graphics.$UI.$WXCore.$Events.$__101__3136__2__16UNQ1695,$Graphics.$UI.$WXCore.$Events.$__101__3136__2__27UNQ1742]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__17UNQ1699=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$UHC.$Base.$Eq_27__DCT74__389__0]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__16UNQ1695=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Graphics.$UI.$WXCore.$Events.$__101__3136__2__17UNQ1699,$Graphics.$UI.$WXCore.$Events.$__101__3136__2__17UNQ1699]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__14UNQ1687=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__390__0,[$UHC.$Base.$Eq__DCT74__88__0]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__13UNQ1683=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$Graphics.$UI.$WXCore.$Events.$__101__3136__2__14UNQ1687]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__12UNQ1679=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$Graphics.$UI.$WXCore.$Events.$__101__3136__2__13UNQ1683]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__10UNQ1671=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__390__0,[$UHC.$Base.$Eq__DCT74__56__0]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__9UNQ1667=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$Graphics.$UI.$WXCore.$Events.$__101__3136__2__10UNQ1671]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__8UNQ1663=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$Graphics.$UI.$WXCore.$Events.$__101__3136__2__9UNQ1667]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__7UNQ1659=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Graphics.$UI.$WXCore.$Events.$__101__3136__2__8UNQ1663,$Graphics.$UI.$WXCore.$Events.$__101__3136__2__12UNQ1679]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__6UNQ1654=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Graphics.$UI.$WXCore.$Events.$__101__3136__2__7UNQ1659,$Graphics.$UI.$WXCore.$Events.$__101__3136__2__16UNQ1695]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__5UNQ1650=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Graphics.$UI.$WXCore.$Events.$__101__3136__2__6UNQ1654,$Graphics.$UI.$WXCore.$Events.$__101__3136__2__21UNQ1715]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__4UNQ1879=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Graphics.$UI.$WXCore.$Events.$__101__3136__2__5UNQ1650,$Graphics.$UI.$WXCore.$Events.$__101__3136__2__35UNQ1733]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__3UNQ1874=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Graphics.$UI.$WXCore.$Events.$__101__3136__2__4UNQ1879,$Graphics.$UI.$WXCore.$Events.$__101__3136__2__64UNQ1816]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3136__2__2UNQ1870=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$Graphics.$UI.$WXCore.$Events.$__101__3136__2__3UNQ1874,$Graphics.$UI.$WXCore.$Events.$__101__3136__2__120UNQ1784]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__101__3144__0__4__0UNQ1658=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$Graphics.$UI.$WXCore.$Events.$__101__3136__2__2UNQ1870]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__99__12__0DFLUHC_2eBase_2e_3d_3d=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$geqdefault,[$Graphics.$UI.$WXCore.$Events.$__Rep0KeyGENRepresentable0,$Graphics.$UI.$WXCore.$Events.$__101__3144__0__4__0UNQ1658,$UHC.$Base.$undefined]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__99__12__0UNQ1649RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$Events.$__99__12__0NEW1833UNQ1649RDC,[$Graphics.$UI.$WXCore.$Events.$__99__12__0UNQ1649RDC,$Graphics.$UI.$WXCore.$Events.$__99__12__0DFLUHC_2eBase_2e_3d_3d]);}),[]);
$Graphics.$UI.$WXCore.$Events.$__99__12__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$Events.$__99__12__0UNQ1649RDC;}),[]);
$Graphics.$UI.$WX.$Events.$filterUNQ273=
 new _F_(function($k,$__)
         {var $__3=
           _e_($__);
          var $__7=
           new _A_($UHC.$Base.$_3d_3d,[$Graphics.$UI.$WXCore.$Events.$__99__12__0,$k,$__3._1]);
          return $__7;});
$Graphics.$UI.$WX.$Events.$key=
 new _F_(function($__,$k)
         {var $__3=
           new _A_($UHC.$Base.$packedStringToString,["key"]);
          var $__4=
           new _A_($Graphics.$UI.$WX.$Events.$filterUNQ273,[$k]);
          return new _A_($Graphics.$UI.$WX.$Events.$keyboardFilter,[$__,$__3,$__4]);});
$Graphics.$UI.$WXCore.$Events.$KeyLeft__=
 new _A_(new _F_(function()
                 {return {_tag_:42};}),[]);
$Graphics.$UI.$WX.$Events.$leftKey=
 new _F_(function($__)
         {return new _A_($Graphics.$UI.$WX.$Events.$key,[$__,$Graphics.$UI.$WXCore.$Events.$KeyLeft__]);});
$Asteroids.$_24okUNQ198=
 new _F_(function($_24x,$_24x2,$_24x3,$_24x4)
         {var $__=
           new _A_($Graphics.$UI.$WX.$Attributes.$_3a_7e,[$Graphics.$UI.$WX.$Timer.$interval,$Asteroids.$__55__348__0]);
          var $__6=
           new _A_($UHC.$Base.$_3a,[$__,$UHC.$Base.$_5b_5d]);
          var $__7=
           new _A_($Graphics.$UI.$WX.$Attributes.$set,[$_24x4,$__6]);
          var $__8=
           new _A_($UHC.$Base.$packedStringToString,["slowdown"]);
          var $__9=
           new _A_($Language.$UHC.$JS.$Marshal.$str,[$__8]);
          var $__10=
           new _A_($Language.$UHC.$JS.$Prelude.$__trace,[$__9]);
          var $__11=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__10,$__7]);
          var $__12=
           new _A_($Graphics.$UI.$WX.$Events.$charKey,[$Graphics.$UI.$WX.$Window.$Reactive__DCT225__33__0,119]);
          var $__13=
           new _A_($Graphics.$UI.$WX.$Events.$on,[$__12]);
          var $__14=
           new _A_($Graphics.$UI.$WX.$Attributes.$_3a_3d,[$__13,$__11]);
          var $__15=
           new _A_($UHC.$Base.$_3a,[$__14,$UHC.$Base.$_5b_5d]);
          var $__16=
           new _A_($Graphics.$UI.$WX.$Attributes.$_3a_7e,[$Graphics.$UI.$WX.$Timer.$interval,$Asteroids.$__55__324__0]);
          var $__17=
           new _A_($UHC.$Base.$_3a,[$__16,$UHC.$Base.$_5b_5d]);
          var $__18=
           new _A_($Graphics.$UI.$WX.$Attributes.$set,[$_24x4,$__17]);
          var $__19=
           new _A_($Graphics.$UI.$WX.$Events.$charKey,[$Graphics.$UI.$WX.$Window.$Reactive__DCT225__33__0,113]);
          var $__20=
           new _A_($Graphics.$UI.$WX.$Events.$on,[$__19]);
          var $__21=
           new _A_($Graphics.$UI.$WX.$Attributes.$_3a_3d,[$__20,$__18]);
          var $__22=
           new _A_($UHC.$Base.$_3a,[$__21,$__15]);
          var $__23=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
          var $__24=
           new _A_($Graphics.$UI.$WXCore.$Types.$varUpdate,[$_24x2,$Asteroids.$__55__301__0]);
          var $__25=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__24,$__23]);
          var $__26=
           new _A_($Graphics.$UI.$WX.$Events.$rightKey,[$Graphics.$UI.$WX.$Window.$Reactive__DCT225__33__0]);
          var $__27=
           new _A_($Graphics.$UI.$WX.$Events.$on,[$__26]);
          var $__28=
           new _A_($Graphics.$UI.$WX.$Attributes.$_3a_3d,[$__27,$__25]);
          var $__29=
           new _A_($UHC.$Base.$_3a,[$__28,$__22]);
          var $__30=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
          var $__31=
           new _A_($Graphics.$UI.$WXCore.$Types.$varUpdate,[$_24x2,$Asteroids.$__55__280__0]);
          var $__32=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__31,$__30]);
          var $__33=
           new _A_($Graphics.$UI.$WX.$Events.$leftKey,[$Graphics.$UI.$WX.$Window.$Reactive__DCT225__33__0]);
          var $__34=
           new _A_($Graphics.$UI.$WX.$Events.$on,[$__33]);
          var $__35=
           new _A_($Graphics.$UI.$WX.$Attributes.$_3a_3d,[$__34,$__32]);
          var $__36=
           new _A_($UHC.$Base.$_3a,[$__35,$__29]);
          var $__37=
           new _A_($Asteroids.$draw,[$_24x,$_24x2]);
          var $__38=
           new _A_($Graphics.$UI.$WX.$Events.$paint,[$Graphics.$UI.$WX.$Window.$Paint__DCT225__41__0]);
          var $__39=
           new _A_($Graphics.$UI.$WX.$Events.$on,[$__38]);
          var $__40=
           new _A_($Graphics.$UI.$WX.$Attributes.$_3a_3d,[$__39,$__37]);
          var $__41=
           new _A_($UHC.$Base.$_3a,[$__40,$__36]);
          return new _A_($Graphics.$UI.$WX.$Attributes.$set,[$_24x3,$__41]);});
$Graphics.$UI.$WX.$Window.$__229__146=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["paintRaw  not implemented"]);}),[]);
$Graphics.$UI.$WX.$Window.$Paint__DCT225__41__0DFLGraphics_2eUI_2eWX_2eEvents_2epaintRaw=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$Graphics.$UI.$WX.$Window.$__229__146]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__windowRefresh=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__windowRefresh;});
$Graphics.$UI.$WXCore.$WindowClass.$windowRefresh=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$WindowClass.$__windowRefresh,$Graphics.$UI.$WXCore.$WindowClass.$window__Methods]);}),[]);
$Graphics.$UI.$WX.$Window.$Paint__DCT225__41__0DFLGraphics_2eUI_2eWX_2eEvents_2erepaint=
 new _F_(function($w)
         {return new _A_($Graphics.$UI.$WXCore.$WindowClass.$windowRefresh,[$w,$UHC.$Base.$False__]);});
$Graphics.$UI.$WX.$Events.$Paint__CLS213__3__0=
 new _F_(function($Paint__)
         {var $Paint__2=
           {_tag_:0,_1:$UHC.$Base.$undefined,_2:$UHC.$Base.$undefined,_3:$UHC.$Base.$undefined};
          return $Paint__2;});
$Graphics.$UI.$WX.$Window.$Paint__NEW103UNQ205EVLDCT225__41__0RDC=
 new _F_(function($Paint__,$Paint__2,$Paint__3)
         {var $Paint__4=
           _e_(new _A_($Graphics.$UI.$WX.$Events.$Paint__CLS213__3__0,[$Paint__]));
          var $__8=
           {_tag_:0,_1:$Paint__2,_2:$Paint__3,_3:$Graphics.$UI.$WX.$Window.$Paint__DCT225__41__0DFLGraphics_2eUI_2eWX_2eEvents_2erepaint};
          return $__8;});
$Graphics.$UI.$WX.$Window.$Paint__NEW99UNQ204DCT225__41__0RDC=
 new _F_(function($Paint__,$Paint__2,$Paint__3)
         {var $Paint__4=
           new _A_($Graphics.$UI.$WX.$Window.$Paint__NEW103UNQ205EVLDCT225__41__0RDC,[$Paint__,$Paint__2,$Paint__3]);
          return $Paint__4;});
$Graphics.$UI.$WXCore.$EventClass.$__eventGetEventObject=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__eventGetEventObject;});
$Graphics.$UI.$WXCore.$EventClass.$eventGetEventObject=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$EventClass.$__eventGetEventObject,$Graphics.$UI.$WXCore.$EventClass.$event__Methods]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__windowGetGraphicsContext=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__windowGetGraphicsContext;});
$Graphics.$UI.$WXCore.$WindowClass.$windowGetGraphicsContext=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$WindowClass.$__windowGetGraphicsContext,$Graphics.$UI.$WXCore.$WindowClass.$window__Methods]);}),[]);
$Data.$Maybe.$fromJust=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW458__0;
          switch($__._tag_)
           {case 0:
             $__swJSW458__0=
              $__._1;
             break;
            case 1:
             var $__4=
              new _A_($UHC.$Base.$packedStringToString,["Maybe.fromJust: Nothing"]);
             var $__5=
              new _A_($UHC.$Base.$error,[$__4]);
             $__swJSW458__0=
              $__5;
             break;}
          return $__swJSW458__0;});
$Graphics.$UI.$WXCore.$WindowClass.$window__Methods=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$LightOO.$Core.$unRecord,$Graphics.$UI.$WXCore.$EvtHandlerClass.$get__EvtHandler__Tail]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__windowGetClientSize=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__windowGetClientSize;});
$Graphics.$UI.$WXCore.$WindowClass.$windowGetClientSize=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$WindowClass.$__windowGetClientSize,$Graphics.$UI.$WXCore.$WindowClass.$window__Methods]);}),[]);
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ43=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Types.$rect,[$UHC.$Base.$Num__DCT74__101__0,$_24x2,$_24x]);
          return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__]);});
$Graphics.$UI.$WXCore.$Types.$pointZero=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__3=
           new _A_($UHC.$Base.$fromInteger,[$__,$__2]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__5=
           new _A_($UHC.$Base.$fromInteger,[$__,$__4]);
          return new _A_($Graphics.$UI.$WXCore.$Types.$Point__,[$__5,$__3]);});
$Graphics.$UI.$WXCore.$WebWindow.$windowGetViewStart=
 new _F_(function($window)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Types.$pointZero,[$UHC.$Base.$Num__DCT74__101__0]);
          return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ39=
 new _F_(function($window,$_24x)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$windowGetViewStart,[$window]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ43,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Graphics.$UI.$WXCore.$WebWindow.$windowGetViewRect=
 new _F_(function($window)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$WindowClass.$windowGetClientSize,[$window]);
          var $__3=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ39,[$window]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__3]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ78=
 new _F_(function($handler,$_24x,$_24x3)
         {return new _A_($handler,[$_24x,$_24x3]);});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ73=
 new _F_(function($handler,$w,$_24x)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$w,$Graphics.$UI.$WXCore.$WebWindow.$windowGetViewRect]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ78,[$handler,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__5]);});
$Graphics.$UI.$WXCore.$WebWindow.$__199__1248NEW491=
 new _F_(function($handler,$window)
         {var $w=
           new _A_($Data.$Maybe.$fromJust,[$window]);
          var $__=
           new _A_($LightOO.$Core.$_23,[$w,$Graphics.$UI.$WXCore.$WindowClass.$windowGetGraphicsContext]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ73,[$handler,$w]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__5]);});
$Data.$Maybe.$isJust=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW460__0;
          switch($__._tag_)
           {case 0:
             $__swJSW460__0=
              $UHC.$Base.$True__;
             break;
            case 1:
             $__swJSW460__0=
              $UHC.$Base.$False__;
             break;}
          return $__swJSW460__0;});
$Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ62=
 new _F_(function($handler,$__,$_24x)
         {var $__4=
           new _A_($UHC.$Base.$packedStringToString,["Graphics/UI/WXCore/WebWindow.hs:332:16: monadic bind"]);
          var $__5=
           new _A_($UHC.$Base.$fail,[$UHC.$Base.$Monad__DCT74__339__0,$__4]);
          var $__6=
           _e_($_24x);
          var $__swJSW461__0;
          switch($__6._tag_)
           {case 0:
             var $window=
              new _A_($LightOO.$Core.$downcast,[$__,$__6._1]);
             var $__9=
              new _A_($Graphics.$UI.$WXCore.$WebWindow.$__199__1248NEW491,[$handler,$window]);
             var $__10=
              new _A_($UHC.$Base.$packedStringToString,["did not receive a window"]);
             var $__11=
              new _A_($UHC.$Base.$error,[$__10]);
             var $__12=
              new _A_($Data.$Maybe.$isJust,[$window]);
             var $__13=
              new _A_($UHC.$Base.$_24,[$UHC.$Base.$not,$__12]);
             var $__14=
              new _A_($Control.$Monad.$when,[$UHC.$Base.$Monad__DCT74__339__0,$__13,$__11]);
             $__swJSW461__0=
              new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__14,$__9]);
             break;
            case 1:
             $__swJSW461__0=
              $__5;
             break;}
          return $__swJSW461__0;});
$Graphics.$UI.$WXCore.$WebWindow.$onPaintUNQ55=
 new _F_(function($handler,$__,$r,$e)
         {var $__5=
           new _A_($LightOO.$Core.$_23,[$e,$Graphics.$UI.$WXCore.$EventClass.$eventGetEventObject]);
          var $__6=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$_24okUNQ62,[$handler,$__]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__5,$__6]);});
$Graphics.$UI.$WXCore.$WindowClass.$Widen__DCT159__2__0DFLLightOO_2eCore_2ewiden=
 new _F_(function($__,$o)
         {return new _A_($LightOO.$Core.$genericWiden,[$__,$o,$Graphics.$UI.$WXCore.$EvtHandlerClass.$get__EvtHandler__Tail,$Graphics.$UI.$WXCore.$EvtHandlerClass.$set__EvtHandler__Tail]);});
$Graphics.$UI.$WXCore.$WindowClass.$Widen__NEW725UNQ2462EVLDCT159__2__0RDC=
 new _F_(function($__,$Widen__)
         {var $Widen__3=
           _e_(new _A_($LightOO.$Core.$Widen__CLS69__6__0,[$Widen__]));
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$WindowClass.$Widen__DCT159__2__0DFLLightOO_2eCore_2ewiden,[$__]);
          var $__6=
           {_tag_:0,_1:$__5};
          return $__6;});
$Graphics.$UI.$WXCore.$WindowClass.$Widen__NEW722UNQ2459DCT159__2__0RDC=
 new _F_(function($__,$Widen__)
         {var $Widen__3=
           new _A_($Graphics.$UI.$WXCore.$WindowClass.$Widen__NEW725UNQ2462EVLDCT159__2__0RDC,[$__,$Widen__]);
          return $Widen__3;});
$Graphics.$UI.$WXCore.$WindowClass.$__161__2548__2__0UNQ2460=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$WindowClass.$Typeable__DCT159__9__0,[$Data.$Typeable.$Typeable__DCT320__26__0]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$Widen__UNQ2459DCT159__2__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$WindowClass.$Widen__NEW722UNQ2459DCT159__2__0RDC,[$Graphics.$UI.$WXCore.$WindowClass.$__161__2548__2__0UNQ2460,$Graphics.$UI.$WXCore.$WindowClass.$Widen__UNQ2459DCT159__2__0RDC]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$Widen__DCT159__2__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$WindowClass.$Widen__UNQ2459DCT159__2__0RDC;}),[]);
$LightOO.$Core.$Sup__DCT69__26__0DFLLightOO_2eCore_2edowncast=
 new _F_(function($__,$__2,$o)
         {var $__4=
           new _A_($LightOO.$Core.$widen,[$__2,$o]);
          var $__5=
           _e_($__4);
          var $__swJSW463__0;
          switch($__5._tag_)
           {case 0:
             var $__7=
              new _A_($LightOO.$Core.$downcast,[$__,$__5._1]);
             $__swJSW463__0=
              $__7;
             break;
            case 1:
             $__swJSW463__0=
              $UHC.$Base.$Nothing__;
             break;}
          return $__swJSW463__0;});
$LightOO.$Core.$Sup__NEW259UNQ252EVLDCT69__26__0RDC=
 new _F_(function($__,$Sup__,$__3)
         {var $Sup__4=
           _e_(new _A_($LightOO.$Core.$Sup__CLS69__4__0,[$Sup__]));
          var $__6=
           new _A_($LightOO.$Core.$Sup__DCT69__26__0DFLLightOO_2eCore_2edowncast,[$__,$__3]);
          var $__7=
           {_tag_:0,_1:$__6};
          return $__7;});
$LightOO.$Core.$Sup__NEW255UNQ249DCT69__26__0RDC=
 new _F_(function($__,$Sup__,$__3)
         {var $Sup__4=
           new _A_($LightOO.$Core.$Sup__NEW259UNQ252EVLDCT69__26__0RDC,[$__,$Sup__,$__3]);
          return $Sup__4;});
$LightOO.$Core.$Sup__DCT69__26__0=
 new _F_(function($__,$__2)
         {var $Sup__=
           _i_();
          _i_set_($Sup__,new _A_($LightOO.$Core.$Sup__NEW255UNQ249DCT69__26__0RDC,[$__,$Sup__,$__2]));
          return $Sup__;});
$LightOO.$Core.$Sup__NEW31UNQ231EVLDCT69__22__0RDC=
 new _F_(function($Sup__)
         {var $Sup__2=
           _e_(new _A_($LightOO.$Core.$Sup__CLS69__4__0,[$Sup__]));
          var $__4=
           {_tag_:0,_1:$UHC.$Base.$Just__};
          return $__4;});
$LightOO.$Core.$Sup__NEW29UNQ230DCT69__22__0RDC=
 new _F_(function($Sup__)
         {var $Sup__2=
           new _A_($LightOO.$Core.$Sup__NEW31UNQ231EVLDCT69__22__0RDC,[$Sup__]);
          return $Sup__2;});
$LightOO.$Core.$Sup__UNQ230DCT69__22__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$Sup__NEW29UNQ230DCT69__22__0RDC,[$LightOO.$Core.$Sup__UNQ230DCT69__22__0RDC]);}),[]);
$LightOO.$Core.$Sup__DCT69__22__0=
 new _A_(new _F_(function()
                 {return $LightOO.$Core.$Sup__UNQ230DCT69__22__0RDC;}),[]);
$LightOO.$Core.$Sup__CLS69__4__0=
 new _F_(function($Sup__)
         {var $Sup__2=
           {_tag_:0,_1:$UHC.$Base.$undefined};
          return $Sup__2;});
$LightOO.$Core.$widen=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$LightOO.$Core.$downcast=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$LightOO.$Core.$Sup__DCT69__19__0DFLLightOO_2eCore_2edowncast=
 new _F_(function($__,$__2,$o)
         {var $__4=
           new _A_($LightOO.$Core.$widen,[$__2,$o]);
          var $__5=
           _e_($__4);
          var $__swJSW468__0;
          switch($__5._tag_)
           {case 0:
             var $__7=
              new _A_($LightOO.$Core.$downcast,[$__,$__5._1]);
             $__swJSW468__0=
              $__7;
             break;
            case 1:
             $__swJSW468__0=
              $UHC.$Base.$Nothing__;
             break;}
          return $__swJSW468__0;});
$LightOO.$Core.$Sup__NEW229UNQ222EVLDCT69__19__0RDC=
 new _F_(function($__,$Sup__,$__3)
         {var $Sup__4=
           _e_(new _A_($LightOO.$Core.$Sup__CLS69__4__0,[$Sup__]));
          var $__6=
           new _A_($LightOO.$Core.$Sup__DCT69__19__0DFLLightOO_2eCore_2edowncast,[$__,$__3]);
          var $__7=
           {_tag_:0,_1:$__6};
          return $__7;});
$LightOO.$Core.$Sup__NEW225UNQ219DCT69__19__0RDC=
 new _F_(function($__,$Sup__,$__3)
         {var $Sup__4=
           new _A_($LightOO.$Core.$Sup__NEW229UNQ222EVLDCT69__19__0RDC,[$__,$Sup__,$__3]);
          return $Sup__4;});
$LightOO.$Core.$Sup__DCT69__19__0=
 new _F_(function($__,$__2)
         {var $Sup__=
           _i_();
          _i_set_($Sup__,new _A_($LightOO.$Core.$Sup__NEW225UNQ219DCT69__19__0RDC,[$__,$Sup__,$__2]));
          return $Sup__;});
$Graphics.$UI.$WXCore.$Types.$rectZero=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__3=
           new _A_($UHC.$Base.$fromInteger,[$__,$__2]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__5=
           new _A_($UHC.$Base.$fromInteger,[$__,$__4]);
          var $__6=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__7=
           new _A_($UHC.$Base.$fromInteger,[$__,$__6]);
          var $__8=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__9=
           new _A_($UHC.$Base.$fromInteger,[$__,$__8]);
          return new _A_($Graphics.$UI.$WXCore.$Types.$Rect__,[$__9,$__7,$__5,$__3]);});
$Data.$Typeable.$__324__368__0=
 new _F_(function($__,$s1,$__3)
         {var $__4=
           _e_($__3);
          var $__7=
           new _A_($UHC.$Base.$_3d_3d,[$__,$s1,$__4._2]);
          return $__7;});
$Data.$Typeable.$Eq__DCT320__1__0DFLUHC_2eBase_2e_3d_3d=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          return new _A_($Data.$Typeable.$__324__368__0,[$__,$__3._2]);});
$Data.$Typeable.$Eq__NEW163UNQ519EVLDCT320__1__0RDC=
 new _F_(function($__,$Eq__)
         {var $Eq__3=
           _e_(new _A_($UHC.$Base.$Eq__CLS74__4__0,[$Eq__]));
          var $__6=
           new _A_($Data.$Typeable.$Eq__DCT320__1__0DFLUHC_2eBase_2e_3d_3d,[$__]);
          var $__7=
           {_tag_:0,_1:$Eq__3._1,_2:$__6};
          return $__7;});
$Data.$Typeable.$Eq__NEW160UNQ516DCT320__1__0RDC=
 new _F_(function($__,$Eq__)
         {var $Eq__3=
           new _A_($Data.$Typeable.$Eq__NEW163UNQ519EVLDCT320__1__0RDC,[$__,$Eq__]);
          return $Eq__3;});
$UHC.$Base.$primEqChar=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primEqInt($__3,$__4);});
$UHC.$Base.$Eq__NEW1755UNQ10102EVLDCT74__56__0RDC=
 new _F_(function($Eq__)
         {var $Eq__2=
           _e_(new _A_($UHC.$Base.$Eq__CLS74__4__0,[$Eq__]));
          var $__5=
           {_tag_:0,_1:$Eq__2._1,_2:$UHC.$Base.$primEqChar};
          return $__5;});
$UHC.$Base.$Eq__NEW1753UNQ10101DCT74__56__0RDC=
 new _F_(function($Eq__)
         {var $Eq__2=
           new _A_($UHC.$Base.$Eq__NEW1755UNQ10102EVLDCT74__56__0RDC,[$Eq__]);
          return $Eq__2;});
$UHC.$Base.$Eq__UNQ10101DCT74__56__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq__NEW1753UNQ10101DCT74__56__0RDC,[$UHC.$Base.$Eq__UNQ10101DCT74__56__0RDC]);}),[]);
$UHC.$Base.$Eq__DCT74__56__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Eq__UNQ10101DCT74__56__0RDC;}),[]);
$Data.$Typeable.$__322__4630__2__0UNQ517=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq__DCT74__394__0,[$UHC.$Base.$Eq__DCT74__56__0]);}),[]);
$Data.$Typeable.$Eq__UNQ516DCT320__1__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Typeable.$Eq__NEW160UNQ516DCT320__1__0RDC,[$Data.$Typeable.$__322__4630__2__0UNQ517,$Data.$Typeable.$Eq__UNQ516DCT320__1__0RDC]);}),[]);
$Data.$Typeable.$Eq__DCT320__1__0=
 new _A_(new _F_(function()
                 {return $Data.$Typeable.$Eq__UNQ516DCT320__1__0RDC;}),[]);
$Data.$Typeable.$__324__388__0=
 new _F_(function($__,$c1,$r1,$__4)
         {var $__5=
           _e_($__4);
          var $__9=
           new _A_($UHC.$Base.$_3d_3d,[$__,$r1,$__5._3]);
          var $__10=
           new _A_($UHC.$Base.$_3d_3d,[$Data.$Typeable.$Eq__DCT320__1__0,$c1,$__5._2]);
          var $__11=
           new _A_($UHC.$Base.$_26_26,[$__10,$__9]);
          return $__11;});
$Data.$Typeable.$Eq__DCT320__0__0DFLUHC_2eBase_2e_3d_3d=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          return new _A_($Data.$Typeable.$__324__388__0,[$__,$__3._2,$__3._3]);});
$Data.$Typeable.$Eq__NEW180UNQ536EVLDCT320__0__0RDC=
 new _F_(function($Eq__,$__)
         {var $Eq__3=
           _e_(new _A_($UHC.$Base.$Eq__CLS74__4__0,[$Eq__]));
          var $__6=
           new _A_($Data.$Typeable.$Eq__DCT320__0__0DFLUHC_2eBase_2e_3d_3d,[$__]);
          var $__7=
           {_tag_:0,_1:$Eq__3._1,_2:$__6};
          return $__7;});
$Data.$Typeable.$Eq__NEW177UNQ533DCT320__0__0RDC=
 new _F_(function($Eq__,$__)
         {var $Eq__3=
           new _A_($Data.$Typeable.$Eq__NEW180UNQ536EVLDCT320__0__0RDC,[$Eq__,$__]);
          return $Eq__3;});
$UHC.$Base.$__Rep0_5b_5dDFLUHC_2eBase_2eto0GENRepresentable0=
 new _F_(function($proj__1)
         {var $proj__2=
           _e_($proj__1);
          var $__swJSW477__0;
          switch($proj__2._tag_)
           {case 0:
             var $proj__4=
              _e_($proj__2.unL1);
             $__swJSW477__0=
              $UHC.$Base.$_5b_5d;
             break;
            case 1:
             var $proj__6=
              _e_($proj__2.unR1);
             var $__=
              new _A_($UHC.$Base.$_3a,[$proj__6._1,$proj__6._2]);
             $__swJSW477__0=
              $__;
             break;}
          return $__swJSW477__0;});
$UHC.$Base.$__Rep0_5b_5dDFLUHC_2eBase_2efrom0GENRepresentable0=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          var $__swJSW480__0;
          switch($x2._tag_)
           {case 0:
             var $__5=
              new _A_($UHC.$Base.$K1__,[$x2._2]);
             var $__6=
              new _A_($UHC.$Base.$M1__,[$__5]);
             var $__7=
              new _A_($UHC.$Base.$K1__,[$x2._1]);
             var $__8=
              new _A_($UHC.$Base.$M1__,[$__7]);
             var $__9=
              new _A_($UHC.$Base.$_3a_2a_3a,[$__8,$__6]);
             var $__10=
              new _A_($UHC.$Base.$M1__,[$__9]);
             var $__11=
              new _A_($UHC.$Base.$R1__,[$__10]);
             var $__12=
              new _A_($UHC.$Base.$M1__,[$__11]);
             $__swJSW480__0=
              $__12;
             break;
            case 1:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__14=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__15=
              new _A_($UHC.$Base.$M1__,[$__14]);
             $__swJSW480__0=
              $__15;
             break;}
          return $__swJSW480__0;});
$UHC.$Base.$__Rep0_5b_5dNEW1144UNQ1262EVLSDCGENRepresentable0=
 new _F_(function($__)
         {var $Representable0__=
           _e_(new _A_($UHC.$Base.$Representable0__CLS74__369__0,[$__]));
          var $__5=
           {_tag_:0,_1:$UHC.$Base.$__Rep0_5b_5dDFLUHC_2eBase_2efrom0GENRepresentable0,_2:$UHC.$Base.$__Rep0_5b_5dDFLUHC_2eBase_2eto0GENRepresentable0};
          return $__5;});
$UHC.$Base.$__Rep0_5b_5dNEW1142UNQ1261SDCGENRepresentable0=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$__Rep0_5b_5dNEW1144UNQ1262EVLSDCGENRepresentable0,[$__]);
          return $__2;});
$UHC.$Base.$__Rep0_5b_5dUNQ1261SDCGENRepresentable0=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$__Rep0_5b_5dNEW1142UNQ1261SDCGENRepresentable0,[$UHC.$Base.$__Rep0_5b_5dUNQ1261SDCGENRepresentable0]);}),[]);
$UHC.$Base.$__Rep0_5b_5dGENRepresentable0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$__Rep0_5b_5dUNQ1261SDCGENRepresentable0;}),[]);
$UHC.$Base.$Eq__NEW1960UNQ10082EVLDCT74__394__0RDC=
 new _F_(function($Eq__,$Eq__2)
         {var $Eq__3=
           _e_(new _A_($UHC.$Base.$Eq__CLS74__4__0,[$Eq__]));
          var $__6=
           {_tag_:0,_1:$Eq__3._1,_2:$Eq__2};
          return $__6;});
$UHC.$Base.$Eq__NEW1949UNQ10070DCT74__394__0RDC=
 new _F_(function($Eq__,$__,$__3,$__4,$__5,$__6,$__7,$__8,$Eq__9,$__10)
         {var $Eq__11=
           new _A_($UHC.$Base.$Eq__NEW1960UNQ10082EVLDCT74__394__0RDC,[$Eq__,$Eq__9]);
          return $Eq__11;});
$UHC.$Base.$Eq__DCT74__394__0=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$UHC.$Base.$Eq_27__DCT74__389__0]);
          var $__3=
           new _A_($UHC.$Base.$Eq_27__DCT74__390__0,[$__]);
          var $__4=
           new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$__3]);
          var $Eq__=
           _i_();
          var $__6=
           _i_();
          var $__7=
           _i_();
          var $__8=
           _i_();
          var $__9=
           _i_();
          var $__10=
           _i_();
          var $Eq__DCT74__394__0DFLUHC_2eBase_2e_3d_3d=
           _i_();
          var $__11=
           _i_();
          _i_set_($Eq__,new _A_($UHC.$Base.$Eq__NEW1949UNQ10070DCT74__394__0RDC,[$Eq__,$__6,$__4,$__8,$__9,$__2,$__10,$__7,$Eq__DCT74__394__0DFLUHC_2eBase_2e_3d_3d,$__11]));
          _i_set_($__6,new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$__7]));
          _i_set_($__7,new _A_($UHC.$Base.$Eq_27__DCT74__390__0,[$Eq__]));
          _i_set_($__8,new _A_($UHC.$Base.$Eq_27__DCT74__393__0,[$__4,$__6]));
          _i_set_($__9,new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$__8]));
          _i_set_($__10,new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$__2,$__9]));
          _i_set_($Eq__DCT74__394__0DFLUHC_2eBase_2e_3d_3d,new _A_($UHC.$Base.$geqdefault,[$UHC.$Base.$__Rep0_5b_5dGENRepresentable0,$__11,$UHC.$Base.$undefined]));
          _i_set_($__11,new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$__10]));
          return $Eq__;});
$Data.$Typeable.$Eq__UNQ533DCT320__0__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Typeable.$Eq__NEW177UNQ533DCT320__0__0RDC,[$Data.$Typeable.$Eq__UNQ533DCT320__0__0RDC,$Data.$Typeable.$__322__4734__2__0UNQ534]);}),[]);
$Data.$Typeable.$__322__4734__2__0UNQ534=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq__DCT74__394__0,[$Data.$Typeable.$Eq__UNQ533DCT320__0__0RDC]);}),[]);
$Data.$Typeable.$Eq__DCT320__0__0=
 new _A_(new _F_(function()
                 {return $Data.$Typeable.$Eq__UNQ533DCT320__0__0RDC;}),[]);
$Data.$Dynamic.$fromDynamic=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          var $__6=
           new _A_($UHC.$Base.$unsafeCoerce,[$__3._2]);
          var $__7=
           new _A_($Data.$Typeable.$typeOf,[$__,$__6]);
          var $__8=
           new _A_($UHC.$Base.$_3d_3d,[$Data.$Typeable.$Eq__DCT320__0__0,$__3._1,$__7]);
          var $__9=
           _e_($__8);
          var $__swJSW484__0;
          switch($__9._tag_)
           {case 0:
             var $__10=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW485__0;
             switch($__10._tag_)
              {case 0:
                $__swJSW485__0=
                 $UHC.$Base.$undefined;
                break;
               case 1:
                $__swJSW485__0=
                 $UHC.$Base.$Nothing__;
                break;}
             $__swJSW484__0=
              $__swJSW485__0;
             break;
            case 1:
             var $__11=
              new _A_($UHC.$Base.$Just__,[$__6]);
             $__swJSW484__0=
              $__11;
             break;}
          return $__swJSW484__0;});
$LightOO.$Core.$genericWiden=
 new _F_(function($__,$o,$getTail,$setTail)
         {var $__5=
           new _A_($getTail,[$o]);
          var $__6=
           _e_($__5);
          var $__swJSW486__0;
          switch($__6._tag_)
           {case 0:
             var $__8=
              new _A_($UHC.$Base.$packedStringToString,["invariant broken: nil case should be caught by a reflexivity instance"]);
             var $__9=
              new _A_($UHC.$Base.$error,[$__8]);
             $__swJSW486__0=
              $__9;
             break;
            case 1:
             var $__11=
              new _A_($Data.$Dynamic.$fromDynamic,[$__,$__6._1]);
             var $__12=
              new _A_($setTail,[$o]);
             var $__13=
              new _A_($UHC.$Base.$_2e,[$__12,$LightOO.$Core.$record]);
             var $__14=
              new _A_($UHC.$Base.$_2e,[$UHC.$Base.$Just__,$__13]);
             var $__15=
              new _A_($UHC.$Base.$maybe,[$UHC.$Base.$Nothing__,$__14,$__11]);
             $__swJSW486__0=
              $__15;
             break;}
          return $__swJSW486__0;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$Widen__DCT153__2__0DFLLightOO_2eCore_2ewiden=
 new _F_(function($__,$o)
         {return new _A_($LightOO.$Core.$genericWiden,[$__,$o,$LightOO.$get__Object__Tail,$LightOO.$set__Object__Tail]);});
$LightOO.$Core.$Widen__CLS69__6__0=
 new _F_(function($Widen__)
         {var $Widen__2=
           {_tag_:0,_1:$UHC.$Base.$undefined};
          return $Widen__2;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$Widen__NEW508UNQ1194EVLDCT153__2__0RDC=
 new _F_(function($__,$Widen__)
         {var $Widen__3=
           _e_(new _A_($LightOO.$Core.$Widen__CLS69__6__0,[$Widen__]));
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$Widen__DCT153__2__0DFLLightOO_2eCore_2ewiden,[$__]);
          var $__6=
           {_tag_:0,_1:$__5};
          return $__6;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$Widen__NEW505UNQ1191DCT153__2__0RDC=
 new _F_(function($__,$Widen__)
         {var $Widen__3=
           new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$Widen__NEW508UNQ1194EVLDCT153__2__0RDC,[$__,$Widen__]);
          return $Widen__3;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$__155__964__2__0UNQ1192=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$Typeable__DCT153__9__0,[$Data.$Typeable.$Typeable__DCT320__26__0]);}),[]);
$Graphics.$UI.$WXCore.$EvtHandlerClass.$Widen__UNQ1191DCT153__2__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$Widen__NEW505UNQ1191DCT153__2__0RDC,[$Graphics.$UI.$WXCore.$EvtHandlerClass.$__155__964__2__0UNQ1192,$Graphics.$UI.$WXCore.$EvtHandlerClass.$Widen__UNQ1191DCT153__2__0RDC]);}),[]);
$Graphics.$UI.$WXCore.$EvtHandlerClass.$Widen__DCT153__2__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$EvtHandlerClass.$Widen__UNQ1191DCT153__2__0RDC;}),[]);
$Graphics.$UI.$WXCore.$Types.$wxEVT__PAINT=
 new _A_(new _F_(function()
                 {return 2;}),[]);
$Graphics.$UI.$WXCore.$WebWindow.$windowOnPaint=
 new _F_(function($w,$handler)
         {var $__=
           new _A_($LightOO.$Core.$Sup__DCT69__26__0,[$LightOO.$Core.$Sup__DCT69__22__0,$Graphics.$UI.$WXCore.$WindowClass.$Widen__DCT159__2__0]);
          var $__4=
           new _A_($LightOO.$Core.$Sup__DCT69__19__0,[$__,$Graphics.$UI.$WXCore.$EvtHandlerClass.$Widen__DCT153__2__0]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$Types.$rectZero,[$UHC.$Base.$Num__DCT74__134__0]);
          var $__6=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$onPaintUNQ55,[$handler,$__4,$__5]);
          return new _A_($LightOO.$Core.$_23,[$w,$Graphics.$UI.$WXCore.$EvtHandlerClass.$evtHandlerBind,$Graphics.$UI.$WXCore.$Types.$wxEVT__PAINT,$__6,$Graphics.$UI.$WXCore.$Types.$idAny,$Graphics.$UI.$WXCore.$Types.$idAny]);});
$Graphics.$UI.$WXCore.$WebWindow.$windowGetOnPaint=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$undefined;}),[]);
$Graphics.$UI.$WX.$Window.$__229__156=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["paint"]);}),[]);
$Graphics.$UI.$WX.$Window.$Paint__DCT225__41__0DFLGraphics_2eUI_2eWX_2eEvents_2epaint=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Events.$newEvent,[$Graphics.$UI.$WX.$Window.$__229__156,$Graphics.$UI.$WXCore.$WebWindow.$windowGetOnPaint,$Graphics.$UI.$WXCore.$WebWindow.$windowOnPaint]);}),[]);
$Graphics.$UI.$WX.$Window.$Paint__UNQ204DCT225__41__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Window.$Paint__NEW99UNQ204DCT225__41__0RDC,[$Graphics.$UI.$WX.$Window.$Paint__UNQ204DCT225__41__0RDC,$Graphics.$UI.$WX.$Window.$Paint__DCT225__41__0DFLGraphics_2eUI_2eWX_2eEvents_2epaint,$Graphics.$UI.$WX.$Window.$Paint__DCT225__41__0DFLGraphics_2eUI_2eWX_2eEvents_2epaintRaw]);}),[]);
$Graphics.$UI.$WX.$Window.$Paint__DCT225__41__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WX.$Window.$Paint__UNQ204DCT225__41__0RDC;}),[]);
$Graphics.$UI.$WXCore.$Types.$varGet=
 new _A_(new _F_(function()
                 {return $UHC.$IOBase.$readIORef;}),[]);
$Graphics.$UI.$WXCore.$Types.$varSet=
 new _A_(new _F_(function()
                 {return $UHC.$IOBase.$writeIORef;}),[]);
$UHC.$Base.$fail=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._3;});
$Graphics.$UI.$WX.$Events.$repaint=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._3;});
$Asteroids.$_24okUNQ54=
 new _F_(function($__,$vrocks,$f,$_24x)
         {var $__5=
           new _A_($UHC.$Base.$packedStringToString,["Asteroids.hs-cpp:64:14: monadic bind"]);
          var $__6=
           new _A_($UHC.$Base.$fail,[$UHC.$Base.$Monad__DCT74__339__0,$__5]);
          var $__7=
           _e_($_24x);
          var $__swJSW490__0;
          switch($__7._tag_)
           {case 0:
             var $__10=
              new _A_($Graphics.$UI.$WX.$Events.$repaint,[$__,$f]);
             var $__11=
              new _A_($Graphics.$UI.$WXCore.$Types.$varSet,[$vrocks,$__7._2]);
             var $__12=
              new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__11,$__10]);
             $__swJSW490__0=
              $__12;
             break;
            case 1:
             $__swJSW490__0=
              $__6;
             break;}
          return $__swJSW490__0;});
$Asteroids.$advance=
 new _F_(function($__,$vrocks,$f)
         {var $__4=
           new _A_($Graphics.$UI.$WXCore.$Types.$varGet,[$vrocks]);
          var $__5=
           new _A_($Asteroids.$_24okUNQ54,[$__,$vrocks,$f]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__5]);});
$Graphics.$UI.$WXCore.$WindowClass.$Narrow__NEW714UNQ2451EVLDCT159__0__0RDC=
 new _F_(function($Narrow__,$Narrow__2)
         {var $Narrow__3=
           _e_(new _A_($LightOO.$Core.$Narrow__CLS69__5__0,[$Narrow__2]));
          var $__5=
           {_tag_:0,_1:$Narrow__};
          return $__5;});
$Graphics.$UI.$WXCore.$WindowClass.$Narrow__NEW711UNQ2448DCT159__0__0RDC=
 new _F_(function($Narrow__,$Narrow__2)
         {var $Narrow__3=
           new _A_($Graphics.$UI.$WXCore.$WindowClass.$Narrow__NEW714UNQ2451EVLDCT159__0__0RDC,[$Narrow__,$Narrow__2]);
          return $Narrow__3;});
$Graphics.$UI.$WXCore.$WindowClass.$__163__1250=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["Window"]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$windowTc=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Typeable.$mkTyCon,[$Graphics.$UI.$WXCore.$WindowClass.$__163__1250]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$Typeable1__DCT159__8__0DFLData_2eTypeable_2etypeOf1=
 new _F_(function($__)
         {return new _A_($Data.$Typeable.$mkTyConApp,[$Graphics.$UI.$WXCore.$WindowClass.$windowTc,$UHC.$Base.$_5b_5d]);});
$Graphics.$UI.$WXCore.$WindowClass.$Typeable1__NEW677UNQ2455EVLDCT159__8__0RDC=
 new _F_(function($Typeable1__)
         {var $Typeable1__2=
           _e_(new _A_($Data.$Typeable.$Typeable1__CLS320__8__0,[$Typeable1__]));
          var $__4=
           {_tag_:0,_1:$Graphics.$UI.$WXCore.$WindowClass.$Typeable1__DCT159__8__0DFLData_2eTypeable_2etypeOf1};
          return $__4;});
$Graphics.$UI.$WXCore.$WindowClass.$Typeable1__NEW675UNQ2454DCT159__8__0RDC=
 new _F_(function($Typeable1__)
         {var $Typeable1__2=
           new _A_($Graphics.$UI.$WXCore.$WindowClass.$Typeable1__NEW677UNQ2455EVLDCT159__8__0RDC,[$Typeable1__]);
          return $Typeable1__2;});
$Graphics.$UI.$WXCore.$WindowClass.$Typeable1__UNQ2454DCT159__8__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$WindowClass.$Typeable1__NEW675UNQ2454DCT159__8__0RDC,[$Graphics.$UI.$WXCore.$WindowClass.$Typeable1__UNQ2454DCT159__8__0RDC]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$Typeable1__DCT159__8__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$WindowClass.$Typeable1__UNQ2454DCT159__8__0RDC;}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$Typeable__NEW702UNQ2470EVLDCT159__9__0RDC=
 new _F_(function($Typeable__,$Typeable__2)
         {var $Typeable__3=
           _e_(new _A_($Data.$Typeable.$Typeable__CLS320__7__0,[$Typeable__2]));
          var $__5=
           {_tag_:0,_1:$Typeable__};
          return $__5;});
$Graphics.$UI.$WXCore.$WindowClass.$Typeable__NEW699UNQ2467DCT159__9__0RDC=
 new _F_(function($Typeable__,$Typeable__2)
         {var $Typeable__3=
           new _A_($Graphics.$UI.$WXCore.$WindowClass.$Typeable__NEW702UNQ2470EVLDCT159__9__0RDC,[$Typeable__,$Typeable__2]);
          return $Typeable__3;});
$Graphics.$UI.$WXCore.$WindowClass.$Typeable__DCT159__9__0=
 new _F_(function($__)
         {var $Typeable__DCT159__9__0DFLData_2eTypeable_2etypeOf=
           new _A_($Data.$Typeable.$typeOfDefault,[$Graphics.$UI.$WXCore.$WindowClass.$Typeable1__DCT159__8__0,$__]);
          var $Typeable__=
           _i_();
          _i_set_($Typeable__,new _A_($Graphics.$UI.$WXCore.$WindowClass.$Typeable__NEW699UNQ2467DCT159__9__0RDC,[$Typeable__DCT159__9__0DFLData_2eTypeable_2etypeOf,$Typeable__]));
          return $Typeable__;});
$Graphics.$UI.$WXCore.$WindowClass.$__161__2459__1__0UNQ2449=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$WindowClass.$Typeable__DCT159__9__0,[$Data.$Typeable.$Typeable__DCT320__26__0]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$__163__1307=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$hideRecord,[$Graphics.$UI.$WXCore.$WindowClass.$__161__2459__1__0UNQ2449]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$Narrow__DCT159__0__0DFLLightOO_2eCore_2enarrow=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$modify__EvtHandler__Tail,[$Graphics.$UI.$WXCore.$WindowClass.$__163__1307]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$Narrow__UNQ2448DCT159__0__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$WindowClass.$Narrow__NEW711UNQ2448DCT159__0__0RDC,[$Graphics.$UI.$WXCore.$WindowClass.$Narrow__DCT159__0__0DFLLightOO_2eCore_2enarrow,$Graphics.$UI.$WXCore.$WindowClass.$Narrow__UNQ2448DCT159__0__0RDC]);}),[]);
$Graphics.$UI.$WXCore.$WindowClass.$Narrow__DCT159__0__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$WindowClass.$Narrow__UNQ2448DCT159__0__0RDC;}),[]);
$Graphics.$UI.$WXCore.$WebWindow.$__197__71__2__0=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$Sub__DCT69__24__0,[$LightOO.$Core.$Sub__DCT69__13__0,$Graphics.$UI.$WXCore.$WindowClass.$Narrow__DCT159__0__0]);}),[]);
$LightOO.$Core.$Sub__NEW22UNQ309EVLDCT69__20__0RDC=
 new _F_(function($Sub__)
         {var $Sub__2=
           _e_(new _A_($LightOO.$Core.$Sub__CLS69__3__0,[$Sub__]));
          var $__4=
           {_tag_:0,_1:$UHC.$Base.$id};
          return $__4;});
$LightOO.$Core.$Sub__NEW20UNQ308DCT69__20__0RDC=
 new _F_(function($Sub__)
         {var $Sub__2=
           new _A_($LightOO.$Core.$Sub__NEW22UNQ309EVLDCT69__20__0RDC,[$Sub__]);
          return $Sub__2;});
$LightOO.$Core.$Sub__UNQ308DCT69__20__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$Sub__NEW20UNQ308DCT69__20__0RDC,[$LightOO.$Core.$Sub__UNQ308DCT69__20__0RDC]);}),[]);
$LightOO.$Core.$Sub__DCT69__20__0=
 new _A_(new _F_(function()
                 {return $LightOO.$Core.$Sub__UNQ308DCT69__20__0RDC;}),[]);
$Graphics.$UI.$WXCore.$EvtHandler.$_24okUNQ11=
 new _F_(function($__,$b,$_24x)
         {var $__4=
           _e_($_24x);
          var $__swJSW495__0;
          switch($__4._tag_)
           {case 0:
             var $__5=
              new _A_($UHC.$Base.$return,[$__,$UHC.$Base.$False__]);
             $__swJSW495__0=
              $__5;
             break;
            case 1:
             $__swJSW495__0=
              $b;
             break;}
          return $__swJSW495__0;});
$Graphics.$UI.$WXCore.$EvtHandler.$onlyIf=
 new _F_(function($__,$a,$b)
         {var $__4=
           new _A_($Graphics.$UI.$WXCore.$EvtHandler.$_24okUNQ11,[$__,$b]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$a,$__4]);});
$Graphics.$UI.$WXCore.$EvtHandler.$beforeUNQ92=
 new _F_(function($__)
         {return new _A_($UHC.$Base.$return,[$__,$UHC.$Base.$True__]);});
$Graphics.$UI.$WXCore.$EvtHandler.$afterUNQ90=
 new _F_(function($__)
         {return new _A_($UHC.$Base.$return,[$__,$UHC.$Base.$True__]);});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$__evtHandlerProcessEventLocally=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__evtHandlerProcessEventLocally;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$evtHandlerProcessEventLocally=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$EvtHandlerClass.$__evtHandlerProcessEventLocally,$Graphics.$UI.$WXCore.$EvtHandlerClass.$evtHandler__Methods]);}),[]);
$Graphics.$UI.$WXCore.$EvtHandler.$__185__296__0UNQ88=
 new _F_(function($self,$evt)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$self,$Graphics.$UI.$WXCore.$EvtHandlerClass.$evtHandlerProcessEventLocally]);
          var $processLocally=
           new _A_($UHC.$Base.$_24,[$__,$evt]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$EvtHandler.$afterUNQ90,[$UHC.$Base.$Monad__DCT74__339__0]);
          var $__6=
           new _A_($Graphics.$UI.$WXCore.$EvtHandler.$beforeUNQ92,[$UHC.$Base.$Monad__DCT74__339__0]);
          var $__7=
           new _A_($Graphics.$UI.$WXCore.$EvtHandler.$onlyIf,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$processLocally]);
          return new _A_($Graphics.$UI.$WXCore.$EvtHandler.$onlyIf,[$UHC.$Base.$Monad__DCT74__339__0,$__7,$__5]);});
$Graphics.$UI.$WXCore.$EvtHandler.$doUnbindUNQ111=
 new _F_(function($evtty,$id,$lid,$entry)
         {var $elid=
           new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$dynamicEventTableEntryLastId,[$entry]);
          var $eid=
           new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$dynamicEventTableEntryId,[$entry]);
          var $eevtty=
           new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$dynamicEventTableEntryEventType,[$entry]);
          var $__=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,$eevtty,$evtty]);
          var $__9=
           new _A_($Graphics.$UI.$WXCore.$Types.$wxID__ANY,[$UHC.$Base.$Num__DCT74__101__0]);
          var $__10=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,$lid,$__9]);
          var $__11=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,$elid,$lid]);
          var $__12=
           new _A_($UHC.$Base.$_7c_7c,[$__11,$__10]);
          var $__13=
           new _A_($UHC.$Base.$_26_26,[$__12,$__]);
          var $__14=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,$eid,$id]);
          var $__15=
           new _A_($UHC.$Base.$_26_26,[$__14,$__13]);
          return new _A_($UHC.$Base.$_24,[$UHC.$Base.$not,$__15]);});
$Graphics.$UI.$WXCore.$EvtHandler.$__185__302__0UNQ102=
 new _F_(function($_24x,$evtty,$id,$lid)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$EvtHandler.$doUnbindUNQ111,[$evtty,$id,$lid]);
          var $__6=
           new _A_($UHC.$Base.$filter,[$__]);
          return new _A_($Data.$IORef.$modifyIORef,[$_24x,$__6]);});
$Graphics.$UI.$WXCore.$EvtHandler.$lookupEntryByEventType=
 new _F_(function($t)
         {var $__=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,$t]);
          var $__3=
           new _A_($UHC.$Base.$_2e,[$__,$Graphics.$UI.$WXCore.$EvtHandlerClass.$dynamicEventTableEntryEventType]);
          return new _A_($UHC.$Base.$_24,[$UHC.$Base.$filter,$__3]);});
$Graphics.$UI.$WXCore.$EventClass.$__eventSkip=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__eventSkip;});
$Graphics.$UI.$WXCore.$EventClass.$eventSkip=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$EventClass.$__eventSkip,$Graphics.$UI.$WXCore.$EventClass.$event__Methods]);}),[]);
$Graphics.$UI.$WXCore.$EvtHandler.$_24okUNQ70=
 new _F_(function($evt,$entry,$lid,$id,$_24x)
         {var $__=
           new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__91__0,$_24x,$lid]);
          var $__7=
           new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__91__0,$_24x,$id]);
          var $__8=
           new _A_($UHC.$Base.$_26_26,[$__7,$__]);
          var $__9=
           new _A_($Graphics.$UI.$WXCore.$Types.$wxID__ANY,[$UHC.$Base.$Num__DCT74__101__0]);
          var $__10=
           new _A_($UHC.$Base.$_2f_3d,[$UHC.$Base.$Eq__DCT74__88__0,$lid,$__9]);
          var $__11=
           new _A_($UHC.$Base.$_26_26,[$__10,$__8]);
          var $__12=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,$id,$_24x]);
          var $__13=
           new _A_($Graphics.$UI.$WXCore.$Types.$wxID__ANY,[$UHC.$Base.$Num__DCT74__101__0]);
          var $__14=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,$lid,$__13]);
          var $__15=
           new _A_($UHC.$Base.$_26_26,[$__14,$__12]);
          var $__16=
           new _A_($UHC.$Base.$_7c_7c,[$__15,$__11]);
          var $__17=
           new _A_($Graphics.$UI.$WXCore.$Types.$wxID__ANY,[$UHC.$Base.$Num__DCT74__101__0]);
          var $__18=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,$id,$__17]);
          var $__19=
           new _A_($UHC.$Base.$_7c_7c,[$__18,$__16]);
          var $__20=
           _e_($__19);
          var $__swJSW498__0;
          switch($__20._tag_)
           {case 0:
             var $__21=
              new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$UHC.$Base.$True__]);
             $__swJSW498__0=
              $__21;
             break;
            case 1:
             var $__22=
              new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$UHC.$Base.$True__]);
             var $__23=
              new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$dynamicEventTableEntryFunc,[$entry,$evt]);
             var $__24=
              new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__23,$__22]);
             var $__25=
              new _A_($LightOO.$Core.$_23,[$evt,$Graphics.$UI.$WXCore.$EventClass.$eventSkip]);
             var $__26=
              new _A_($UHC.$Base.$_24,[$__25,$UHC.$Base.$False__]);
             var $__27=
              new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__26,$__24]);
             $__swJSW498__0=
              $__27;
             break;}
          return $__swJSW498__0;});
$Graphics.$UI.$WXCore.$EventClass.$__eventGetId=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__eventGetId;});
$Graphics.$UI.$WXCore.$EventClass.$eventGetId=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$EventClass.$__eventGetId,$Graphics.$UI.$WXCore.$EventClass.$event__Methods]);}),[]);
$Graphics.$UI.$WXCore.$EvtHandler.$processEventIfMatchesIdUNQ49=
 new _F_(function($evt,$entry)
         {var $id=
           new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$dynamicEventTableEntryId,[$entry]);
          var $lid=
           new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$dynamicEventTableEntryLastId,[$entry]);
          var $__=
           new _A_($LightOO.$Core.$_23,[$evt,$Graphics.$UI.$WXCore.$EventClass.$eventGetId]);
          var $__6=
           new _A_($Graphics.$UI.$WXCore.$EvtHandler.$_24okUNQ70,[$evt,$entry,$lid,$id]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__6]);});
$Graphics.$UI.$WXCore.$EvtHandler.$_24okUNQ83=
 new _F_(function($evt,$_24x,$_24x3)
         {var $matchingHandlers=
           new _A_($Graphics.$UI.$WXCore.$EvtHandler.$lookupEntryByEventType,[$_24x3,$_24x]);
          var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$UHC.$Base.$True__]);
          var $__6=
           new _A_($Graphics.$UI.$WXCore.$EvtHandler.$processEventIfMatchesIdUNQ49,[$evt]);
          var $__7=
           new _A_($UHC.$Base.$mapM__,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$matchingHandlers]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__7,$__]);});
$Graphics.$UI.$WXCore.$EventClass.$__eventGetType=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__eventGetType;});
$Graphics.$UI.$WXCore.$EventClass.$eventGetType=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$EventClass.$__eventGetType,$Graphics.$UI.$WXCore.$EventClass.$event__Methods]);}),[]);
$Graphics.$UI.$WXCore.$EvtHandler.$_24okUNQ52=
 new _F_(function($evt,$_24x)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$evt,$Graphics.$UI.$WXCore.$EventClass.$eventGetType]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$EvtHandler.$_24okUNQ83,[$evt,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Graphics.$UI.$WXCore.$EvtHandler.$__185__295__0UNQ46=
 new _F_(function($_24x,$evt)
         {var $__=
           new _A_($UHC.$IOBase.$readIORef,[$_24x]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$EvtHandler.$_24okUNQ52,[$evt]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Data.$IORef.$modifyIORef=
 new _F_(function($ref,$f)
         {var $__=
           new _A_($UHC.$IOBase.$writeIORef,[$ref]);
          var $__4=
           new _A_($UHC.$Base.$_2e,[$__,$f]);
          var $__5=
           new _A_($UHC.$IOBase.$readIORef,[$ref]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__5,$__4]);});
$Graphics.$UI.$WXCore.$EvtHandler.$entryNEW121UNQ142=
 new _F_(function($evtty,$f,$id,$lastId)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["dynamicEventTableEntryUserData not implemented"]);
          var $__6=
           new _A_($UHC.$Base.$error,[$__]);
          return {_tag_:0,dynamicEventTableEntryEventType:$evtty,dynamicEventTableEntryId:$id,dynamicEventTableEntryLastId:$lastId,dynamicEventTableEntryFunc:$f,dynamicEventTableEntryUserData:$__6};});
$Graphics.$UI.$WXCore.$EvtHandler.$__185__304__0UNQ137=
 new _F_(function($_24x,$evtty,$f,$id,$lastId)
         {var $entry=
           new _A_($Graphics.$UI.$WXCore.$EvtHandler.$entryNEW121UNQ142,[$evtty,$f,$id,$lastId]);
          var $__=
           new _A_($UHC.$Base.$_3a,[$entry]);
          return new _A_($Data.$IORef.$modifyIORef,[$_24x,$__]);});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$dynamicEventTableEntryLastId=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.dynamicEventTableEntryLastId;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$dynamicEventTableEntryEventType=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.dynamicEventTableEntryEventType;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$dynamicEventTableEntryId=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.dynamicEventTableEntryId;});
$Graphics.$UI.$WXCore.$EvtHandler.$findHandlerUNQ126=
 new _F_(function($evtty,$id,$lid,$entry)
         {var $elid=
           new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$dynamicEventTableEntryLastId,[$entry]);
          var $eid=
           new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$dynamicEventTableEntryId,[$entry]);
          var $eevtty=
           new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$dynamicEventTableEntryEventType,[$entry]);
          var $__=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,$eevtty,$evtty]);
          var $__9=
           new _A_($Graphics.$UI.$WXCore.$Types.$wxID__ANY,[$UHC.$Base.$Num__DCT74__101__0]);
          var $__10=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,$lid,$__9]);
          var $__11=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,$elid,$lid]);
          var $__12=
           new _A_($UHC.$Base.$_7c_7c,[$__11,$__10]);
          var $__13=
           new _A_($UHC.$Base.$_26_26,[$__12,$__]);
          var $__14=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,$eid,$id]);
          return new _A_($UHC.$Base.$_26_26,[$__14,$__13]);});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$dynamicEventTableEntryFunc=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.dynamicEventTableEntryFunc;});
$Graphics.$UI.$WXCore.$EvtHandler.$maybeHeadUNQ120=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW505__0;
          switch($__._tag_)
           {case 0:
             var $__5=
              new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$dynamicEventTableEntryFunc,[$__._1]);
             var $__6=
              new _A_($UHC.$Base.$Just__,[$__5]);
             $__swJSW505__0=
              $__6;
             break;
            case 1:
             $__swJSW505__0=
              $UHC.$Base.$Nothing__;
             break;}
          return $__swJSW505__0;});
$Graphics.$UI.$WXCore.$EvtHandler.$__185__303__0UNQ116=
 new _F_(function($_24x,$evtty,$id,$lid)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$EvtHandler.$findHandlerUNQ126,[$evtty,$id,$lid]);
          var $__6=
           new _A_($UHC.$Base.$filter,[$__]);
          var $__7=
           new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$EvtHandler.$maybeHeadUNQ120,$__6]);
          var $__8=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
          var $__9=
           new _A_($UHC.$Base.$_2e,[$__8,$__7]);
          var $__10=
           new _A_($UHC.$IOBase.$readIORef,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__10,$__9]);});
$Graphics.$UI.$WXCore.$EvtHandler.$__187__39NEW10=
 new _F_(function($tail,$self,$_24x)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["_evtHandlerUnlink not implemented"]);
          var $__5=
           new _A_($UHC.$Base.$error,[$__]);
          var $__6=
           new _A_($UHC.$Base.$packedStringToString,["_evtHandlerTryThis not implemented"]);
          var $__7=
           new _A_($UHC.$Base.$error,[$__6]);
          var $__8=
           new _A_($UHC.$Base.$packedStringToString,["_evtHandlerTryBefore not implemented"]);
          var $__9=
           new _A_($UHC.$Base.$error,[$__8]);
          var $__10=
           new _A_($UHC.$Base.$packedStringToString,["_evtHandlerTryAfter not implemented"]);
          var $__11=
           new _A_($UHC.$Base.$error,[$__10]);
          var $__12=
           new _A_($UHC.$Base.$packedStringToString,["_evtHandlerSetPreviousHandler not implemented"]);
          var $__13=
           new _A_($UHC.$Base.$error,[$__12]);
          var $__14=
           new _A_($UHC.$Base.$packedStringToString,["_evtHandlerSetNextHandler not implemented"]);
          var $__15=
           new _A_($UHC.$Base.$error,[$__14]);
          var $__16=
           new _A_($UHC.$Base.$packedStringToString,["_evtHandlerSetEvtHandlerEnabled not implemented"]);
          var $__17=
           new _A_($UHC.$Base.$error,[$__16]);
          var $__18=
           new _A_($UHC.$Base.$packedStringToString,["_evtHandlerQueueEvent not implemented"]);
          var $__19=
           new _A_($UHC.$Base.$error,[$__18]);
          var $__20=
           new _A_($UHC.$Base.$packedStringToString,["_evtHandlerProcessPendingEvents not implemented"]);
          var $__21=
           new _A_($UHC.$Base.$error,[$__20]);
          var $__22=
           new _A_($UHC.$Base.$packedStringToString,["_evtHandlerIsUnlinked not implemented"]);
          var $__23=
           new _A_($UHC.$Base.$error,[$__22]);
          var $__24=
           new _A_($UHC.$Base.$packedStringToString,["_evtHandlerGetPreviousHandler not implemented"]);
          var $__25=
           new _A_($UHC.$Base.$error,[$__24]);
          var $__26=
           new _A_($UHC.$Base.$packedStringToString,["_evtHandlerGetNextHandler not implemented"]);
          var $__27=
           new _A_($UHC.$Base.$error,[$__26]);
          var $__28=
           new _A_($UHC.$Base.$packedStringToString,["_evtHandlerGetEvtHandlerEnabled not implemented"]);
          var $__29=
           new _A_($UHC.$Base.$error,[$__28]);
          var $__30=
           new _A_($UHC.$Base.$packedStringToString,["_evtHandlerDeletePendingEvents not implemented"]);
          var $__31=
           new _A_($UHC.$Base.$error,[$__30]);
          var $__32=
           new _A_($UHC.$Base.$packedStringToString,["_evtHandlerAddPendingEvent not implemented"]);
          var $__33=
           new _A_($UHC.$Base.$error,[$__32]);
          var $__34=
           new _A_($Graphics.$UI.$WXCore.$EvtHandler.$__185__295__0UNQ46,[$_24x]);
          var $__35=
           new _A_($Graphics.$UI.$WXCore.$EvtHandler.$__185__296__0UNQ88,[$self]);
          var $__36=
           new _A_($Graphics.$UI.$WXCore.$EvtHandler.$__185__302__0UNQ102,[$_24x]);
          var $__37=
           new _A_($Graphics.$UI.$WXCore.$EvtHandler.$__185__303__0UNQ116,[$_24x]);
          var $__38=
           new _A_($Graphics.$UI.$WXCore.$EvtHandler.$__185__304__0UNQ137,[$_24x]);
          return {_tag_:0,__evtHandlerAddPendingEvent:$__33,__evtHandlerBind:$__38,__evtGetHandler:$__37,__evtHandlerUnBind:$__36,__evtHandlerDeletePendingEvents:$__31,__evtHandlerGetEvtHandlerEnabled:$__29,__evtHandlerGetNextHandler:$__27,__evtHandlerGetPreviousHandler:$__25,__evtHandlerIsUnlinked:$__23,__evtHandlerProcessEvent:$__35,__evtHandlerProcessEventLocally:$__34,__evtHandlerProcessPendingEvents:$__21,__evtHandlerQueueEvent:$__19,__evtHandlerSetEvtHandlerEnabled:$__17,__evtHandlerSetNextHandler:$__15,__evtHandlerSetPreviousHandler:$__13,__evtHandlerTryAfter:$__11,__evtHandlerTryBefore:$__9,__evtHandlerTryThis:$__7,__evtHandlerUnlink:$__5,__evtHandlerTail:$tail};});
$Graphics.$UI.$WXCore.$EvtHandler.$_24okUNQ32=
 new _F_(function($tail,$self,$_24x)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$EvtHandler.$__187__39NEW10,[$tail,$self,$_24x]);
          return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__]);});
$Graphics.$UI.$WXCore.$EvtHandler.$evthandler_27UNQ21=
 new _F_(function($tail,$super,$self)
         {var $__=
           new _A_($UHC.$IOBase.$newIORef,[$UHC.$Base.$_5b_5d]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$EvtHandler.$_24okUNQ32,[$tail,$self]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__5]);});
$Graphics.$UI.$WXCore.$EvtHandler.$evthandler=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$extends,[$Graphics.$UI.$WXCore.$EvtHandler.$evthandler_27UNQ21,$LightOO.$object,$LightOO.$Core.$noOverride,$LightOO.$set__Object__Tail]);}),[]);
$Graphics.$UI.$WXCore.$Timer.$__193__233NEW91=
 new _F_(function($id)
         {var $__=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,$id,$Graphics.$UI.$WXCore.$Types.$idAny]);
          var $__3=
           _e_($__);
          var $__swJSW506__0;
          switch($__3._tag_)
           {case 0:
             var $__4=
              new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$id]);
             $__swJSW506__0=
              $__4;
             break;
            case 1:
             $__swJSW506__0=
              $Graphics.$UI.$WXCore.$Types.$idCreate;
             break;}
          return $__swJSW506__0;});
$Language.$UHC.$JS.$HTML5.$Window.$clearInterval=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__);
          var $__5=
           _e_($__2);
          var $__6=
           _e_($__4.clearInterval($__5));
          var $__7=
           _e_([]);
          return [$__3,$__7];});
$Graphics.$UI.$WXCore.$Timer.$_24okUNQ83=
 new _F_(function($_24x,$_24x2,$_24x3)
         {var $__=
           new _A_($UHC.$IOBase.$writeIORef,[$_24x,$UHC.$Base.$False__]);
          var $__5=
           new _A_($Language.$UHC.$JS.$HTML5.$Window.$clearInterval,[$_24x2,$_24x3]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__5,$__]);});
$Graphics.$UI.$WXCore.$Timer.$_24okUNQ77=
 new _F_(function($_24x,$_24x2,$_24x3)
         {var $__=
           new _A_($UHC.$IOBase.$readIORef,[$_24x2]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$Timer.$_24okUNQ83,[$_24x,$_24x3]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__5]);});
$Graphics.$UI.$WXCore.$Timer.$__191__487__0NEW28UNQ74=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Timer.$_24okUNQ77,[$_24x,$_24x2]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Language.$UHC.$JS.$HTML5.$Window.$window,$__]);});
$Graphics.$UI.$WXCore.$EventClass.$__eventSetEventObject=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__eventSetEventObject;});
$Graphics.$UI.$WXCore.$EventClass.$event__Methods=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$LightOO.$Core.$unRecord,$LightOO.$get__Object__Tail]);}),[]);
$Graphics.$UI.$WXCore.$EventClass.$eventSetEventObject=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$EventClass.$__eventSetEventObject,$Graphics.$UI.$WXCore.$EventClass.$event__Methods]);}),[]);
$Language.$UHC.$JS.$HTML5.$Window.$__setInterval=
 new _F_(function($__,$__2,$__3,$__4)
         {var $__5=
           _e_($__);
          var $__6=
           _e_($__2);
          var $__7=
           _e_($__3);
          var $__8=
           _e_($__5.setInterval($__6,$__7));
          return [$__4,$__8];});
$Language.$UHC.$JS.$HTML5.$Window.$_24okUNQ38=
 new _F_(function($w,$mils,$_24x)
         {return new _A_($Language.$UHC.$JS.$HTML5.$Window.$__setInterval,[$w,$_24x,$mils]);});
$Language.$UHC.$JS.$HTML5.$Window.$setInterval=
 new _F_(function($w,$f,$mils)
         {var $__=
           new _A_($Language.$UHC.$JS.$Prelude.$wrapFunc,[$f]);
          var $__5=
           new _A_($Language.$UHC.$JS.$HTML5.$Window.$_24okUNQ38,[$w,$mils]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__5]);});
$Graphics.$UI.$WXCore.$Timer.$_24okUNQ132=
 new _F_(function($_24x,$_24x2,$_24x3)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$UHC.$Base.$True__]);
          var $__5=
           new _A_($UHC.$IOBase.$writeIORef,[$_24x2,$_24x3]);
          var $__6=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__5,$__]);
          var $__7=
           new _A_($UHC.$IOBase.$writeIORef,[$_24x,$UHC.$Base.$True__]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__7,$__6]);});
$Graphics.$UI.$WXCore.$Timer.$_24okUNQ124=
 new _F_(function($_24x,$_24x2,$milli,$cb,$_24x5)
         {var $__=
           new _A_($Language.$UHC.$JS.$HTML5.$Window.$setInterval,[$_24x5,$cb,$milli]);
          var $__7=
           new _A_($Graphics.$UI.$WXCore.$Timer.$_24okUNQ132,[$_24x,$_24x2]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__7]);});
$Control.$Monad.$when=
 new _F_(function($__,$p,$s)
         {var $__4=
           _e_($p);
          var $__swJSW508__0;
          switch($__4._tag_)
           {case 0:
             var $__5=
              new _A_($UHC.$Base.$return,[$__,[]]);
             $__swJSW508__0=
              $__5;
             break;
            case 1:
             $__swJSW508__0=
              $s;
             break;}
          return $__swJSW508__0;});
$Graphics.$UI.$WXCore.$TimerClass.$__timerStop=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__timerStop;});
$Graphics.$UI.$WXCore.$TimerClass.$timerStop=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$TimerClass.$__timerStop,$Graphics.$UI.$WXCore.$TimerClass.$timer__Methods]);}),[]);
$Graphics.$UI.$WXCore.$EvtHandlerClass.$__evtHandlerProcessEvent=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__evtHandlerProcessEvent;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$evtHandlerProcessEvent=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$EvtHandlerClass.$__evtHandlerProcessEvent,$Graphics.$UI.$WXCore.$EvtHandlerClass.$evtHandler__Methods]);}),[]);
$Graphics.$UI.$WXCore.$Timer.$_24okUNQ115=
 new _F_(function($self,$_24x,$_24x3,$milli,$oneshot,$__,$_24x7,$_24x8)
         {var $__9=
           new _A_($LightOO.$Core.$_23,[$self,$Graphics.$UI.$WXCore.$TimerClass.$timerStop]);
          var $__10=
           new _A_($Control.$Monad.$when,[$UHC.$Base.$Monad__DCT74__339__0,$oneshot,$__9]);
          var $__11=
           new _A_($LightOO.$Core.$upcast,[$__,$_24x7]);
          var $__12=
           new _A_($LightOO.$Core.$_23,[$_24x8,$Graphics.$UI.$WXCore.$EvtHandlerClass.$evtHandlerProcessEvent]);
          var $__13=
           new _A_($UHC.$Base.$_24,[$__12,$__11]);
          var $cb=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__13,$__10]);
          var $__15=
           new _A_($Graphics.$UI.$WXCore.$Timer.$_24okUNQ124,[$_24x,$_24x3,$milli,$cb]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Language.$UHC.$JS.$HTML5.$Window.$window,$__15]);});
$Graphics.$UI.$WXCore.$Timer.$__193__135NEW51=
 new _F_(function($self,$_24x,$_24x3,$_24x4,$milli,$oneshot,$__,$_24x8)
         {var $__9=
           new _A_($UHC.$IOBase.$readIORef,[$_24x]);
          var $__10=
           new _A_($Graphics.$UI.$WXCore.$Timer.$_24okUNQ115,[$self,$_24x3,$_24x4,$milli,$oneshot,$__,$_24x8]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__9,$__10]);});
$Graphics.$UI.$WXCore.$Timer.$_24okUNQ109=
 new _F_(function($self,$_24x,$_24x3,$_24x4,$milli,$oneshot,$this,$__,$__9,$_24x10)
         {var $__11=
           new _A_($Graphics.$UI.$WXCore.$Timer.$__193__135NEW51,[$self,$_24x,$_24x3,$_24x4,$milli,$oneshot,$__,$_24x10]);
          var $__12=
           new _A_($LightOO.$Core.$upcast,[$__9,$this]);
          var $__13=
           new _A_($LightOO.$Core.$_23,[$_24x10,$Graphics.$UI.$WXCore.$EventClass.$eventSetEventObject]);
          var $__14=
           new _A_($UHC.$Base.$_24,[$__13,$__12]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__14,$__11]);});
$LightOO.$Core.$Sub__NEW15UNQ321EVLDCT69__9__0RDC=
 new _F_(function($Sub__)
         {var $Sub__2=
           _e_(new _A_($LightOO.$Core.$Sub__CLS69__3__0,[$Sub__]));
          var $__4=
           {_tag_:0,_1:$UHC.$Base.$id};
          return $__4;});
$LightOO.$Core.$Sub__NEW13UNQ320DCT69__9__0RDC=
 new _F_(function($Sub__)
         {var $Sub__2=
           new _A_($LightOO.$Core.$Sub__NEW15UNQ321EVLDCT69__9__0RDC,[$Sub__]);
          return $Sub__2;});
$LightOO.$Core.$Sub__UNQ320DCT69__9__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$Sub__NEW13UNQ320DCT69__9__0RDC,[$LightOO.$Core.$Sub__UNQ320DCT69__9__0RDC]);}),[]);
$LightOO.$Core.$Sub__DCT69__9__0=
 new _A_(new _F_(function()
                 {return $LightOO.$Core.$Sub__UNQ320DCT69__9__0RDC;}),[]);
$LightOO.$Core.$Sub__NEW119UNQ285EVLDCT69__17__0RDC=
 new _F_(function($Sub__,$Sub__2)
         {var $Sub__3=
           _e_(new _A_($LightOO.$Core.$Sub__CLS69__3__0,[$Sub__]));
          var $__5=
           {_tag_:0,_1:$Sub__2};
          return $__5;});
$LightOO.$Core.$Sub__NEW116UNQ282DCT69__17__0RDC=
 new _F_(function($Sub__,$Sub__2)
         {var $Sub__3=
           new _A_($LightOO.$Core.$Sub__NEW119UNQ285EVLDCT69__17__0RDC,[$Sub__,$Sub__2]);
          return $Sub__3;});
$LightOO.$Core.$Sub__DCT69__17__0=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($LightOO.$Core.$narrow,[$__2]);
          var $__4=
           new _A_($LightOO.$Core.$upcast,[$__]);
          var $Sub__DCT69__17__0DFLLightOO_2eCore_2eupcast=
           new _A_($UHC.$Base.$_2e,[$__4,$__3]);
          var $Sub__=
           _i_();
          _i_set_($Sub__,new _A_($LightOO.$Core.$Sub__NEW116UNQ282DCT69__17__0RDC,[$Sub__,$Sub__DCT69__17__0DFLLightOO_2eCore_2eupcast]));
          return $Sub__;});
$Graphics.$UI.$WXCore.$TimerClass.$__timerGetInterval=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__timerGetInterval;});
$Graphics.$UI.$WXCore.$TimerClass.$timerGetInterval=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$TimerClass.$__timerGetInterval,$Graphics.$UI.$WXCore.$TimerClass.$timer__Methods]);}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$__timerEventGetTimer=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__timerEventGetTimer;});
$Graphics.$UI.$WXCore.$TimerClass.$timerEvent__Methods=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$LightOO.$Core.$unRecord,$Graphics.$UI.$WXCore.$EventClass.$get__Event__Tail]);}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$timerEventGetTimer=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$TimerClass.$__timerEventGetTimer,$Graphics.$UI.$WXCore.$TimerClass.$timerEvent__Methods]);}),[]);
$Graphics.$UI.$WXCore.$Timer.$__193__16NEW2=
 new _F_(function($timer,$tail,$self)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$timer]);
          var $__5=
           new _A_($LightOO.$Core.$_23,[$self,$Graphics.$UI.$WXCore.$TimerClass.$timerEventGetTimer]);
          var $__6=
           new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__5,$Graphics.$UI.$WXCore.$TimerClass.$timerGetInterval]);
          return {_tag_:0,__timerEventGetInterval:$__6,__timerEventGetTimer:$__,__timerEventTail:$tail};});
$Graphics.$UI.$WXCore.$Timer.$timerEvent_27UNQ8=
 new _F_(function($timer,$tail,$super,$__,$self)
         {var $__6=
           new _A_($Graphics.$UI.$WXCore.$Timer.$__193__16NEW2,[$timer,$tail,$self]);
          return new _A_($UHC.$Base.$return,[$__,$__6]);});
$Graphics.$UI.$WXCore.$Timer.$__193__32__0=
 new _F_(function($timer,$__,$__3)
         {return new _A_($Graphics.$UI.$WXCore.$Timer.$timerEvent_27UNQ8,[$timer,$__,$__3,$UHC.$Base.$Monad__DCT74__339__0]);});
$UHC.$Base.$Just__=
 new _F_(function($x1)
         {return {_tag_:0,_1:$x1};});
$Graphics.$UI.$WXCore.$Event.$__71__118__0UNQ22=
 new _F_(function($__)
         {return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);});
$Graphics.$UI.$WXCore.$Event.$__73__17NEW3=
 new _F_(function($id,$eventType,$tail,$_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
          var $__6=
           new _A_($UHC.$IOBase.$writeIORef,[$_24x]);
          var $__7=
           new _A_($UHC.$Base.$_2e,[$__6,$UHC.$Base.$Just__]);
          var $__8=
           new _A_($UHC.$IOBase.$readIORef,[$_24x]);
          var $__9=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$UHC.$Base.$False__]);
          var $__10=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$id]);
          var $__11=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$eventType]);
          return {_tag_:0,__eventGetType:$__11,__eventGetId:$__10,__eventGetSkipped:$__9,__eventGetEventObject:$__8,__eventSetEventObject:$__7,__eventSkip:$Graphics.$UI.$WXCore.$Event.$__71__118__0UNQ22,__eventStopPropagation:$__,__eventTail:$tail};});
$Graphics.$UI.$WXCore.$Event.$_24okUNQ16=
 new _F_(function($id,$eventType,$tail,$_24x)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Event.$__73__17NEW3,[$id,$eventType,$tail,$_24x]);
          return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__]);});
$Graphics.$UI.$WXCore.$Event.$event_27UNQ7=
 new _F_(function($id,$eventType,$tail,$super,$self)
         {var $__=
           new _A_($UHC.$IOBase.$newIORef,[$UHC.$Base.$Nothing__]);
          var $__7=
           new _A_($Graphics.$UI.$WXCore.$Event.$_24okUNQ16,[$id,$eventType,$tail]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__7]);});
$LightOO.$__157__185NEW83=
 new _F_(function($tail,$_24x)
         {var $__=
           new _A_($UHC.$IOBase.$readIORef,[$_24x]);
          var $__4=
           new _A_($UHC.$IOBase.$writeIORef,[$_24x]);
          return {_tag_:0,objectGetFlag:$__,objectSetFlag:$__4,__objectTail:$tail};});
$LightOO.$_24okUNQ194=
 new _F_(function($tail,$_24x)
         {var $__=
           new _A_($LightOO.$__157__185NEW83,[$tail,$_24x]);
          return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__]);});
$LightOO.$__157__176__0=
 new _F_(function($tail,$self)
         {var $__=
           new _A_($UHC.$IOBase.$newIORef,[$UHC.$Base.$False__]);
          var $__4=
           new _A_($LightOO.$_24okUNQ194,[$tail]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$LightOO.$object=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_24,[$LightOO.$Core.$clazz,$LightOO.$__157__176__0]);}),[]);
$Graphics.$UI.$WXCore.$Event.$event=
 new _F_(function($id,$eventType)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Event.$event_27UNQ7,[$id,$eventType]);
          return new _A_($LightOO.$Core.$extends,[$__,$LightOO.$object,$LightOO.$Core.$noOverride,$LightOO.$set__Object__Tail]);});
$Graphics.$UI.$WXCore.$Timer.$timerEvent=
 new _F_(function($id,$timer)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Event.$event,[$id,$Graphics.$UI.$WXCore.$Types.$wxEVT__TIMER]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$Timer.$__193__32__0,[$timer]);
          return new _A_($LightOO.$Core.$extends,[$__4,$__,$LightOO.$Core.$noOverride,$Graphics.$UI.$WXCore.$EventClass.$set__Event__Tail]);});
$Control.$Monad.$Fix.$mfix=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$LightOO.$Core.$new=
 new _F_(function($__,$c)
         {var $__3=
           new _A_($c,[$LightOO.$Core.$emptyRecord]);
          var $__4=
           new _A_($Control.$Monad.$Fix.$mfix,[$__]);
          return new _A_($UHC.$Base.$_24,[$__4,$__3]);});
$Control.$Monad.$Fix.$MonadFix__CLS97__0__0=
 new _F_(function($MonadFix__)
         {var $MonadFix__2=
           {_tag_:0,_1:$UHC.$Base.$undefined,_2:$UHC.$Base.$undefined};
          return $MonadFix__2;});
$UHC.$Base.$NonTermination__=
 new _A_(new _F_(function()
                 {return {_tag_:10};}),[]);
$System.$IO.$Fix.$_24okUNQ19=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$_24x2]);
          var $__4=
           new _A_($UHC.$IOBase.$writeIORef,[$_24x,$_24x2]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__]);});
$System.$IO.$Fix.$_24okUNQ12=
 new _F_(function($k,$_24x,$_24x3)
         {var $__=
           new _A_($k,[$_24x3]);
          var $__5=
           new _A_($System.$IO.$Fix.$_24okUNQ19,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__5]);});
$System.$IO.$Unsafe.$unsafeInterleaveIO=
 new _F_(function($f)
         {var $__=
           new _A_($UHC.$IOBase.$unsafePerformIO,[$f]);
          return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__]);});
$System.$IO.$Fix.$_24okUNQ6=
 new _F_(function($k,$_24x)
         {var $__=
           new _A_($UHC.$IOBase.$readIORef,[$_24x]);
          var $__4=
           new _A_($System.$IO.$Unsafe.$unsafeInterleaveIO,[$__]);
          var $__5=
           new _A_($System.$IO.$Fix.$_24okUNQ12,[$k,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__5]);});
$System.$IO.$Fix.$fixIO=
 new _F_(function($k)
         {var $__=
           new _A_($UHC.$Base.$throw,[$UHC.$Base.$NonTermination__]);
          var $__3=
           new _A_($UHC.$IOBase.$newIORef,[$__]);
          var $__4=
           new _A_($System.$IO.$Fix.$_24okUNQ6,[$k]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__3,$__4]);});
$Control.$Monad.$Fix.$MonadFix__NEW44UNQ19EVLDCT97__4__0RDC=
 new _F_(function($MonadFix__)
         {var $MonadFix__2=
           _e_(new _A_($Control.$Monad.$Fix.$MonadFix__CLS97__0__0,[$MonadFix__]));
          var $__5=
           {_tag_:0,_1:$System.$IO.$Fix.$fixIO,_2:$UHC.$Base.$Monad__DCT74__339__0};
          return $__5;});
$Control.$Monad.$Fix.$MonadFix__NEW42UNQ18DCT97__4__0RDC=
 new _F_(function($MonadFix__)
         {var $MonadFix__2=
           new _A_($Control.$Monad.$Fix.$MonadFix__NEW44UNQ19EVLDCT97__4__0RDC,[$MonadFix__]);
          return $MonadFix__2;});
$Control.$Monad.$Fix.$MonadFix__UNQ18DCT97__4__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Control.$Monad.$Fix.$MonadFix__NEW42UNQ18DCT97__4__0RDC,[$Control.$Monad.$Fix.$MonadFix__UNQ18DCT97__4__0RDC]);}),[]);
$Control.$Monad.$Fix.$MonadFix__DCT97__4__0=
 new _A_(new _F_(function()
                 {return $Control.$Monad.$Fix.$MonadFix__UNQ18DCT97__4__0RDC;}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$Narrow__NEW198UNQ578EVLDCT177__14__0RDC=
 new _F_(function($Narrow__,$Narrow__2)
         {var $Narrow__3=
           _e_(new _A_($LightOO.$Core.$Narrow__CLS69__5__0,[$Narrow__2]));
          var $__5=
           {_tag_:0,_1:$Narrow__};
          return $__5;});
$Graphics.$UI.$WXCore.$TimerClass.$Narrow__NEW195UNQ575DCT177__14__0RDC=
 new _F_(function($Narrow__,$Narrow__2)
         {var $Narrow__3=
           new _A_($Graphics.$UI.$WXCore.$TimerClass.$Narrow__NEW198UNQ578EVLDCT177__14__0RDC,[$Narrow__,$Narrow__2]);
          return $Narrow__3;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$__157__917__0=
 new _F_(function($v,$o)
         {var $__=
           new _A_($LightOO.$Core.$unRecord,[$o]);
          var $__4=
           new _A_($LightOO.$Core.$setTail,[$Graphics.$UI.$WXCore.$EvtHandlerClass.$ModTail__DCT153__3__0,$__,$v]);
          return new _A_($UHC.$Base.$_24,[$LightOO.$Core.$record,$__4]);});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$set__EvtHandler__Tail=
 new _F_(function($o,$v)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$__157__917__0,[$v]);
          return new _A_($LightOO.$modify__Object__Tail,[$__,$o]);});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$modify__EvtHandler__Tail=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$mkMod,[$Graphics.$UI.$WXCore.$EvtHandlerClass.$set__EvtHandler__Tail,$Graphics.$UI.$WXCore.$EvtHandlerClass.$get__EvtHandler__Tail]);}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$Typeable__NEW75UNQ555EVLDCT177__23__0RDC=
 new _F_(function($Typeable__,$Typeable__2)
         {var $Typeable__3=
           _e_(new _A_($Data.$Typeable.$Typeable__CLS320__7__0,[$Typeable__]));
          var $__5=
           {_tag_:0,_1:$Typeable__2};
          return $__5;});
$Graphics.$UI.$WXCore.$TimerClass.$Typeable__NEW72UNQ552DCT177__23__0RDC=
 new _F_(function($Typeable__,$Typeable__2)
         {var $Typeable__3=
           new _A_($Graphics.$UI.$WXCore.$TimerClass.$Typeable__NEW75UNQ555EVLDCT177__23__0RDC,[$Typeable__,$Typeable__2]);
          return $Typeable__3;});
$Graphics.$UI.$WXCore.$TimerClass.$__181__122=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["Timer"]);}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$timerTc=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Typeable.$mkTyCon,[$Graphics.$UI.$WXCore.$TimerClass.$__181__122]);}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$Typeable1__DCT177__22__0DFLData_2eTypeable_2etypeOf1=
 new _F_(function($__)
         {return new _A_($Data.$Typeable.$mkTyConApp,[$Graphics.$UI.$WXCore.$TimerClass.$timerTc,$UHC.$Base.$_5b_5d]);});
$Graphics.$UI.$WXCore.$TimerClass.$Typeable1__NEW66UNQ571EVLDCT177__22__0RDC=
 new _F_(function($Typeable1__)
         {var $Typeable1__2=
           _e_(new _A_($Data.$Typeable.$Typeable1__CLS320__8__0,[$Typeable1__]));
          var $__4=
           {_tag_:0,_1:$Graphics.$UI.$WXCore.$TimerClass.$Typeable1__DCT177__22__0DFLData_2eTypeable_2etypeOf1};
          return $__4;});
$Graphics.$UI.$WXCore.$TimerClass.$Typeable1__NEW64UNQ570DCT177__22__0RDC=
 new _F_(function($Typeable1__)
         {var $Typeable1__2=
           new _A_($Graphics.$UI.$WXCore.$TimerClass.$Typeable1__NEW66UNQ571EVLDCT177__22__0RDC,[$Typeable1__]);
          return $Typeable1__2;});
$Graphics.$UI.$WXCore.$TimerClass.$Typeable1__UNQ570DCT177__22__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$TimerClass.$Typeable1__NEW64UNQ570DCT177__22__0RDC,[$Graphics.$UI.$WXCore.$TimerClass.$Typeable1__UNQ570DCT177__22__0RDC]);}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$Typeable1__DCT177__22__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$TimerClass.$Typeable1__UNQ570DCT177__22__0RDC;}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$Typeable__DCT177__23__0=
 new _F_(function($__)
         {var $Typeable__DCT177__23__0DFLData_2eTypeable_2etypeOf=
           new _A_($Data.$Typeable.$typeOfDefault,[$Graphics.$UI.$WXCore.$TimerClass.$Typeable1__DCT177__22__0,$__]);
          var $Typeable__=
           _i_();
          _i_set_($Typeable__,new _A_($Graphics.$UI.$WXCore.$TimerClass.$Typeable__NEW72UNQ552DCT177__23__0RDC,[$Typeable__,$Typeable__DCT177__23__0DFLData_2eTypeable_2etypeOf]));
          return $Typeable__;});
$Graphics.$UI.$WXCore.$TimerClass.$__179__1897__1__0UNQ576=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$TimerClass.$Typeable__DCT177__23__0,[$Data.$Typeable.$Typeable__DCT320__26__0]);}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$__181__348=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$hideRecord,[$Graphics.$UI.$WXCore.$TimerClass.$__179__1897__1__0UNQ576]);}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$Narrow__DCT177__14__0DFLLightOO_2eCore_2enarrow=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$modify__EvtHandler__Tail,[$Graphics.$UI.$WXCore.$TimerClass.$__181__348]);}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$Narrow__UNQ575DCT177__14__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$TimerClass.$Narrow__NEW195UNQ575DCT177__14__0RDC,[$Graphics.$UI.$WXCore.$TimerClass.$Narrow__DCT177__14__0DFLLightOO_2eCore_2enarrow,$Graphics.$UI.$WXCore.$TimerClass.$Narrow__UNQ575DCT177__14__0RDC]);}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$Narrow__DCT177__14__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$TimerClass.$Narrow__UNQ575DCT177__14__0RDC;}),[]);
$Graphics.$UI.$WXCore.$EvtHandlerClass.$Typeable__NEW450UNQ1239EVLDCT153__9__0RDC=
 new _F_(function($Typeable__,$Typeable__2)
         {var $Typeable__3=
           _e_(new _A_($Data.$Typeable.$Typeable__CLS320__7__0,[$Typeable__2]));
          var $__5=
           {_tag_:0,_1:$Typeable__};
          return $__5;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$Typeable__NEW447UNQ1236DCT153__9__0RDC=
 new _F_(function($Typeable__,$Typeable__2)
         {var $Typeable__3=
           new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$Typeable__NEW450UNQ1239EVLDCT153__9__0RDC,[$Typeable__,$Typeable__2]);
          return $Typeable__3;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$__157__350=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["EvtHandler"]);}),[]);
$Graphics.$UI.$WXCore.$EvtHandlerClass.$evtHandlerTc=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Typeable.$mkTyCon,[$Graphics.$UI.$WXCore.$EvtHandlerClass.$__157__350]);}),[]);
$Graphics.$UI.$WXCore.$EvtHandlerClass.$Typeable1__DCT153__8__0DFLData_2eTypeable_2etypeOf1=
 new _F_(function($__)
         {return new _A_($Data.$Typeable.$mkTyConApp,[$Graphics.$UI.$WXCore.$EvtHandlerClass.$evtHandlerTc,$UHC.$Base.$_5b_5d]);});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$Typeable1__NEW441UNQ1231EVLDCT153__8__0RDC=
 new _F_(function($Typeable1__)
         {var $Typeable1__2=
           _e_(new _A_($Data.$Typeable.$Typeable1__CLS320__8__0,[$Typeable1__]));
          var $__4=
           {_tag_:0,_1:$Graphics.$UI.$WXCore.$EvtHandlerClass.$Typeable1__DCT153__8__0DFLData_2eTypeable_2etypeOf1};
          return $__4;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$Typeable1__NEW439UNQ1230DCT153__8__0RDC=
 new _F_(function($Typeable1__)
         {var $Typeable1__2=
           new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$Typeable1__NEW441UNQ1231EVLDCT153__8__0RDC,[$Typeable1__]);
          return $Typeable1__2;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$Typeable1__UNQ1230DCT153__8__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$Typeable1__NEW439UNQ1230DCT153__8__0RDC,[$Graphics.$UI.$WXCore.$EvtHandlerClass.$Typeable1__UNQ1230DCT153__8__0RDC]);}),[]);
$Graphics.$UI.$WXCore.$EvtHandlerClass.$Typeable1__DCT153__8__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$EvtHandlerClass.$Typeable1__UNQ1230DCT153__8__0RDC;}),[]);
$Graphics.$UI.$WXCore.$EvtHandlerClass.$Typeable__DCT153__9__0=
 new _F_(function($__)
         {var $Typeable__DCT153__9__0DFLData_2eTypeable_2etypeOf=
           new _A_($Data.$Typeable.$typeOfDefault,[$Graphics.$UI.$WXCore.$EvtHandlerClass.$Typeable1__DCT153__8__0,$__]);
          var $Typeable__=
           _i_();
          _i_set_($Typeable__,new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$Typeable__NEW447UNQ1236DCT153__9__0RDC,[$Typeable__DCT153__9__0DFLData_2eTypeable_2etypeOf,$Typeable__]));
          return $Typeable__;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$__155__1142__1__0UNQ1243=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$Typeable__DCT153__9__0,[$Data.$Typeable.$Typeable__DCT320__26__0]);}),[]);
$Graphics.$UI.$WXCore.$EvtHandlerClass.$__157__1036=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$hideRecord,[$Graphics.$UI.$WXCore.$EvtHandlerClass.$__155__1142__1__0UNQ1243]);}),[]);
$Graphics.$UI.$WXCore.$EvtHandlerClass.$Narrow__DCT153__0__0DFLLightOO_2eCore_2enarrow=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$modify__Object__Tail,[$Graphics.$UI.$WXCore.$EvtHandlerClass.$__157__1036]);}),[]);
$Graphics.$UI.$WXCore.$EvtHandlerClass.$Narrow__NEW570UNQ1245EVLDCT153__0__0RDC=
 new _F_(function($Narrow__,$Narrow__2)
         {var $Narrow__3=
           _e_(new _A_($LightOO.$Core.$Narrow__CLS69__5__0,[$Narrow__]));
          var $__5=
           {_tag_:0,_1:$Narrow__2};
          return $__5;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$Narrow__NEW567UNQ1242DCT153__0__0RDC=
 new _F_(function($Narrow__,$Narrow__2)
         {var $Narrow__3=
           new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$Narrow__NEW570UNQ1245EVLDCT153__0__0RDC,[$Narrow__,$Narrow__2]);
          return $Narrow__3;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$Narrow__UNQ1242DCT153__0__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$Narrow__NEW567UNQ1242DCT153__0__0RDC,[$Graphics.$UI.$WXCore.$EvtHandlerClass.$Narrow__UNQ1242DCT153__0__0RDC,$Graphics.$UI.$WXCore.$EvtHandlerClass.$Narrow__DCT153__0__0DFLLightOO_2eCore_2enarrow]);}),[]);
$Graphics.$UI.$WXCore.$EvtHandlerClass.$Narrow__DCT153__0__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$EvtHandlerClass.$Narrow__UNQ1242DCT153__0__0RDC;}),[]);
$Graphics.$UI.$WXCore.$Timer.$_24okUNQ97=
 new _F_(function($self,$_24x,$_24x3,$_24x4,$milli,$oneshot,$this,$__,$_24x9)
         {var $__10=
           new _A_($LightOO.$Core.$Sub__DCT69__17__0,[$LightOO.$Core.$Sub__DCT69__9__0,$Graphics.$UI.$WXCore.$EvtHandlerClass.$Narrow__DCT153__0__0]);
          var $__11=
           new _A_($LightOO.$Core.$Sub__DCT69__24__0,[$__10,$Graphics.$UI.$WXCore.$TimerClass.$Narrow__DCT177__14__0]);
          var $__12=
           new _A_($Graphics.$UI.$WXCore.$Timer.$timerEvent,[$_24x9,$this]);
          var $__13=
           new _A_($LightOO.$Core.$new,[$Control.$Monad.$Fix.$MonadFix__DCT97__4__0]);
          var $__14=
           new _A_($UHC.$Base.$_24,[$__13,$__12]);
          var $__15=
           new _A_($Graphics.$UI.$WXCore.$Timer.$_24okUNQ109,[$self,$_24x,$_24x3,$_24x4,$milli,$oneshot,$this,$__,$__11]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__14,$__15]);});
$LightOO.$Core.$upcast=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$LightOO.$Core.$Sub__NEW143UNQ301EVLDCT69__24__0RDC=
 new _F_(function($Sub__,$Sub__2)
         {var $Sub__3=
           _e_(new _A_($LightOO.$Core.$Sub__CLS69__3__0,[$Sub__]));
          var $__5=
           {_tag_:0,_1:$Sub__2};
          return $__5;});
$LightOO.$Core.$Sub__NEW140UNQ298DCT69__24__0RDC=
 new _F_(function($Sub__,$Sub__2)
         {var $Sub__3=
           new _A_($LightOO.$Core.$Sub__NEW143UNQ301EVLDCT69__24__0RDC,[$Sub__,$Sub__2]);
          return $Sub__3;});
$LightOO.$Core.$narrow=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$LightOO.$Core.$Sub__DCT69__24__0=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($LightOO.$Core.$narrow,[$__2]);
          var $__4=
           new _A_($LightOO.$Core.$upcast,[$__]);
          var $Sub__DCT69__24__0DFLLightOO_2eCore_2eupcast=
           new _A_($UHC.$Base.$_2e,[$__4,$__3]);
          var $Sub__=
           _i_();
          _i_set_($Sub__,new _A_($LightOO.$Core.$Sub__NEW140UNQ298DCT69__24__0RDC,[$Sub__,$Sub__DCT69__24__0DFLLightOO_2eCore_2eupcast]));
          return $Sub__;});
$LightOO.$Core.$Sub__CLS69__3__0=
 new _F_(function($Sub__)
         {var $Sub__2=
           {_tag_:0,_1:$UHC.$Base.$undefined};
          return $Sub__2;});
$LightOO.$Core.$Sub__NEW94UNQ261EVLDCT69__13__0RDC=
 new _F_(function($Sub__)
         {var $Sub__2=
           _e_(new _A_($LightOO.$Core.$Sub__CLS69__3__0,[$Sub__]));
          var $__4=
           {_tag_:0,_1:$UHC.$Base.$id};
          return $__4;});
$LightOO.$Core.$Sub__NEW92UNQ260DCT69__13__0RDC=
 new _F_(function($Sub__)
         {var $Sub__2=
           new _A_($LightOO.$Core.$Sub__NEW94UNQ261EVLDCT69__13__0RDC,[$Sub__]);
          return $Sub__2;});
$LightOO.$Core.$Sub__UNQ260DCT69__13__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$Sub__NEW92UNQ260DCT69__13__0RDC,[$LightOO.$Core.$Sub__UNQ260DCT69__13__0RDC]);}),[]);
$LightOO.$Core.$Sub__DCT69__13__0=
 new _A_(new _F_(function()
                 {return $LightOO.$Core.$Sub__UNQ260DCT69__13__0RDC;}),[]);
$Graphics.$UI.$WXCore.$EventClass.$__61__439__0=
 new _F_(function($v,$o)
         {var $__=
           new _A_($LightOO.$Core.$unRecord,[$o]);
          var $__4=
           new _A_($LightOO.$Core.$setTail,[$Graphics.$UI.$WXCore.$EventClass.$ModTail__DCT57__3__0,$__,$v]);
          return new _A_($UHC.$Base.$_24,[$LightOO.$Core.$record,$__4]);});
$LightOO.$set__Object__Tail=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$setTail,[$LightOO.$ModTail__DCT153__3__0]);}),[]);
$LightOO.$modify__Object__Tail=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$mkMod,[$LightOO.$set__Object__Tail,$LightOO.$get__Object__Tail]);}),[]);
$Graphics.$UI.$WXCore.$EventClass.$set__Event__Tail=
 new _F_(function($o,$v)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$EventClass.$__61__439__0,[$v]);
          return new _A_($LightOO.$modify__Object__Tail,[$__,$o]);});
$Graphics.$UI.$WXCore.$EventClass.$__61__119=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$LightOO.$Core.$unRecord,$LightOO.$get__Object__Tail]);}),[]);
$Graphics.$UI.$WXCore.$EventClass.$__eventTail=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__eventTail;});
$Graphics.$UI.$WXCore.$EventClass.$ModTail__DCT57__3__0DFLLightOO_2eCore_2esetTail=
 new _F_(function($o,$v)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["has no field update '[Graphics.UI.WXCore.EventClass._eventTail]'"]);
          var $__4=
           new _A_($UHC.$Base.$error,[$__]);
          var $__5=
           _e_($o);
          var $__14=
           {_tag_:0,__eventGetType:$__5.__eventGetType,__eventGetId:$__5.__eventGetId,__eventGetSkipped:$__5.__eventGetSkipped,__eventGetEventObject:$__5.__eventGetEventObject,__eventSetEventObject:$__5.__eventSetEventObject,__eventSkip:$__5.__eventSkip,__eventStopPropagation:$__5.__eventStopPropagation,__eventTail:$v};
          return $__14;});
$Graphics.$UI.$WXCore.$EventClass.$ModTail__NEW56UNQ353EVLDCT57__3__0RDC=
 new _F_(function($ModTail__)
         {var $ModTail__2=
           _e_(new _A_($LightOO.$Core.$ModTail__CLS69__7__0,[$ModTail__]));
          var $__6=
           {_tag_:0,_1:$Graphics.$UI.$WXCore.$EventClass.$__eventTail,_2:$ModTail__2._2,_3:$Graphics.$UI.$WXCore.$EventClass.$ModTail__DCT57__3__0DFLLightOO_2eCore_2esetTail};
          return $__6;});
$Graphics.$UI.$WXCore.$EventClass.$ModTail__NEW54UNQ352DCT57__3__0RDC=
 new _F_(function($ModTail__)
         {var $ModTail__2=
           new _A_($Graphics.$UI.$WXCore.$EventClass.$ModTail__NEW56UNQ353EVLDCT57__3__0RDC,[$ModTail__]);
          return $ModTail__2;});
$Graphics.$UI.$WXCore.$EventClass.$ModTail__UNQ352DCT57__3__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$EventClass.$ModTail__NEW54UNQ352DCT57__3__0RDC,[$Graphics.$UI.$WXCore.$EventClass.$ModTail__UNQ352DCT57__3__0RDC]);}),[]);
$Graphics.$UI.$WXCore.$EventClass.$ModTail__DCT57__3__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$EventClass.$ModTail__UNQ352DCT57__3__0RDC;}),[]);
$Graphics.$UI.$WXCore.$EventClass.$__61__118=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$getTail,[$Graphics.$UI.$WXCore.$EventClass.$ModTail__DCT57__3__0]);}),[]);
$Graphics.$UI.$WXCore.$EventClass.$get__Event__Tail=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$EventClass.$__61__118,$Graphics.$UI.$WXCore.$EventClass.$__61__119]);}),[]);
$Graphics.$UI.$WXCore.$EventClass.$modify__Event__Tail=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$mkMod,[$Graphics.$UI.$WXCore.$EventClass.$set__Event__Tail,$Graphics.$UI.$WXCore.$EventClass.$get__Event__Tail]);}),[]);
$Data.$Typeable.$Typeable1__CLS320__8__0=
 new _F_(function($Typeable1__)
         {var $Typeable1__2=
           {_tag_:0,_1:$UHC.$Base.$undefined};
          return $Typeable1__2;});
$Graphics.$UI.$WXCore.$TimerClass.$__181__488=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["TimerEvent"]);}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$timerEventTc=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Typeable.$mkTyCon,[$Graphics.$UI.$WXCore.$TimerClass.$__181__488]);}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$Typeable1__DCT177__8__0DFLData_2eTypeable_2etypeOf1=
 new _F_(function($__)
         {return new _A_($Data.$Typeable.$mkTyConApp,[$Graphics.$UI.$WXCore.$TimerClass.$timerEventTc,$UHC.$Base.$_5b_5d]);});
$Graphics.$UI.$WXCore.$TimerClass.$Typeable1__NEW273UNQ566EVLDCT177__8__0RDC=
 new _F_(function($Typeable1__)
         {var $Typeable1__2=
           _e_(new _A_($Data.$Typeable.$Typeable1__CLS320__8__0,[$Typeable1__]));
          var $__4=
           {_tag_:0,_1:$Graphics.$UI.$WXCore.$TimerClass.$Typeable1__DCT177__8__0DFLData_2eTypeable_2etypeOf1};
          return $__4;});
$Graphics.$UI.$WXCore.$TimerClass.$Typeable1__NEW271UNQ565DCT177__8__0RDC=
 new _F_(function($Typeable1__)
         {var $Typeable1__2=
           new _A_($Graphics.$UI.$WXCore.$TimerClass.$Typeable1__NEW273UNQ566EVLDCT177__8__0RDC,[$Typeable1__]);
          return $Typeable1__2;});
$Graphics.$UI.$WXCore.$TimerClass.$Typeable1__UNQ565DCT177__8__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$TimerClass.$Typeable1__NEW271UNQ565DCT177__8__0RDC,[$Graphics.$UI.$WXCore.$TimerClass.$Typeable1__UNQ565DCT177__8__0RDC]);}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$Typeable1__DCT177__8__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$TimerClass.$Typeable1__UNQ565DCT177__8__0RDC;}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$Typeable__NEW282UNQ562EVLDCT177__9__0RDC=
 new _F_(function($Typeable__,$Typeable__2)
         {var $Typeable__3=
           _e_(new _A_($Data.$Typeable.$Typeable__CLS320__7__0,[$Typeable__]));
          var $__5=
           {_tag_:0,_1:$Typeable__2};
          return $__5;});
$Graphics.$UI.$WXCore.$TimerClass.$Typeable__NEW279UNQ559DCT177__9__0RDC=
 new _F_(function($Typeable__,$Typeable__2)
         {var $Typeable__3=
           new _A_($Graphics.$UI.$WXCore.$TimerClass.$Typeable__NEW282UNQ562EVLDCT177__9__0RDC,[$Typeable__,$Typeable__2]);
          return $Typeable__3;});
$Data.$Typeable.$typeOf1=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$Data.$Typeable.$arg__kNEW346UNQ400=
 new _F_(function($arg__tr)
         {var $__=
           _e_($arg__tr);
          return $__._1;});
$Data.$Typeable.$__324__704__0=
 new _F_(function($tc,$tr__k,$trs,$arg__tr)
         {var $arg__k=
           new _A_($Data.$Typeable.$arg__kNEW346UNQ400,[$arg__tr]);
          var $__=
           new _A_($UHC.$Base.$_3a,[$arg__tr,$UHC.$Base.$_5b_5d]);
          var $__7=
           new _A_($UHC.$Base.$_2b_2b,[$trs,$__]);
          var $__8=
           new _A_($Data.$Typeable.$appKey,[$tr__k,$arg__k]);
          return new _A_($Data.$Typeable.$TypeRep__,[$__8,$tc,$__7]);});
$Data.$Typeable.$mkAppTy=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return new _A_($Data.$Typeable.$__324__704__0,[$__2._2,$__2._1,$__2._3]);});
$Data.$Typeable.$typeOfDefault=
 new _F_(function($__,$__2,$x)
         {var $__4=
           new _A_($UHC.$Base.$undefined,[$x]);
          var $__5=
           new _A_($Data.$Typeable.$typeOf,[$__2,$__4]);
          var $__6=
           new _A_($Data.$Typeable.$typeOf1,[$__,$x]);
          return new _A_($Data.$Typeable.$mkAppTy,[$__6,$__5]);});
$Graphics.$UI.$WXCore.$TimerClass.$Typeable__DCT177__9__0=
 new _F_(function($__)
         {var $Typeable__DCT177__9__0DFLData_2eTypeable_2etypeOf=
           new _A_($Data.$Typeable.$typeOfDefault,[$Graphics.$UI.$WXCore.$TimerClass.$Typeable1__DCT177__8__0,$__]);
          var $Typeable__=
           _i_();
          _i_set_($Typeable__,new _A_($Graphics.$UI.$WXCore.$TimerClass.$Typeable__NEW279UNQ559DCT177__9__0RDC,[$Typeable__,$Typeable__DCT177__9__0DFLData_2eTypeable_2etypeOf]));
          return $Typeable__;});
$Data.$Typeable.$typeOf0_27=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$UHC.$Base.$asTypeOf=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$const;}),[]);
$Data.$Typeable.$typeOf0default=
 new _F_(function($__,$__2,$rep,$x)
         {var $__5=
           new _A_($UHC.$Base.$from0,[$__,$x]);
          var $__6=
           new _A_($UHC.$Base.$asTypeOf,[$__5,$rep]);
          return new _A_($Data.$Typeable.$typeOf0_27,[$__2,$__6]);});
$UHC.$Base.$foldl=
 new _F_(function($x1,$x2,$x3)
         {var $x34=
           _e_($x3);
          var $__swJSW536__0;
          switch($x34._tag_)
           {case 0:
             var $__=
              new _A_($x1,[$x2,$x34._1]);
             var $__8=
              new _A_($UHC.$Base.$foldl,[$x1,$__,$x34._2]);
             $__swJSW536__0=
              $__8;
             break;
            case 1:
             $__swJSW536__0=
              $x2;
             break;}
          return $__swJSW536__0;});
$Data.$Typeable.$appKey=
 new _F_(function($k1,$k2)
         {return 0;});
$Data.$Typeable.$appKeys=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$foldl,[$Data.$Typeable.$appKey]);}),[]);
$Data.$Typeable.$_24okUNQ373=
 new _F_(function($_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           new _A_($UHC.$Base.$_3a,[$__._1,$UHC.$Base.$_5b_5d]);
          return $__6;});
$Data.$Typeable.$arg__ksNEW256UNQ372=
 new _F_(function($args)
         {return new _A_($UHC.$Base.$concatMap,[$Data.$Typeable.$_24okUNQ373,$args]);});
$Data.$Typeable.$TypeRep__=
 new _F_(function($x1,$x2,$x3)
         {return {_tag_:0,_1:$x1,_2:$x2,_3:$x3};});
$Data.$Typeable.$__324__555__0=
 new _F_(function($tc,$tc__k,$args)
         {var $arg__ks=
           new _A_($Data.$Typeable.$arg__ksNEW256UNQ372,[$args]);
          var $__=
           new _A_($Data.$Typeable.$appKeys,[$tc__k,$arg__ks]);
          return new _A_($Data.$Typeable.$TypeRep__,[$__,$tc,$args]);});
$Data.$Typeable.$mkTyConApp=
 new _F_(function($tc)
         {var $tc2=
           _e_($tc);
          return new _A_($Data.$Typeable.$__324__555__0,[$tc2,$tc2._1]);});
$Data.$Typeable.$TyCon__=
 new _F_(function($x1,$x2)
         {return {_tag_:0,_1:$x1,_2:$x2};});
$Data.$Typeable.$mkTyConKey=
 new _F_(function($str)
         {return 0;});
$Data.$Typeable.$mkTyCon=
 new _F_(function($str)
         {var $__=
           new _A_($Data.$Typeable.$mkTyConKey,[$str]);
          return new _A_($Data.$Typeable.$TyCon__,[$__,$str]);});
$UHC.$Base.$datatypeName=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$Data.$Typeable.$Typeable0_27__DCT320__23__0DFLData_2eTypeable_2etypeOf0_27=
 new _F_(function($__,$x)
         {var $__3=
           new _A_($UHC.$Base.$datatypeName,[$__,$x]);
          var $__4=
           new _A_($Data.$Typeable.$mkTyCon,[$__3]);
          return new _A_($Data.$Typeable.$mkTyConApp,[$__4,$UHC.$Base.$_5b_5d]);});
$Data.$Typeable.$Typeable0_27__CLS320__22__0=
 new _F_(function($Typeable0_27__)
         {var $Typeable0_27__2=
           {_tag_:0,_1:$UHC.$Base.$undefined};
          return $Typeable0_27__2;});
$Data.$Typeable.$Typeable0_27__NEW314UNQ608EVLDCT320__23__0RDC=
 new _F_(function($Typeable0_27__,$__)
         {var $Typeable0_27__3=
           _e_(new _A_($Data.$Typeable.$Typeable0_27__CLS320__22__0,[$Typeable0_27__]));
          var $__5=
           new _A_($Data.$Typeable.$Typeable0_27__DCT320__23__0DFLData_2eTypeable_2etypeOf0_27,[$__]);
          var $__6=
           {_tag_:0,_1:$__5};
          return $__6;});
$Data.$Typeable.$Typeable0_27__NEW311UNQ606DCT320__23__0RDC=
 new _F_(function($Typeable0_27__,$__)
         {var $Typeable0_27__3=
           new _A_($Data.$Typeable.$Typeable0_27__NEW314UNQ608EVLDCT320__23__0RDC,[$Typeable0_27__,$__]);
          return $Typeable0_27__3;});
$Data.$Typeable.$Typeable0_27__DCT320__23__0=
 new _F_(function($__)
         {var $Typeable0_27__=
           _i_();
          _i_set_($Typeable0_27__,new _A_($Data.$Typeable.$Typeable0_27__NEW311UNQ606DCT320__23__0RDC,[$Typeable0_27__,$__]));
          return $Typeable0_27__;});
$UHC.$Generics.$Tuple.$Datatype__DCT146__0__0DFLUHC_2eBase_2emoduleName=
 new _F_(function($__)
         {return new _A_($UHC.$Base.$packedStringToString,["Prelude"]);});
$UHC.$Generics.$Tuple.$Datatype__DCT146__0__0DFLUHC_2eBase_2edatatypeName=
 new _F_(function($__)
         {return new _A_($UHC.$Base.$packedStringToString,["()"]);});
$UHC.$Base.$Datatype__CLS74__350__0=
 new _F_(function($Datatype__)
         {var $Datatype__2=
           {_tag_:0,_1:$UHC.$Base.$undefined,_2:$UHC.$Base.$undefined};
          return $Datatype__2;});
$UHC.$Generics.$Tuple.$Datatype__NEW455UNQ726EVLDCT146__0__0RDC=
 new _F_(function($Datatype__)
         {var $Datatype__2=
           _e_(new _A_($UHC.$Base.$Datatype__CLS74__350__0,[$Datatype__]));
          var $__5=
           {_tag_:0,_1:$UHC.$Generics.$Tuple.$Datatype__DCT146__0__0DFLUHC_2eBase_2edatatypeName,_2:$UHC.$Generics.$Tuple.$Datatype__DCT146__0__0DFLUHC_2eBase_2emoduleName};
          return $__5;});
$UHC.$Generics.$Tuple.$Datatype__NEW453UNQ725DCT146__0__0RDC=
 new _F_(function($Datatype__)
         {var $Datatype__2=
           new _A_($UHC.$Generics.$Tuple.$Datatype__NEW455UNQ726EVLDCT146__0__0RDC,[$Datatype__]);
          return $Datatype__2;});
$UHC.$Generics.$Tuple.$Datatype__UNQ725DCT146__0__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Generics.$Tuple.$Datatype__NEW453UNQ725DCT146__0__0RDC,[$UHC.$Generics.$Tuple.$Datatype__UNQ725DCT146__0__0RDC]);}),[]);
$UHC.$Generics.$Tuple.$Datatype__DCT146__0__0=
 new _A_(new _F_(function()
                 {return $UHC.$Generics.$Tuple.$Datatype__UNQ725DCT146__0__0RDC;}),[]);
$Data.$Typeable.$__322__5766__0__4__0UNQ705=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Typeable.$Typeable0_27__DCT320__23__0,[$UHC.$Generics.$Tuple.$Datatype__DCT146__0__0]);}),[]);
$UHC.$Generics.$Tuple.$Representable0__DCT146__2__0DFLUHC_2eBase_2eto0=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return [];});
$UHC.$Generics.$Tuple.$Representable0__DCT146__2__0DFLUHC_2eBase_2efrom0=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $UHC.$Base.$U1__;});
$UHC.$Generics.$Tuple.$Representable0__NEW700UNQ921EVLDCT146__2__0RDC=
 new _F_(function($Representable0__)
         {var $Representable0__2=
           _e_(new _A_($UHC.$Base.$Representable0__CLS74__369__0,[$Representable0__]));
          var $__5=
           {_tag_:0,_1:$UHC.$Generics.$Tuple.$Representable0__DCT146__2__0DFLUHC_2eBase_2efrom0,_2:$UHC.$Generics.$Tuple.$Representable0__DCT146__2__0DFLUHC_2eBase_2eto0};
          return $__5;});
$UHC.$Generics.$Tuple.$Representable0__NEW698UNQ920DCT146__2__0RDC=
 new _F_(function($Representable0__)
         {var $Representable0__2=
           new _A_($UHC.$Generics.$Tuple.$Representable0__NEW700UNQ921EVLDCT146__2__0RDC,[$Representable0__]);
          return $Representable0__2;});
$UHC.$Generics.$Tuple.$Representable0__UNQ920DCT146__2__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Generics.$Tuple.$Representable0__NEW698UNQ920DCT146__2__0RDC,[$UHC.$Generics.$Tuple.$Representable0__UNQ920DCT146__2__0RDC]);}),[]);
$UHC.$Generics.$Tuple.$Representable0__DCT146__2__0=
 new _A_(new _F_(function()
                 {return $UHC.$Generics.$Tuple.$Representable0__UNQ920DCT146__2__0RDC;}),[]);
$Data.$Typeable.$Typeable__DCT320__26__0DFLData_2eTypeable_2etypeOf=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Typeable.$typeOf0default,[$UHC.$Generics.$Tuple.$Representable0__DCT146__2__0,$Data.$Typeable.$__322__5766__0__4__0UNQ705,$UHC.$Base.$undefined]);}),[]);
$Data.$Typeable.$Typeable__CLS320__7__0=
 new _F_(function($Typeable__)
         {var $Typeable__2=
           {_tag_:0,_1:$UHC.$Base.$undefined};
          return $Typeable__2;});
$Data.$Typeable.$Typeable__NEW567UNQ708EVLDCT320__26__0RDC=
 new _F_(function($Typeable__,$Typeable__2)
         {var $Typeable__3=
           _e_(new _A_($Data.$Typeable.$Typeable__CLS320__7__0,[$Typeable__]));
          var $__5=
           {_tag_:0,_1:$Typeable__2};
          return $__5;});
$Data.$Typeable.$Typeable__NEW564UNQ704DCT320__26__0RDC=
 new _F_(function($Typeable__,$Typeable__2)
         {var $Typeable__3=
           new _A_($Data.$Typeable.$Typeable__NEW567UNQ708EVLDCT320__26__0RDC,[$Typeable__,$Typeable__2]);
          return $Typeable__3;});
$Data.$Typeable.$Typeable__UNQ704DCT320__26__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Typeable.$Typeable__NEW564UNQ704DCT320__26__0RDC,[$Data.$Typeable.$Typeable__UNQ704DCT320__26__0RDC,$Data.$Typeable.$Typeable__DCT320__26__0DFLData_2eTypeable_2etypeOf]);}),[]);
$Data.$Typeable.$Typeable__DCT320__26__0=
 new _A_(new _F_(function()
                 {return $Data.$Typeable.$Typeable__UNQ704DCT320__26__0RDC;}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$__179__2130__1__0UNQ630=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$TimerClass.$Typeable__DCT177__9__0,[$Data.$Typeable.$Typeable__DCT320__26__0]);}),[]);
$Data.$Typeable.$typeOf=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$UHC.$Base.$unsafeCoerce=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primUnsafeId($__2);});
$Data.$Dynamic.$Dynamic__=
 new _F_(function($x1,$x2)
         {return {_tag_:0,_1:$x1,_2:$x2};});
$Data.$Dynamic.$toDyn=
 new _F_(function($__,$v)
         {var $__3=
           new _A_($UHC.$Base.$unsafeCoerce,[$v]);
          var $__4=
           new _A_($Data.$Typeable.$typeOf,[$__,$v]);
          return new _A_($Data.$Dynamic.$Dynamic__,[$__4,$__3]);});
$UHC.$Base.$Right__=
 new _F_(function($x1)
         {return {_tag_:1,_1:$x1};});
$LightOO.$Core.$hideRecord=
 new _F_(function($__,$x1)
         {var $__3=
           new _A_($UHC.$Base.$packedStringToString,["invariant: cannot hide an already hidden record"]);
          var $__4=
           new _A_($UHC.$Base.$error,[$__3]);
          var $__5=
           _e_($x1);
          var $__swJSW547__0;
          switch($__5._tag_)
           {case 0:
             var $__7=
              new _A_($Data.$Dynamic.$toDyn,[$__,$__5._1]);
             var $__8=
              new _A_($UHC.$Base.$Right__,[$__7]);
             $__swJSW547__0=
              $__8;
             break;
            case 1:
             $__swJSW547__0=
              $__4;
             break;}
          return $__swJSW547__0;});
$Graphics.$UI.$WXCore.$TimerClass.$__181__518=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$hideRecord,[$Graphics.$UI.$WXCore.$TimerClass.$__179__2130__1__0UNQ630]);}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$Narrow__DCT177__0__0DFLLightOO_2eCore_2enarrow=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$EventClass.$modify__Event__Tail,[$Graphics.$UI.$WXCore.$TimerClass.$__181__518]);}),[]);
$LightOO.$Core.$Narrow__CLS69__5__0=
 new _F_(function($Narrow__)
         {var $Narrow__2=
           {_tag_:0,_1:$UHC.$Base.$undefined};
          return $Narrow__2;});
$Graphics.$UI.$WXCore.$TimerClass.$Narrow__NEW294UNQ632EVLDCT177__0__0RDC=
 new _F_(function($Narrow__,$Narrow__2)
         {var $Narrow__3=
           _e_(new _A_($LightOO.$Core.$Narrow__CLS69__5__0,[$Narrow__2]));
          var $__5=
           {_tag_:0,_1:$Narrow__};
          return $__5;});
$Graphics.$UI.$WXCore.$TimerClass.$Narrow__NEW291UNQ629DCT177__0__0RDC=
 new _F_(function($Narrow__,$Narrow__2)
         {var $Narrow__3=
           new _A_($Graphics.$UI.$WXCore.$TimerClass.$Narrow__NEW294UNQ632EVLDCT177__0__0RDC,[$Narrow__,$Narrow__2]);
          return $Narrow__3;});
$Graphics.$UI.$WXCore.$TimerClass.$Narrow__UNQ629DCT177__0__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$TimerClass.$Narrow__NEW291UNQ629DCT177__0__0RDC,[$Graphics.$UI.$WXCore.$TimerClass.$Narrow__DCT177__0__0DFLLightOO_2eCore_2enarrow,$Graphics.$UI.$WXCore.$TimerClass.$Narrow__UNQ629DCT177__0__0RDC]);}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$Narrow__DCT177__0__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$TimerClass.$Narrow__UNQ629DCT177__0__0RDC;}),[]);
$Graphics.$UI.$WXCore.$Types.$_24okUNQ794=
 new _F_(function($v,$f,$_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$_24x]);
          var $__5=
           new _A_($f,[$_24x]);
          var $__6=
           new _A_($UHC.$IOBase.$writeIORef,[$v,$__5]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__]);});
$UHC.$MutVar.$readMutVar=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primReadMutVar($__3,$__4);});
$UHC.$STRef.$readSTRef=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          var $__4=
           new _A_($UHC.$MutVar.$readMutVar,[$__2._1]);
          var $__5=
           new _A_($UHC.$Base.$_24,[$UHC.$ST.$ST__,$__4]);
          return $__5;});
$UHC.$IOBase.$readIORef=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$STRef.$readSTRef,[$__]);
          return new _A_($UHC.$IOBase.$stToIO,[$__2]);});
$Graphics.$UI.$WXCore.$Types.$varUpdate=
 new _F_(function($v,$f)
         {var $__=
           new _A_($UHC.$IOBase.$readIORef,[$v]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$Types.$_24okUNQ794,[$v,$f]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Graphics.$UI.$WXCore.$Types.$wxID__HIGHEST=
 new _A_(new _F_(function()
                 {return 5999;}),[]);
$Graphics.$UI.$WXCore.$Types.$__55__2984=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$Graphics.$UI.$WXCore.$Types.$wxID__HIGHEST,1]);}),[]);
$Graphics.$UI.$WXCore.$Types.$__55__2983=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$Types.$varCreate,[$Graphics.$UI.$WXCore.$Types.$__55__2984]);}),[]);
$Graphics.$UI.$WXCore.$Types.$varTopId=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$IOBase.$unsafePerformIO,[$Graphics.$UI.$WXCore.$Types.$__55__2983]);}),[]);
$Graphics.$UI.$WXCore.$Types.$__55__2990__0=
 new _F_(function($_24x__52__9__0)
         {return new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$_24x__52__9__0,1]);});
$Graphics.$UI.$WXCore.$Types.$idCreate=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$Types.$varUpdate,[$Graphics.$UI.$WXCore.$Types.$varTopId,$Graphics.$UI.$WXCore.$Types.$__55__2990__0]);}),[]);
$Graphics.$UI.$WXCore.$Timer.$__193__108NEW37=
 new _F_(function($__,$self,$_24x,$_24x4,$_24x5,$milli,$oneshot)
         {var $__8=
           new _A_($LightOO.$Core.$Sub__DCT69__24__0,[$LightOO.$Core.$Sub__DCT69__13__0,$Graphics.$UI.$WXCore.$TimerClass.$Narrow__DCT177__0__0]);
          var $this=
           new _A_($LightOO.$Core.$upcast,[$__,$self]);
          var $__10=
           new _A_($Graphics.$UI.$WXCore.$Timer.$_24okUNQ97,[$self,$_24x,$_24x4,$_24x5,$milli,$oneshot,$this,$__8]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Graphics.$UI.$WXCore.$Types.$idCreate,$__10]);});
$Graphics.$UI.$WXCore.$Timer.$__191__488__0UNQ87=
 new _F_(function($__,$self,$_24x,$_24x4,$_24x5,$_24x6,$milli,$oneshot)
         {var $__9=
           new _A_($Graphics.$UI.$WXCore.$Timer.$__193__108NEW37,[$__,$self,$_24x,$_24x5,$_24x6,$milli,$oneshot]);
          var $__10=
           new _A_($UHC.$IOBase.$writeIORef,[$_24x4,$oneshot]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__10,$__9]);});
$UHC.$MutVar.$writeMutVar=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__);
          var $__5=
           _e_($__3);
          return primWriteMutVar($__4,$__2,$__5);});
$UHC.$STRef.$__132__9__0=
 new _F_(function($var,$val,$s1)
         {var $__=
           new _A_($UHC.$MutVar.$writeMutVar,[$var,$val,$s1]);
          return [$__,[]];});
$UHC.$STRef.$__132__5__0=
 new _F_(function($var,$val)
         {var $__=
           new _A_($UHC.$STRef.$__132__9__0,[$var,$val]);
          return new _A_($UHC.$Base.$_24,[$UHC.$ST.$ST__,$__]);});
$UHC.$STRef.$writeSTRef=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return new _A_($UHC.$STRef.$__132__5__0,[$__2._1]);});
$UHC.$IOBase.$writeIORef=
 new _F_(function($__,$v)
         {var $__3=
           new _A_($UHC.$STRef.$writeSTRef,[$__,$v]);
          return new _A_($UHC.$IOBase.$stToIO,[$__3]);});
$Graphics.$UI.$WXCore.$Timer.$__191__489__0UNQ136=
 new _F_(function($_24x,$_24x2,$newOwner,$newId)
         {var $__=
           new _A_($UHC.$IOBase.$writeIORef,[$_24x,$newOwner]);
          var $__6=
           new _A_($UHC.$IOBase.$writeIORef,[$_24x2,$newId]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__]);});
$Graphics.$UI.$WXCore.$Timer.$__193__77NEW19=
 new _F_(function($tail,$__,$self,$_24x,$_24x5,$_24x6,$_24x7,$_24x8)
         {var $__9=
           new _A_($Graphics.$UI.$WXCore.$Timer.$__191__487__0NEW28UNQ74,[$_24x7,$_24x8]);
          var $__10=
           new _A_($UHC.$IOBase.$readIORef,[$_24x7]);
          var $__11=
           new _A_($UHC.$IOBase.$readIORef,[$_24x6]);
          var $__12=
           new _A_($UHC.$IOBase.$readIORef,[$_24x5]);
          var $__13=
           new _A_($UHC.$IOBase.$readIORef,[$_24x]);
          var $__14=
           new _A_($UHC.$IOBase.$readIORef,[$_24x8]);
          var $__15=
           new _A_($Graphics.$UI.$WXCore.$Timer.$__191__488__0UNQ87,[$__,$self,$_24x5,$_24x6,$_24x7,$_24x8]);
          var $__16=
           new _A_($Graphics.$UI.$WXCore.$Timer.$__191__489__0UNQ136,[$_24x5,$_24x8]);
          return {_tag_:0,__timerGetId:$__14,__timerGetInterval:$__13,__timerGetOwner:$__12,__timerIsOneShot:$__11,__timerIsRunning:$__10,__timerSetOwner:$__16,__timerStart:$__15,__timerStop:$__9,__timerTail:$tail};});
$Graphics.$UI.$WXCore.$Timer.$_24okUNQ69=
 new _F_(function($tail,$__,$self,$_24x,$_24x5,$_24x6,$_24x7,$_24x8)
         {var $__9=
           new _A_($Graphics.$UI.$WXCore.$Timer.$__193__77NEW19,[$tail,$__,$self,$_24x,$_24x5,$_24x6,$_24x7,$_24x8]);
          return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__9]);});
$Graphics.$UI.$WXCore.$Timer.$_24okUNQ61=
 new _F_(function($tail,$__,$self,$_24x,$_24x5,$_24x6,$_24x7,$_24x8)
         {var $__9=
           new _A_($UHC.$IOBase.$newIORef,[$_24x8]);
          var $__10=
           new _A_($Graphics.$UI.$WXCore.$Timer.$_24okUNQ69,[$tail,$__,$self,$_24x,$_24x5,$_24x6,$_24x7]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__9,$__10]);});
$Graphics.$UI.$WXCore.$Timer.$_24okUNQ53=
 new _F_(function($id,$tail,$__,$self,$_24x,$_24x6,$_24x7,$_24x8)
         {var $__9=
           new _A_($Graphics.$UI.$WXCore.$Timer.$__193__233NEW91,[$id]);
          var $__10=
           new _A_($Graphics.$UI.$WXCore.$Timer.$_24okUNQ61,[$tail,$__,$self,$_24x,$_24x6,$_24x7,$_24x8]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__9,$__10]);});
$Graphics.$UI.$WXCore.$Timer.$_24okUNQ45=
 new _F_(function($id,$tail,$__,$self,$_24x,$_24x6,$_24x7)
         {var $__8=
           new _A_($UHC.$IOBase.$newIORef,[$UHC.$Base.$False__]);
          var $__9=
           new _A_($Graphics.$UI.$WXCore.$Timer.$_24okUNQ53,[$id,$tail,$__,$self,$_24x,$_24x6,$_24x7]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__8,$__9]);});
$Graphics.$UI.$WXCore.$Timer.$_24okUNQ39=
 new _F_(function($id,$tail,$__,$self,$_24x,$_24x6)
         {var $__7=
           new _A_($UHC.$IOBase.$newIORef,[$UHC.$Base.$False__]);
          var $__8=
           new _A_($Graphics.$UI.$WXCore.$Timer.$_24okUNQ45,[$id,$tail,$__,$self,$_24x,$_24x6]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__7,$__8]);});
$Graphics.$UI.$WXCore.$Timer.$_24okUNQ33=
 new _F_(function($owner,$id,$tail,$__,$self,$_24x)
         {var $__7=
           new _A_($UHC.$IOBase.$newIORef,[$owner]);
          var $__8=
           new _A_($Graphics.$UI.$WXCore.$Timer.$_24okUNQ39,[$id,$tail,$__,$self,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__7,$__8]);});
$Graphics.$UI.$WXCore.$Timer.$timer_27UNQ25=
 new _F_(function($owner,$id,$tail,$super,$__,$self)
         {var $__7=
           new _A_($UHC.$IOBase.$newIORef,[0]);
          var $__8=
           new _A_($Graphics.$UI.$WXCore.$Timer.$_24okUNQ33,[$owner,$id,$tail,$__,$self]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__7,$__8]);});
$Graphics.$UI.$WXCore.$Timer.$__193__261__0=
 new _F_(function($owner,$__,$id,$__4,$__5)
         {return new _A_($Graphics.$UI.$WXCore.$Timer.$timer_27UNQ25,[$owner,$id,$__4,$__5,$__]);});
$LightOO.$Core.$__73__438__0=
 new _F_(function($cont,$self,$t)
         {return new _A_($cont,[$t,$self]);});
$LightOO.$Core.$clazz=
 new _F_(function($cont,$tail,$self)
         {var $__=
           new _A_($LightOO.$Core.$__73__438__0,[$cont,$self]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$tail,$__]);});
$LightOO.$Core.$_24okUNQ158=
 new _F_(function($oplus,$_24x,$_24x3)
         {var $__=
           new _A_($LightOO.$Core.$record,[$_24x]);
          var $__5=
           new _A_($oplus,[$_24x3,$__]);
          var $__6=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
          return new _A_($UHC.$Base.$_24,[$__6,$__5]);});
$LightOO.$Core.$_24okUNQ154=
 new _F_(function($override,$oplus,$self,$_24x,$_24x5)
         {var $__=
           new _A_($override,[$_24x,$self]);
          var $__7=
           new _A_($LightOO.$Core.$_24okUNQ158,[$oplus,$_24x5]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__7]);});
$LightOO.$Core.$_24okUNQ146=
 new _F_(function($w,$override,$oplus,$tail,$self,$_24x)
         {var $__=
           new _A_($w,[$tail,$_24x,$self]);
          var $__8=
           new _A_($LightOO.$Core.$_24okUNQ154,[$override,$oplus,$self,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__8]);});
$UHC.$Base.$Left__=
 new _F_(function($x1)
         {return {_tag_:0,_1:$x1};});
$LightOO.$Core.$record=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Left__;}),[]);
$LightOO.$Core.$__73__278=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$record,[[]]);}),[]);
$LightOO.$Core.$emptyRecord=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$LightOO.$Core.$__73__278]);}),[]);
$LightOO.$Core.$__73__453__0=
 new _F_(function($w,$g,$override,$oplus,$tail,$self)
         {var $__=
           new _A_($g,[$LightOO.$Core.$emptyRecord,$self]);
          var $__8=
           new _A_($LightOO.$Core.$_24okUNQ146,[$w,$override,$oplus,$tail,$self]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__8]);});
$LightOO.$Core.$extends=
 new _F_(function($w,$g,$override,$oplus)
         {var $__=
           new _A_($LightOO.$Core.$__73__453__0,[$w,$g,$override,$oplus]);
          return new _A_($UHC.$Base.$_24,[$LightOO.$Core.$clazz,$__]);});
$LightOO.$Core.$noOverride=
 new _F_(function($sup,$__)
         {return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$sup]);});
$Graphics.$UI.$WXCore.$Timer.$timer=
 new _F_(function($owner,$__,$id)
         {var $__4=
           new _A_($Graphics.$UI.$WXCore.$Timer.$__193__261__0,[$owner,$__,$id]);
          return new _A_($LightOO.$Core.$extends,[$__4,$Graphics.$UI.$WXCore.$EvtHandler.$evthandler,$LightOO.$Core.$noOverride,$Graphics.$UI.$WXCore.$EvtHandlerClass.$set__EvtHandler__Tail]);});
$Graphics.$UI.$WXCore.$WebWindow.$windowTimerCreate=
 new _F_(function($w)
         {var $__=
           new _A_($LightOO.$Core.$upcast,[$Graphics.$UI.$WXCore.$WebWindow.$__197__71__2__0,$w]);
          var $__3=
           new _A_($Graphics.$UI.$WXCore.$Timer.$timer,[$__,$LightOO.$Core.$Sub__DCT69__20__0,$Graphics.$UI.$WXCore.$Types.$idAny]);
          var $__4=
           new _A_($LightOO.$Core.$new,[$Control.$Monad.$Fix.$MonadFix__DCT97__4__0]);
          return new _A_($UHC.$Base.$_24,[$__4,$__3]);});
$Graphics.$UI.$WXCore.$TimerClass.$__timerStart=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__timerStart;});
$Graphics.$UI.$WXCore.$TimerClass.$timerStart=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$TimerClass.$__timerStart,$Graphics.$UI.$WXCore.$TimerClass.$timer__Methods]);}),[]);
$Graphics.$UI.$WX.$Attributes.$setpropUNQ655=
 new _F_(function($w,$x1)
         {var $__=
           _e_($x1);
          var $__swJSW552__0;
          switch($__._tag_)
           {case 0:
             var $__6=
              _e_($__._1);
             var $__12=
              new _A_($__._2,[$w]);
             var $__13=
              new _A_($__6._4,[$w,$__12]);
             $__swJSW552__0=
              $__13;
             break;
            case 1:
             var $__16=
              _e_($__._1);
             var $__22=
              new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
             var $__23=
              new _A_($__._2,[$w]);
             var $__24=
              new _A_($__16._5,[$w,$__23]);
             var $__25=
              new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__24,$__22]);
             $__swJSW552__0=
              $__25;
             break;
            case 2:
             var $__28=
              _e_($__._1);
             var $__34=
              new _A_($__28._4,[$w,$__._2]);
             $__swJSW552__0=
              $__34;
             break;
            case 3:
             var $__37=
              _e_($__._1);
             var $__43=
              new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
             var $__44=
              new _A_($__37._5,[$w,$__._2]);
             var $__45=
              new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__44,$__43]);
             $__swJSW552__0=
              $__45;
             break;}
          return $__swJSW552__0;});
$UHC.$Base.$foldr=
 new _F_(function($x1,$x2,$x3)
         {var $x34=
           _e_($x3);
          var $__swJSW557__0;
          switch($x34._tag_)
           {case 0:
             var $__=
              new _A_($UHC.$Base.$foldr,[$x1,$x2,$x34._2]);
             var $__8=
              new _A_($x1,[$x34._1,$__]);
             $__swJSW557__0=
              $__8;
             break;
            case 1:
             $__swJSW557__0=
              $x2;
             break;}
          return $__swJSW557__0;});
$UHC.$Base.$sequence__=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$return,[$__,[]]);
          var $__3=
           new _A_($UHC.$Base.$_3e_3e,[$__]);
          return new _A_($UHC.$Base.$foldr,[$__3,$__2]);});
$UHC.$Base.$mapM__=
 new _F_(function($__,$f)
         {var $__3=
           new _A_($UHC.$Base.$map,[$f]);
          var $__4=
           new _A_($UHC.$Base.$sequence__,[$__]);
          return new _A_($UHC.$Base.$_2e,[$__4,$__3]);});
$Graphics.$UI.$WX.$Attributes.$set=
 new _F_(function($w,$props)
         {var $__=
           new _A_($Graphics.$UI.$WX.$Attributes.$setpropUNQ655,[$w]);
          return new _A_($UHC.$Base.$mapM__,[$UHC.$Base.$Monad__DCT74__339__0,$__,$props]);});
$Graphics.$UI.$WX.$Timer.$_24okUNQ14=
 new _F_(function($props,$_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$_24x]);
          var $__4=
           new _A_($Graphics.$UI.$WX.$Attributes.$set,[$_24x,$props]);
          var $__5=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__]);
          var $__6=
           new _A_($Graphics.$UI.$WXCore.$TimerClass.$timerStart,[$_24x,1000,$UHC.$Base.$False__]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__5]);});
$Graphics.$UI.$WX.$Timer.$timer=
 new _F_(function($parent,$props)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$WebWindow.$windowTimerCreate,[$parent]);
          var $__4=
           new _A_($Graphics.$UI.$WX.$Timer.$_24okUNQ14,[$props]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Graphics.$UI.$WX.$Events.$command=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$Graphics.$UI.$WX.$Events.$on=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2._1;});
$Graphics.$UI.$WX.$Events.$Commanding__CLS213__1__0=
 new _F_(function($Commanding__)
         {var $Commanding__2=
           {_tag_:0,_1:$UHC.$Base.$undefined};
          return $Commanding__2;});
$Graphics.$UI.$WX.$Timer.$Commanding__NEW55UNQ63EVLDCT237__3__0RDC=
 new _F_(function($Commanding__,$Commanding__2)
         {var $Commanding__3=
           _e_(new _A_($Graphics.$UI.$WX.$Events.$Commanding__CLS213__1__0,[$Commanding__2]));
          var $__5=
           {_tag_:0,_1:$Commanding__};
          return $__5;});
$Graphics.$UI.$WX.$Timer.$Commanding__NEW52UNQ62DCT237__3__0RDC=
 new _F_(function($Commanding__,$Commanding__2)
         {var $Commanding__3=
           new _A_($Graphics.$UI.$WX.$Timer.$Commanding__NEW55UNQ63EVLDCT237__3__0RDC,[$Commanding__,$Commanding__2]);
          return $Commanding__3;});
$Graphics.$UI.$WX.$Timer.$__241__122=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["command"]);}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$_24okUNQ540=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["touched: event object"]);
          var $__3=
           new _A_($UHC.$Base.$error,[$__]);
          var $__4=
           new _A_($_24x,[$__3]);
          var $__5=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
          return new _A_($UHC.$Base.$_24,[$__5,$__4]);});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$__evtGetHandler=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__evtGetHandler;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$evtHandlerGetHandler=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$EvtHandlerClass.$__evtGetHandler,$Graphics.$UI.$WXCore.$EvtHandlerClass.$evtHandler__Methods]);}),[]);
$UHC.$Base.$maybe=
 new _F_(function($x1,$x2,$x3)
         {var $x34=
           _e_($x3);
          var $__swJSW562__0;
          switch($x34._tag_)
           {case 0:
             var $__=
              new _A_($x2,[$x34._1]);
             $__swJSW562__0=
              $__;
             break;
            case 1:
             $__swJSW562__0=
              $x1;
             break;}
          return $__swJSW562__0;});
$Graphics.$UI.$WXCore.$TimerClass.$_24okUNQ544=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
          var $__3=
           new _A_($UHC.$Base.$_24,[$UHC.$Base.$const,$__]);
          var $__4=
           new _A_($UHC.$Base.$maybe,[$__3,$UHC.$Base.$id,$_24x]);
          var $__5=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
          return new _A_($UHC.$Base.$_24,[$__5,$__4]);});
$Graphics.$UI.$WXCore.$TimerClass.$__181__761NEW422=
 new _F_(function($_24x)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$_24x,$Graphics.$UI.$WXCore.$EvtHandlerClass.$evtHandlerGetHandler,$Graphics.$UI.$WXCore.$Types.$wxEVT__TIMER,$Graphics.$UI.$WXCore.$Types.$idAny,$Graphics.$UI.$WXCore.$Types.$idAny]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$Graphics.$UI.$WXCore.$TimerClass.$_24okUNQ544]);});
$Graphics.$UI.$WXCore.$TimerClass.$_24okUNQ536=
 new _F_(function($_24x)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$TimerClass.$__181__761NEW422,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$Graphics.$UI.$WXCore.$TimerClass.$_24okUNQ540]);});
$Graphics.$UI.$WXCore.$TimerClass.$timerGetOnCommand=
 new _F_(function($t)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$t,$Graphics.$UI.$WXCore.$TimerClass.$timerGetOwner]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$Graphics.$UI.$WXCore.$TimerClass.$_24okUNQ536]);});
$Graphics.$UI.$WX.$Attributes.$_24okUNQ551=
 new _F_(function($setter,$w,$f,$_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$_24x]);
          var $__6=
           new _A_($f,[$_24x]);
          var $__7=
           new _A_($setter,[$w,$__6]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__7,$__]);});
$Graphics.$UI.$WX.$Attributes.$updaterUNQ545=
 new _F_(function($getter,$setter,$w,$f)
         {var $__=
           new _A_($getter,[$w]);
          var $__6=
           new _A_($Graphics.$UI.$WX.$Attributes.$_24okUNQ551,[$setter,$w,$f]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__6]);});
$Graphics.$UI.$WX.$Attributes.$Attr__=
 new _F_(function($x1,$x2,$x3,$x4,$x5)
         {return {_tag_:0,_1:$x1,_2:$x2,_3:$x3,_4:$x4,_5:$x5};});
$Graphics.$UI.$WX.$Attributes.$makeAttr=
 new _F_(function($name)
         {return new _A_($Graphics.$UI.$WX.$Attributes.$Attr__,[$name,$UHC.$Base.$Nothing__]);});
$Graphics.$UI.$WX.$Attributes.$newAttr=
 new _F_(function($name,$getter,$setter)
         {var $__=
           new _A_($Graphics.$UI.$WX.$Attributes.$updaterUNQ545,[$getter,$setter]);
          return new _A_($Graphics.$UI.$WX.$Attributes.$makeAttr,[$name,$getter,$setter,$__]);});
$Graphics.$UI.$WX.$Events.$Event__=
 new _F_(function($x1)
         {return {_tag_:0,_1:$x1};});
$Graphics.$UI.$WX.$Events.$newEvent=
 new _F_(function($name,$getter,$setter)
         {var $__=
           new _A_($Graphics.$UI.$WX.$Attributes.$newAttr,[$name,$getter,$setter]);
          return new _A_($Graphics.$UI.$WX.$Events.$Event__,[$__]);});
$Graphics.$UI.$WXCore.$Types.$wxID__ANY=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__3=
           new _A_($UHC.$Base.$fromInteger,[$__,$__2]);
          return new _A_($UHC.$Base.$negate,[$__,$__3]);});
$Graphics.$UI.$WXCore.$Types.$idAny=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$Types.$wxID__ANY,[$UHC.$Base.$Num__DCT74__101__0]);}),[]);
$UHC.$Base.$const=
 new _F_(function($k,$__)
         {return $k;});
$LightOO.$Core.$_23=
 new _F_(function($o,$f)
         {return new _A_($f,[$o]);});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$__evtHandlerBind=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__evtHandlerBind;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$evtHandler__Methods=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$LightOO.$Core.$unRecord,$LightOO.$get__Object__Tail]);}),[]);
$Graphics.$UI.$WXCore.$EvtHandlerClass.$evtHandlerBind=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$EvtHandlerClass.$__evtHandlerBind,$Graphics.$UI.$WXCore.$EvtHandlerClass.$evtHandler__Methods]);}),[]);
$Graphics.$UI.$WXCore.$Types.$wxEVT__TIMER=
 new _A_(new _F_(function()
                 {return 1;}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$_24okUNQ523=
 new _F_(function($f,$_24x)
         {var $__=
           new _A_($UHC.$Base.$const,[$f]);
          return new _A_($LightOO.$Core.$_23,[$_24x,$Graphics.$UI.$WXCore.$EvtHandlerClass.$evtHandlerBind,$Graphics.$UI.$WXCore.$Types.$wxEVT__TIMER,$__,$Graphics.$UI.$WXCore.$Types.$idAny,$Graphics.$UI.$WXCore.$Types.$idAny]);});
$Graphics.$UI.$WXCore.$TimerClass.$__timerGetOwner=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__timerGetOwner;});
$LightOO.$ModTail__DCT153__3__0DFLLightOO_2eCore_2esetTail=
 new _F_(function($o,$v)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["has no field update '[LightOO._objectTail]'"]);
          var $__4=
           new _A_($UHC.$Base.$error,[$__]);
          var $__5=
           _e_($o);
          var $__9=
           {_tag_:0,objectGetFlag:$__5.objectGetFlag,objectSetFlag:$__5.objectSetFlag,__objectTail:$v};
          return $__9;});
$LightOO.$__objectTail=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__objectTail;});
$LightOO.$ModTail__NEW150UNQ218EVLDCT153__3__0RDC=
 new _F_(function($ModTail__)
         {var $ModTail__2=
           _e_(new _A_($LightOO.$Core.$ModTail__CLS69__7__0,[$ModTail__]));
          var $__6=
           {_tag_:0,_1:$LightOO.$__objectTail,_2:$ModTail__2._2,_3:$LightOO.$ModTail__DCT153__3__0DFLLightOO_2eCore_2esetTail};
          return $__6;});
$LightOO.$ModTail__NEW148UNQ217DCT153__3__0RDC=
 new _F_(function($ModTail__)
         {var $ModTail__2=
           new _A_($LightOO.$ModTail__NEW150UNQ218EVLDCT153__3__0RDC,[$ModTail__]);
          return $ModTail__2;});
$LightOO.$ModTail__UNQ217DCT153__3__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$ModTail__NEW148UNQ217DCT153__3__0RDC,[$LightOO.$ModTail__UNQ217DCT153__3__0RDC]);}),[]);
$LightOO.$ModTail__DCT153__3__0=
 new _A_(new _F_(function()
                 {return $LightOO.$ModTail__UNQ217DCT153__3__0RDC;}),[]);
$LightOO.$get__Object__Tail=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$getTail,[$LightOO.$ModTail__DCT153__3__0]);}),[]);
$Graphics.$UI.$WXCore.$EvtHandlerClass.$__157__1001=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$LightOO.$Core.$unRecord,$LightOO.$get__Object__Tail]);}),[]);
$Graphics.$UI.$WXCore.$EvtHandlerClass.$ModTail__DCT153__3__0DFLLightOO_2eCore_2esetTail=
 new _F_(function($o,$v)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["has no field update '[Graphics.UI.WXCore.EvtHandlerClass._evtHandlerTail]'"]);
          var $__4=
           new _A_($UHC.$Base.$error,[$__]);
          var $__5=
           _e_($o);
          var $__27=
           {_tag_:0,__evtHandlerAddPendingEvent:$__5.__evtHandlerAddPendingEvent,__evtHandlerBind:$__5.__evtHandlerBind,__evtGetHandler:$__5.__evtGetHandler,__evtHandlerUnBind:$__5.__evtHandlerUnBind,__evtHandlerDeletePendingEvents:$__5.__evtHandlerDeletePendingEvents,__evtHandlerGetEvtHandlerEnabled:$__5.__evtHandlerGetEvtHandlerEnabled,__evtHandlerGetNextHandler:$__5.__evtHandlerGetNextHandler,__evtHandlerGetPreviousHandler:$__5.__evtHandlerGetPreviousHandler,__evtHandlerIsUnlinked:$__5.__evtHandlerIsUnlinked,__evtHandlerProcessEvent:$__5.__evtHandlerProcessEvent,__evtHandlerProcessEventLocally:$__5.__evtHandlerProcessEventLocally,__evtHandlerProcessPendingEvents:$__5.__evtHandlerProcessPendingEvents,__evtHandlerQueueEvent:$__5.__evtHandlerQueueEvent,__evtHandlerSetEvtHandlerEnabled:$__5.__evtHandlerSetEvtHandlerEnabled,__evtHandlerSetNextHandler:$__5.__evtHandlerSetNextHandler,__evtHandlerSetPreviousHandler:$__5.__evtHandlerSetPreviousHandler,__evtHandlerTryAfter:$__5.__evtHandlerTryAfter,__evtHandlerTryBefore:$__5.__evtHandlerTryBefore,__evtHandlerTryThis:$__5.__evtHandlerTryThis,__evtHandlerUnlink:$__5.__evtHandlerUnlink,__evtHandlerTail:$v};
          return $__27;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$__evtHandlerTail=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.__evtHandlerTail;});
$LightOO.$Core.$setTail=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._3;});
$LightOO.$Core.$mkMod=
 new _F_(function($set,$get,$f,$o)
         {var $__=
           new _A_($get,[$o]);
          var $__6=
           new _A_($f,[$__]);
          return new _A_($set,[$o,$__6]);});
$LightOO.$Core.$ModTail__CLS69__7__0=
 new _F_(function($ModTail__)
         {var $__=
           new _A_($LightOO.$Core.$getTail,[$ModTail__]);
          var $__3=
           new _A_($LightOO.$Core.$setTail,[$ModTail__]);
          var $ModTail__CLS69__7__0DFLLightOO_2eCore_2emodifyTail=
           new _A_($LightOO.$Core.$mkMod,[$__3,$__]);
          var $ModTail__4=
           {_tag_:0,_1:$UHC.$Base.$undefined,_2:$ModTail__CLS69__7__0DFLLightOO_2eCore_2emodifyTail,_3:$UHC.$Base.$undefined};
          return $ModTail__4;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$ModTail__NEW490UNQ1199EVLDCT153__3__0RDC=
 new _F_(function($ModTail__)
         {var $ModTail__2=
           _e_(new _A_($LightOO.$Core.$ModTail__CLS69__7__0,[$ModTail__]));
          var $__6=
           {_tag_:0,_1:$Graphics.$UI.$WXCore.$EvtHandlerClass.$__evtHandlerTail,_2:$ModTail__2._2,_3:$Graphics.$UI.$WXCore.$EvtHandlerClass.$ModTail__DCT153__3__0DFLLightOO_2eCore_2esetTail};
          return $__6;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$ModTail__NEW488UNQ1198DCT153__3__0RDC=
 new _F_(function($ModTail__)
         {var $ModTail__2=
           new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$ModTail__NEW490UNQ1199EVLDCT153__3__0RDC,[$ModTail__]);
          return $ModTail__2;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$ModTail__UNQ1198DCT153__3__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WXCore.$EvtHandlerClass.$ModTail__NEW488UNQ1198DCT153__3__0RDC,[$Graphics.$UI.$WXCore.$EvtHandlerClass.$ModTail__UNQ1198DCT153__3__0RDC]);}),[]);
$Graphics.$UI.$WXCore.$EvtHandlerClass.$ModTail__DCT153__3__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WXCore.$EvtHandlerClass.$ModTail__UNQ1198DCT153__3__0RDC;}),[]);
$LightOO.$Core.$getTail=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$Graphics.$UI.$WXCore.$EvtHandlerClass.$__157__1000=
 new _A_(new _F_(function()
                 {return new _A_($LightOO.$Core.$getTail,[$Graphics.$UI.$WXCore.$EvtHandlerClass.$ModTail__DCT153__3__0]);}),[]);
$Graphics.$UI.$WXCore.$EvtHandlerClass.$get__EvtHandler__Tail=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$EvtHandlerClass.$__157__1000,$Graphics.$UI.$WXCore.$EvtHandlerClass.$__157__1001]);}),[]);
$LightOO.$Core.$unRecord=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          var $__swJSW573__0;
          switch($__2._tag_)
           {case 0:
             $__swJSW573__0=
              $__2._1;
             break;
            case 1:
             $__swJSW573__0=
              $UHC.$Base.$undefined;
             break;}
          return $__swJSW573__0;});
$Graphics.$UI.$WXCore.$TimerClass.$timer__Methods=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$LightOO.$Core.$unRecord,$Graphics.$UI.$WXCore.$EvtHandlerClass.$get__EvtHandler__Tail]);}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$timerGetOwner=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Graphics.$UI.$WXCore.$TimerClass.$__timerGetOwner,$Graphics.$UI.$WXCore.$TimerClass.$timer__Methods]);}),[]);
$Graphics.$UI.$WXCore.$TimerClass.$timerOnCommand=
 new _F_(function($t,$f)
         {var $__=
           new _A_($LightOO.$Core.$_23,[$t,$Graphics.$UI.$WXCore.$TimerClass.$timerGetOwner]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$TimerClass.$_24okUNQ523,[$f]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Graphics.$UI.$WX.$Timer.$Commanding__DCT237__3__0DFLGraphics_2eUI_2eWX_2eEvents_2ecommand=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Events.$newEvent,[$Graphics.$UI.$WX.$Timer.$__241__122,$Graphics.$UI.$WXCore.$TimerClass.$timerGetOnCommand,$Graphics.$UI.$WXCore.$TimerClass.$timerOnCommand]);}),[]);
$Graphics.$UI.$WX.$Timer.$Commanding__UNQ62DCT237__3__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$Timer.$Commanding__NEW52UNQ62DCT237__3__0RDC,[$Graphics.$UI.$WX.$Timer.$Commanding__DCT237__3__0DFLGraphics_2eUI_2eWX_2eEvents_2ecommand,$Graphics.$UI.$WX.$Timer.$Commanding__UNQ62DCT237__3__0RDC]);}),[]);
$Graphics.$UI.$WX.$Timer.$Commanding__DCT237__3__0=
 new _A_(new _F_(function()
                 {return $Graphics.$UI.$WX.$Timer.$Commanding__UNQ62DCT237__3__0RDC;}),[]);
$Asteroids.$_24okUNQ175=
 new _F_(function($_24x,$_24x2,$_24x3)
         {var $__=
           new _A_($Asteroids.$advance,[$Graphics.$UI.$WX.$Window.$Paint__DCT225__41__0,$_24x,$_24x3]);
          var $__5=
           new _A_($Graphics.$UI.$WX.$Events.$command,[$Graphics.$UI.$WX.$Timer.$Commanding__DCT237__3__0]);
          var $__6=
           new _A_($Graphics.$UI.$WX.$Events.$on,[$__5]);
          var $__7=
           new _A_($Graphics.$UI.$WX.$Attributes.$_3a_3d,[$__6,$__]);
          var $__8=
           new _A_($UHC.$Base.$_3a,[$__7,$UHC.$Base.$_5b_5d]);
          var $__9=
           new _A_($Graphics.$UI.$WX.$Attributes.$_3a_3d,[$Graphics.$UI.$WX.$Timer.$interval,50]);
          var $__10=
           new _A_($UHC.$Base.$_3a,[$__9,$__8]);
          var $__11=
           new _A_($Graphics.$UI.$WX.$Timer.$timer,[$_24x3,$__10]);
          var $__12=
           new _A_($Asteroids.$_24okUNQ198,[$_24x,$_24x2,$_24x3]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__11,$__12]);});
$UHC.$Base.$Nothing__=
 new _A_(new _F_(function()
                 {return {_tag_:1};}),[]);
$Graphics.$UI.$WXCore.$Types.$Size__=
 new _F_(function($x1,$x2)
         {return {_tag_:0,sizeW:$x1,sizeH:$x2};});
$Graphics.$UI.$WXCore.$Types.$sz=
 new _F_(function($__)
         {return $Graphics.$UI.$WXCore.$Types.$Size__;});
$Graphics.$UI.$WXCore.$Types.$pt=
 new _F_(function($__)
         {return $Graphics.$UI.$WXCore.$Types.$Point__;});
$Graphics.$UI.$WX.$Attributes.$_3a_3d=
 new _F_(function($x1,$x2)
         {return {_tag_:2,_1:$x1,_2:$x2};});
$Graphics.$UI.$WX.$Classes.$area=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$Graphics.$UI.$WXCore.$Types.$Rect__=
 new _F_(function($x1,$x2,$x3,$x4)
         {return {_tag_:0,rectLeft:$x1,rectTop:$x2,rectWidth:$x3,rectHeight:$x4};});
$Graphics.$UI.$WXCore.$Types.$__55__1697__0=
 new _F_(function($x,$y,$__)
         {var $__4=
           _e_($__);
          var $__7=
           new _A_($Graphics.$UI.$WXCore.$Types.$Rect__,[$x,$y,$__4.sizeW,$__4.sizeH]);
          return $__7;});
$Graphics.$UI.$WXCore.$Types.$rect=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          return new _A_($Graphics.$UI.$WXCore.$Types.$__55__1697__0,[$__3.pointX,$__3.pointY]);});
$Asteroids.$_24okUNQ161=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Types.$sz,[$UHC.$Base.$Num__DCT74__101__0,$Asteroids.$width,$Asteroids.$height]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$Types.$pt,[$UHC.$Base.$Num__DCT74__101__0,0,0]);
          var $__5=
           new _A_($Graphics.$UI.$WXCore.$Types.$rect,[$UHC.$Base.$Num__DCT74__101__0,$__4,$__]);
          var $__6=
           new _A_($Graphics.$UI.$WX.$Classes.$area,[$Graphics.$UI.$WX.$Window.$Dimensions__DCT225__4__0]);
          var $__7=
           new _A_($Graphics.$UI.$WX.$Attributes.$_3a_3d,[$__6,$__5]);
          var $__8=
           new _A_($UHC.$Base.$_3a,[$__7,$UHC.$Base.$_5b_5d]);
          var $__9=
           new _A_($Graphics.$UI.$WX.$Window.$window,[$UHC.$Base.$Nothing__,$__8]);
          var $__10=
           new _A_($Asteroids.$_24okUNQ175,[$_24x,$_24x2]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__9,$__10]);});
$UHC.$Base.$div=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._3;});
$Asteroids.$_24okUNQ149=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$div,[$UHC.$Base.$Integral__DCT74__110__0,$Asteroids.$width,2]);
          var $__3=
           new _A_($UHC.$Base.$_24,[$Graphics.$UI.$WXCore.$Types.$varCreate,$__]);
          var $__4=
           new _A_($Asteroids.$_24okUNQ161,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__3,$__4]);});
$UHC.$Base.$null=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW578__0;
          switch($__._tag_)
           {case 0:
             $__swJSW578__0=
              $UHC.$Base.$False__;
             break;
            case 1:
             $__swJSW578__0=
              $UHC.$Base.$True__;
             break;}
          return $__swJSW578__0;});
$UHC.$Base.$_24okUNQ3578=
 new _F_(function($p,$_24x)
         {var $__=
           new _A_($p,[$_24x]);
          var $__4=
           _e_($__);
          var $__swJSW579__0;
          switch($__4._tag_)
           {case 0:
             $__swJSW579__0=
              $UHC.$Base.$_5b_5d;
             break;
            case 1:
             var $__5=
              new _A_($UHC.$Base.$_3a,[$_24x,$UHC.$Base.$_5b_5d]);
             $__swJSW579__0=
              $__5;
             break;}
          return $__swJSW579__0;});
$UHC.$Base.$filter=
 new _F_(function($p,$xs)
         {var $__=
           new _A_($UHC.$Base.$_24okUNQ3578,[$p]);
          return new _A_($UHC.$Base.$concatMap,[$__,$xs]);});
$UHC.$Base.$tail=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          var $__swJSW580__0;
          switch($__2._tag_)
           {case 0:
             $__swJSW580__0=
              $__2._2;
             break;
            case 1:
             $__swJSW580__0=
              $UHC.$Base.$undefined;
             break;}
          return $__swJSW580__0;});
$UHC.$Base.$head=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          var $__swJSW581__0;
          switch($__2._tag_)
           {case 0:
             $__swJSW581__0=
              $__2._1;
             break;
            case 1:
             $__swJSW581__0=
              $UHC.$Base.$undefined;
             break;}
          return $__swJSW581__0;});
$Asteroids.$flatten=
 new _F_(function($x1,$x2)
         {var $x23=
           _e_($x2);
          var $__swJSW582__0;
          switch($x23._tag_)
           {case 0:
             var $__=
              new _A_($UHC.$Base.$map,[$UHC.$Base.$tail,$x1]);
             var $__7=
              new _A_($UHC.$Base.$_2e,[$UHC.$Base.$not,$UHC.$Base.$null]);
             var $later=
              new _A_($UHC.$Base.$filter,[$__7,$__]);
             var $now=
              new _A_($UHC.$Base.$map,[$UHC.$Base.$head,$x1]);
             var $__10=
              new _A_($UHC.$Base.$_2b_2b,[$x23._1,$later]);
             var $__11=
              new _A_($Asteroids.$flatten,[$__10,$x23._2]);
             $__swJSW582__0=
              new _A_($UHC.$Base.$_3a,[$now,$__11]);
             break;
            case 1:
             var $__=
              new _A_($UHC.$Base.$packedStringToString,["Empty rocks list not expected in function flatten"]);
             var $__13=
              new _A_($UHC.$Base.$error,[$__]);
             $__swJSW582__0=
              $__13;
             break;}
          return $__swJSW582__0;});
$Asteroids.$random=
 new _F_(function($__)
         {var $__2=
           _e_(Math.random());
          return [$__,$__2];});
$UHC.$Base.$RealWorld__=
 new _A_(new _F_(function()
                 {return {_tag_:0};}),[]);
$UHC.$Base.$realWorld=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$RealWorld__;}),[]);
$UHC.$Base.$ioWorld=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$realWorld;}),[]);
$UHC.$IOBase.$unsafePerformIO=
 new _F_(function($__)
         {var $__2=
           new _A_($__,[$UHC.$Base.$ioWorld]);
          var $__3=
           _e_($__2);
          return $__3[1];});
$Asteroids.$rand=
 new _F_(function($__)
         {return new _A_($UHC.$IOBase.$unsafePerformIO,[$Asteroids.$random]);});
$Asteroids.$randoms=
 new _A_(new _F_(function()
                 {var $inf=
                   _i_();
                  _i_set_($inf,new _A_($UHC.$Base.$_3a,[$UHC.$Base.$undefined,$inf]));
                  return new _A_($UHC.$Base.$map,[$Asteroids.$rand,$inf]);}),[]);
$Asteroids.$diameter=
 new _A_(new _F_(function()
                 {return 24;}),[]);
$Graphics.$UI.$WXCore.$Types.$Point__=
 new _F_(function($x1,$x2)
         {return {_tag_:0,pointX:$x1,pointY:$x2};});
$Graphics.$UI.$WXCore.$Types.$point=
 new _F_(function($__)
         {return $Graphics.$UI.$WXCore.$Types.$Point__;});
$Asteroids.$_24okUNQ88=
 new _F_(function($x,$_24x)
         {var $__=
           new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$_24x,$Asteroids.$diameter]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$Types.$point,[$UHC.$Base.$Num__DCT74__101__0,$x,$__]);
          return new _A_($UHC.$Base.$_3a,[$__4,$UHC.$Base.$_5b_5d]);});
$Asteroids.$height=
 new _A_(new _F_(function()
                 {return 300;}),[]);
$Asteroids.$track=
 new _F_(function($x)
         {var $__=
           new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__101__0,2,$Asteroids.$diameter]);
          var $__3=
           new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$Asteroids.$height,$__]);
          var $__4=
           new _A_($UHC.$Base.$enumFromThenTo,[$UHC.$Base.$Enum__DCT74__118__0,0,6,$__3]);
          var $__5=
           new _A_($Asteroids.$_24okUNQ88,[$x]);
          return new _A_($UHC.$Base.$concatMap,[$__5,$__4]);});
$UHC.$Base.$floor=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._4;});
$Asteroids.$width=
 new _A_(new _F_(function()
                 {return 300;}),[]);
$UHC.$Base.$primCosDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primCosDouble($__2);});
$UHC.$Base.$mNEW3634UNQ5962=
 new _F_(function($__,$__2,$__3,$n,$r)
         {var $__6=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__7=
           new _A_($UHC.$Base.$fromInteger,[$__2,$__6]);
          var $__8=
           new _A_($UHC.$Base.$_3c,[$__,$r,$__7]);
          var $__9=
           _e_($__8);
          var $__swJSW585__0;
          switch($__9._tag_)
           {case 0:
             var $__10=
              new _A_($UHC.$Base.$packedStringToInteger,["1"]);
             var $__11=
              new _A_($UHC.$Base.$fromInteger,[$__3,$__10]);
             var $__12=
              new _A_($UHC.$Base.$_2b,[$__3,$n,$__11]);
             $__swJSW585__0=
              $__12;
             break;
            case 1:
             var $__13=
              new _A_($UHC.$Base.$packedStringToInteger,["1"]);
             var $__14=
              new _A_($UHC.$Base.$fromInteger,[$__3,$__13]);
             var $__15=
              new _A_($UHC.$Base.$_2d,[$__3,$n,$__14]);
             $__swJSW585__0=
              $__15;
             break;}
          return $__swJSW585__0;});
$UHC.$Base.$nNEW3628UNQ5960=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Base.$rNEW3631UNQ5961=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Base.$__78__7405__0=
 new _F_(function($RealFrac__,$__,$__3,$__4,$__5,$__6,$__7,$x)
         {var $__9=
           new _A_($UHC.$Base.$properFraction,[$RealFrac__,$__7,$x]);
          var $n=
           new _A_($UHC.$Base.$nNEW3628UNQ5960,[$__9]);
          var $r=
           new _A_($UHC.$Base.$rNEW3631UNQ5961,[$__9]);
          var $m=
           new _A_($UHC.$Base.$mNEW3634UNQ5962,[$__,$__3,$__6,$n,$r]);
          var $__13=
           new _A_($UHC.$Base.$packedStringToInteger,["2"]);
          var $__14=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__15=
           new _A_($UHC.$Base.$_3a_25,[$__14,$__13]);
          var $__16=
           new _A_($UHC.$Base.$fromRational,[$__4,$__15]);
          var $__17=
           new _A_($UHC.$Base.$abs,[$__3,$r]);
          var $__18=
           new _A_($UHC.$Base.$_2d,[$__3,$__17,$__16]);
          var $__19=
           new _A_($UHC.$Base.$signum,[$__3,$__18]);
          var $__20=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__21=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__20]);
          var $__22=
           new _A_($UHC.$Base.$negate,[$__3,$__21]);
          var $__23=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$__5,$__22,$__19]));
          var $__swJSW588__0;
          switch($__23._tag_)
           {case 0:
             var $__24=
              new _A_($UHC.$Base.$packedStringToInteger,["0"]);
             var $__25=
              new _A_($UHC.$Base.$fromInteger,[$__3,$__24]);
             var $__26=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$__5,$__25,$__19]));
             var $__swJSW589__0;
             switch($__26._tag_)
              {case 0:
                var $__27=
                 new _A_($UHC.$Base.$packedStringToInteger,["1"]);
                var $__28=
                 new _A_($UHC.$Base.$fromInteger,[$__3,$__27]);
                var $__29=
                 _e_(new _A_($UHC.$Base.$_3d_3d,[$__5,$__28,$__19]));
                var $__swJSW590__0;
                switch($__29._tag_)
                 {case 0:
                   $__swJSW590__0=
                    $UHC.$Base.$undefined;
                   break;
                  case 1:
                   $__swJSW590__0=
                    $m;
                   break;}
                $__swJSW589__0=
                 $__swJSW590__0;
                break;
               case 1:
                var $__30=
                 new _A_($UHC.$Base.$even,[$__7,$n]);
                var $__31=
                 _e_($__30);
                var $__swJSW591__0;
                switch($__31._tag_)
                 {case 0:
                   $__swJSW591__0=
                    $m;
                   break;
                  case 1:
                   $__swJSW591__0=
                    $n;
                   break;}
                $__swJSW589__0=
                 $__swJSW591__0;
                break;}
             $__swJSW588__0=
              $__swJSW589__0;
             break;
            case 1:
             $__swJSW588__0=
              $n;
             break;}
          return $__swJSW588__0;});
$UHC.$Base.$__76__22018__2__27NEW3620UNQ5938=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Base.$__76__22169__2__0NEW3623UNQ5932=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Base.$RealFrac__CLS74__24__0DFLUHC_2eBase_2eround=
 new _F_(function($RealFrac__,$__,$__3,$__4,$__5,$__6)
         {var $__7=
           new _A_($UHC.$Base.$__76__22018__2__27NEW3620UNQ5938,[$__6]);
          var $__8=
           new _A_($UHC.$Base.$__76__22169__2__0NEW3623UNQ5932,[$__7]);
          return new _A_($UHC.$Base.$__78__7405__0,[$RealFrac__,$__,$__3,$__4,$__5,$__8,$__6]);});
$UHC.$Base.$__76__22494__2__0NEW3601UNQ5860=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Base.$__76__22018__2__39NEW3598UNQ5863=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Base.$rNEW3609UNQ5884=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Base.$nNEW3606UNQ5883=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Base.$__78__7369__0=
 new _F_(function($RealFrac__,$__,$__3,$__4,$__5,$x)
         {var $__7=
           new _A_($UHC.$Base.$properFraction,[$RealFrac__,$__5,$x]);
          var $n=
           new _A_($UHC.$Base.$nNEW3606UNQ5883,[$__7]);
          var $r=
           new _A_($UHC.$Base.$rNEW3609UNQ5884,[$__7]);
          var $__10=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__11=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__10]);
          var $__12=
           new _A_($UHC.$Base.$_3e,[$__,$r,$__11]);
          var $__13=
           _e_($__12);
          var $__swJSW598__0;
          switch($__13._tag_)
           {case 0:
             $__swJSW598__0=
              $n;
             break;
            case 1:
             var $__14=
              new _A_($UHC.$Base.$packedStringToInteger,["1"]);
             var $__15=
              new _A_($UHC.$Base.$fromInteger,[$__4,$__14]);
             var $__16=
              new _A_($UHC.$Base.$_2b,[$__4,$n,$__15]);
             $__swJSW598__0=
              $__16;
             break;}
          return $__swJSW598__0;});
$UHC.$Base.$RealFrac__CLS74__24__0DFLUHC_2eBase_2eceiling=
 new _F_(function($RealFrac__,$__,$__3,$__4)
         {var $__5=
           new _A_($UHC.$Base.$__76__22018__2__39NEW3598UNQ5863,[$__4]);
          var $__6=
           new _A_($UHC.$Base.$__76__22494__2__0NEW3601UNQ5860,[$__5]);
          return new _A_($UHC.$Base.$__78__7369__0,[$RealFrac__,$__,$__3,$__6,$__4]);});
$UHC.$Base.$mNEW3693UNQ5984=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Base.$RealFrac__CLS74__24__0DFLUHC_2eBase_2etruncate=
 new _F_(function($RealFrac__,$__,$x)
         {var $__4=
           new _A_($UHC.$Base.$properFraction,[$RealFrac__,$__,$x]);
          var $m=
           new _A_($UHC.$Base.$mNEW3693UNQ5984,[$__4]);
          return $m;});
$UHC.$Base.$__76__22018__2__16NEW3582UNQ5853=
 new _F_(function($RealFrac__)
         {var $Real__=
           _e_($RealFrac__);
          return $Real__._2;});
$UHC.$Base.$__76__22018__2__1NEW3588UNQ5820=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._2;});
$UHC.$Base.$__76__22018__2__2NEW3585UNQ5819=
 new _F_(function($RealFrac__)
         {var $Fractional__=
           _e_($RealFrac__);
          return $Fractional__._1;});
$UHC.$Base.$__76__22263__1__0NEW3591UNQ5821=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$UHC.$Base.$__76__22605__2__0NEW3673UNQ5896=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Base.$properFraction=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._5;});
$UHC.$Base.$nNEW3678UNQ5919=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Base.$rNEW3681UNQ5920=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Base.$__78__7493__0=
 new _F_(function($RealFrac__,$__,$__3,$__4,$__5,$x)
         {var $__7=
           new _A_($UHC.$Base.$properFraction,[$RealFrac__,$__4,$x]);
          var $n=
           new _A_($UHC.$Base.$nNEW3678UNQ5919,[$__7]);
          var $r=
           new _A_($UHC.$Base.$rNEW3681UNQ5920,[$__7]);
          var $__10=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__11=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__10]);
          var $__12=
           new _A_($UHC.$Base.$_3c,[$__,$r,$__11]);
          var $__13=
           _e_($__12);
          var $__swJSW608__0;
          switch($__13._tag_)
           {case 0:
             $__swJSW608__0=
              $n;
             break;
            case 1:
             var $__14=
              new _A_($UHC.$Base.$packedStringToInteger,["1"]);
             var $__15=
              new _A_($UHC.$Base.$fromInteger,[$__5,$__14]);
             var $__16=
              new _A_($UHC.$Base.$_2d,[$__5,$n,$__15]);
             $__swJSW608__0=
              $__16;
             break;}
          return $__swJSW608__0;});
$UHC.$Base.$__76__22018__2__10NEW3670UNQ5899=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Base.$RealFrac__CLS74__24__0DFLUHC_2eBase_2efloor=
 new _F_(function($RealFrac__,$__,$__3,$__4)
         {var $__5=
           new _A_($UHC.$Base.$__76__22018__2__10NEW3670UNQ5899,[$__4]);
          var $__6=
           new _A_($UHC.$Base.$__76__22605__2__0NEW3673UNQ5896,[$__5]);
          return new _A_($UHC.$Base.$__78__7493__0,[$RealFrac__,$__,$__3,$__4,$__6]);});
$UHC.$Base.$__76__22569__2__0NEW3594UNQ5826=
 new _F_(function($__)
         {var $Ord__=
           _e_($__);
          return $Ord__._2;});
$UHC.$Base.$RealFrac__CLS74__24__0=
 new _F_(function($RealFrac__)
         {var $__=
           new _A_($UHC.$Base.$__76__22018__2__16NEW3582UNQ5853,[$RealFrac__]);
          var $__3=
           new _A_($UHC.$Base.$__76__22018__2__2NEW3585UNQ5819,[$RealFrac__]);
          var $__4=
           new _A_($UHC.$Base.$__76__22018__2__1NEW3588UNQ5820,[$__3]);
          var $__5=
           new _A_($UHC.$Base.$__76__22263__1__0NEW3591UNQ5821,[$__4]);
          var $__6=
           new _A_($UHC.$Base.$__76__22569__2__0NEW3594UNQ5826,[$__]);
          var $__7=
           new _A_($UHC.$Base.$RealFrac__CLS74__24__0DFLUHC_2eBase_2etruncate,[$RealFrac__]);
          var $__8=
           new _A_($UHC.$Base.$RealFrac__CLS74__24__0DFLUHC_2eBase_2eround,[$RealFrac__,$__6,$__4,$__3,$__5]);
          var $__9=
           new _A_($UHC.$Base.$RealFrac__CLS74__24__0DFLUHC_2eBase_2efloor,[$RealFrac__,$__6,$__4]);
          var $__10=
           new _A_($UHC.$Base.$RealFrac__CLS74__24__0DFLUHC_2eBase_2eceiling,[$RealFrac__,$__6,$__4]);
          var $RealFrac__11=
           {_tag_:0,_1:$UHC.$Base.$undefined,_2:$UHC.$Base.$undefined,_3:$__10,_4:$__9,_5:$UHC.$Base.$undefined,_6:$__8,_7:$__7};
          return $RealFrac__11;});
$UHC.$Base.$__78__13247__0=
 new _F_(function($n,$m,$b,$__,$__5,$__6)
         {var $__7=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__8=
           new _A_($UHC.$Base.$fromInteger,[$__6,$__7]);
          var $__9=
           new _A_($UHC.$Base.$fromInteger,[$__5,$b]);
          var $__10=
           new _A_($UHC.$Base.$_5e,[$__5,$UHC.$Base.$Integral__DCT74__110__0,$__9,$n]);
          var $__11=
           new _A_($UHC.$Base.$fromInteger,[$__5,$m]);
          var $__12=
           new _A_($UHC.$Base.$_2a,[$__5,$__11,$__10]);
          return [$__12,$__8];});
$UHC.$Base.$__78__13234__0=
 new _F_(function($n,$w,$r,$__,$__5,$__6)
         {var $__7=
           new _A_($UHC.$Base.$encodeFloat,[$__,$r,$n]);
          var $__8=
           new _A_($UHC.$Base.$fromInteger,[$__5,$w]);
          return [$__8,$__7];});
$UHC.$Base.$rNEW6557UNQ6180=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Base.$wNEW6554UNQ6179=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Base.$nNEW6545UNQ6172=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Base.$mNEW6548UNQ6171=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Base.$floatProperFraction=
 new _F_(function($__,$x)
         {var $b=
           new _A_($UHC.$Base.$floatRadix,[$__,$x]);
          var $__4=
           new _A_($UHC.$Base.$decodeFloat,[$__,$x]);
          var $n=
           new _A_($UHC.$Base.$nNEW6545UNQ6172,[$__4]);
          var $m=
           new _A_($UHC.$Base.$mNEW6548UNQ6171,[$__4]);
          var $__7=
           new _A_($UHC.$Base.$negate,[$UHC.$Base.$Num__DCT74__101__0,$n]);
          var $__8=
           new _A_($UHC.$Base.$_5e,[$UHC.$Base.$Num__DCT74__134__0,$UHC.$Base.$Integral__DCT74__110__0,$b,$__7]);
          var $__9=
           new _A_($UHC.$Base.$quotRem,[$UHC.$Base.$Integral__DCT74__143__0,$m,$__8]);
          var $w=
           new _A_($UHC.$Base.$wNEW6554UNQ6179,[$__9]);
          var $r=
           new _A_($UHC.$Base.$rNEW6557UNQ6180,[$__9]);
          var $__12=
           new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__91__0,$n,0]);
          var $__13=
           _e_($__12);
          var $__swJSW615__0;
          switch($__13._tag_)
           {case 0:
             var $__14=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW616__0;
             switch($__14._tag_)
              {case 0:
                var $__15=
                 new _A_($UHC.$Base.$packedStringToString,["FAIL 75_388_0"]);
                var $__16=
                 new _A_($UHC.$Base.$error,[$__15]);
                $__swJSW616__0=
                 $__16;
                break;
               case 1:
                $__swJSW616__0=
                 new _A_($UHC.$Base.$__78__13234__0,[$n,$w,$r]);
                break;}
             $__swJSW615__0=
              $__swJSW616__0;
             break;
            case 1:
             $__swJSW615__0=
              new _A_($UHC.$Base.$__78__13247__0,[$n,$m,$b]);
             break;}
          return $__swJSW615__0;});
$UHC.$Base.$primAtan2Double=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primAtan2Double($__3,$__4);});
$UHC.$Base.$primIsIEEE=
 new _A_(new _F_(function()
                 {return primIsIEEE();}),[]);
$UHC.$Base.$__76__25210__2__0NEW3872UNQ6856=
 new _F_(function($__)
         {var $Ord__=
           _e_($__);
          return $Ord__._2;});
$UHC.$Base.$__76__17006__2__30NEW3869UNQ6857=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Base.$__76__25220__2__0NEW3875UNQ6854=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Base.$__76__25236__2__0NEW3866UNQ6849=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._2;});
$UHC.$Base.$__78__7876__0=
 new _F_(function($__,$__2,$__3,$__4,$__5,$x,$n)
         {var $__8=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__9=
           new _A_($UHC.$Base.$fromInteger,[$__5,$__8]);
          var $__10=
           new _A_($UHC.$Base.$_3e_3d,[$__2,$n,$__9]);
          var $__11=
           _e_($__10);
          var $__swJSW621__0;
          switch($__11._tag_)
           {case 0:
             var $__12=
              new _A_($UHC.$Base.$negate,[$__5,$n]);
             var $__13=
              new _A_($UHC.$Base.$_5e,[$__4,$__3,$x,$__12]);
             var $__14=
              new _A_($UHC.$Base.$recip,[$__,$__13]);
             $__swJSW621__0=
              $__14;
             break;
            case 1:
             var $__15=
              new _A_($UHC.$Base.$_5e,[$__4,$__3,$x,$n]);
             $__swJSW621__0=
              $__15;
             break;}
          return $__swJSW621__0;});
$UHC.$Base.$_5e_5e=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($UHC.$Base.$__76__25236__2__0NEW3866UNQ6849,[$__]);
          var $__4=
           new _A_($UHC.$Base.$__76__17006__2__30NEW3869UNQ6857,[$__2]);
          var $__5=
           new _A_($UHC.$Base.$__76__25210__2__0NEW3872UNQ6856,[$__4]);
          var $__6=
           new _A_($UHC.$Base.$__76__25220__2__0NEW3875UNQ6854,[$__4]);
          return new _A_($UHC.$Base.$__78__7876__0,[$__,$__5,$__2,$__3,$__6]);});
$UHC.$Base.$primRecipDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primRecipDouble($__2);});
$UHC.$Base.$primSqrtDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primSqrtDouble($__2);});
$UHC.$Base.$primCoshDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primCoshDouble($__2);});
$UHC.$Base.$__76__24052__10__0NEW3810UNQ6231=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$UHC.$Base.$__76__24052__13__0NEW3807UNQ6230=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Base.$__76__24023__2__0NEW3804UNQ6237=
 new _F_(function($__)
         {var $Ord__=
           _e_($__);
          return $Ord__._2;});
$UHC.$Base.$__76__17006__2__1NEW3801UNQ6229=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Base.$__76__17006__2__46NEW1668UNQ6133=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Base.$__76__17006__2__47NEW1665UNQ6132=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Base.$__78__3654__0=
 new _F_(function($__,$__2,$__3,$n)
         {var $__5=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__6=
           new _A_($UHC.$Base.$fromInteger,[$__,$__5]);
          var $__7=
           new _A_($UHC.$Base.$packedStringToInteger,["2"]);
          var $__8=
           new _A_($UHC.$Base.$fromInteger,[$__,$__7]);
          var $__9=
           new _A_($UHC.$Base.$rem,[$__3,$n,$__8]);
          return new _A_($UHC.$Base.$_3d_3d,[$__2,$__9,$__6]);});
$UHC.$Base.$__76__23253__2__0NEW1671UNQ6126=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$UHC.$Base.$even=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$__76__17006__2__47NEW1665UNQ6132,[$__]);
          var $__3=
           new _A_($UHC.$Base.$__76__17006__2__46NEW1668UNQ6133,[$__2]);
          var $__4=
           new _A_($UHC.$Base.$__76__23253__2__0NEW1671UNQ6126,[$__3]);
          return new _A_($UHC.$Base.$__78__3654__0,[$__3,$__4,$__]);});
$UHC.$Base.$gUNQ6333=
 new _F_(function($__,$__2,$__3,$__4,$x3,$x,$n)
         {var $__8=
           new _A_($UHC.$Base.$even,[$__2,$n]);
          var $__9=
           _e_($__8);
          var $__swJSW629__0;
          switch($__9._tag_)
           {case 0:
             var $__10=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW630__0;
             switch($__10._tag_)
              {case 0:
                var $__11=
                 new _A_($UHC.$Base.$packedStringToString,["FAIL 75_113_0"]);
                var $__12=
                 new _A_($UHC.$Base.$error,[$__11]);
                $__swJSW630__0=
                 $__12;
                break;
               case 1:
                var $__13=
                 new _A_($UHC.$Base.$_2a,[$__,$x,$x3]);
                var $__14=
                 new _A_($UHC.$Base.$packedStringToInteger,["1"]);
                var $__15=
                 new _A_($UHC.$Base.$fromInteger,[$__4,$__14]);
                var $__16=
                 new _A_($UHC.$Base.$_2d,[$__4,$n,$__15]);
                var $__17=
                 new _A_($UHC.$Base.$fUNQ6293,[$__,$x,$__2,$__3,$__4,$__16,$__13]);
                $__swJSW630__0=
                 $__17;
                break;}
             $__swJSW629__0=
              $__swJSW630__0;
             break;
            case 1:
             var $__18=
              new _A_($UHC.$Base.$packedStringToInteger,["2"]);
             var $__19=
              new _A_($UHC.$Base.$fromInteger,[$__4,$__18]);
             var $__20=
              new _A_($UHC.$Base.$quot,[$__2,$n,$__19]);
             var $__21=
              new _A_($UHC.$Base.$_2a,[$__,$x,$x]);
             var $__22=
              new _A_($UHC.$Base.$gUNQ6333,[$__,$__2,$__3,$__4,$x3,$__21,$__20]);
             $__swJSW629__0=
              $__22;
             break;}
          return $__swJSW629__0;});
$UHC.$Base.$__76__23788__0NEW3825UNQ6329CCN=
 new _F_(function($__,$x1,$__3,$__4,$__5,$x2,$x3)
         {return new _A_($UHC.$Base.$gUNQ6333,[$__,$__3,$__4,$__5,$x3,$x1,$x2]);});
$UHC.$Base.$fUNQ6293=
 new _F_(function($__,$x1,$__3,$__4,$__5,$x2,$x3)
         {var $__8=
           new _A_($UHC.$Base.$__76__23788__0NEW3825UNQ6329CCN,[$__,$x1,$__3,$__4,$__5,$x2,$x3]);
          var $__9=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__10=
           new _A_($UHC.$Base.$fromInteger,[$__5,$__9]);
          var $x211=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$__4,$__10,$x2]));
          var $__swJSW631__0;
          switch($x211._tag_)
           {case 0:
             $__swJSW631__0=
              $__8;
             break;
            case 1:
             $__swJSW631__0=
              $x3;
             break;}
          return $__swJSW631__0;});
$UHC.$Base.$xNEW3814UNQ6289CCN=
 new _F_(function($__,$__2,$__3,$__4,$__5,$x1,$x2)
         {var $__8=
           new _A_($UHC.$Base.$packedStringToString,["Prelude.^: negative exponent"]);
          var $x=
           new _A_($UHC.$Base.$error,[$__8]);
          var $__10=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__11=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__10]);
          var $__12=
           new _A_($UHC.$Base.$_3e,[$__5,$x2,$__11]);
          var $__13=
           _e_($__12);
          var $__swJSW632__0;
          switch($__13._tag_)
           {case 0:
             $__swJSW632__0=
              $x;
             break;
            case 1:
             var $__14=
              new _A_($UHC.$Base.$packedStringToInteger,["1"]);
             var $__15=
              new _A_($UHC.$Base.$fromInteger,[$__3,$__14]);
             var $__16=
              new _A_($UHC.$Base.$_2d,[$__3,$x2,$__15]);
             var $__17=
              new _A_($UHC.$Base.$fUNQ6293,[$__,$x1,$__4,$__2,$__3,$__16,$x1]);
             $__swJSW632__0=
              $__17;
             break;}
          return $__swJSW632__0;});
$UHC.$Base.$__78__7751__0=
 new _F_(function($__,$__2,$__3,$__4,$__5,$x1,$x2)
         {var $x=
           new _A_($UHC.$Base.$xNEW3814UNQ6289CCN,[$__,$__2,$__3,$__4,$__5,$x1,$x2]);
          var $__9=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__10=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__9]);
          var $x211=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$__2,$__10,$x2]));
          var $__swJSW633__0;
          switch($x211._tag_)
           {case 0:
             $__swJSW633__0=
              $x;
             break;
            case 1:
             var $__12=
              new _A_($UHC.$Base.$packedStringToInteger,["1"]);
             var $__13=
              new _A_($UHC.$Base.$fromInteger,[$__,$__12]);
             $__swJSW633__0=
              $__13;
             break;}
          return $__swJSW633__0;});
$UHC.$Base.$_5e=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($UHC.$Base.$__76__17006__2__1NEW3801UNQ6229,[$__2]);
          var $__4=
           new _A_($UHC.$Base.$__76__24023__2__0NEW3804UNQ6237,[$__3]);
          var $__5=
           new _A_($UHC.$Base.$__76__24052__13__0NEW3807UNQ6230,[$__3]);
          var $__6=
           new _A_($UHC.$Base.$__76__24052__10__0NEW3810UNQ6231,[$__5]);
          return new _A_($UHC.$Base.$__78__7751__0,[$__,$__6,$__5,$__2,$__4]);});
$UHC.$Base.$primMinExpDouble=
 new _A_(new _F_(function()
                 {return primMinExpDouble();}),[]);
$UHC.$Base.$primAtanDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primAtanDouble($__2);});
$UHC.$Base.$primMaxExpDouble=
 new _A_(new _F_(function()
                 {return primMaxExpDouble();}),[]);
$UHC.$Base.$mNEW5823UNQ5650=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Base.$nNEW5820UNQ5651=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Base.$RealFloat__CLS74__30__0DFLUHC_2eBase_2eexponent=
 new _F_(function($RealFloat__,$x)
         {var $__=
           new _A_($UHC.$Base.$decodeFloat,[$RealFloat__,$x]);
          var $n=
           new _A_($UHC.$Base.$nNEW5820UNQ5651,[$__]);
          var $m=
           new _A_($UHC.$Base.$mNEW5823UNQ5650,[$__]);
          var $__6=
           new _A_($UHC.$Base.$primIntToInteger,[0]);
          var $__7=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__130__0,$m,$__6]);
          var $__8=
           _e_($__7);
          var $__swJSW636__0;
          switch($__8._tag_)
           {case 0:
             var $__9=
              new _A_($UHC.$Base.$floatDigits,[$RealFloat__,$x]);
             var $__10=
              new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$n,$__9]);
             $__swJSW636__0=
              $__10;
             break;
            case 1:
             $__swJSW636__0=
              0;
             break;}
          return $__swJSW636__0;});
$UHC.$Base.$nNEW5811UNQ5673=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Base.$mNEW5814UNQ5672=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Base.$RealFloat__CLS74__30__0DFLUHC_2eBase_2escaleFloat=
 new _F_(function($RealFloat__,$k,$x)
         {var $__=
           new _A_($UHC.$Base.$decodeFloat,[$RealFloat__,$x]);
          var $n=
           new _A_($UHC.$Base.$nNEW5811UNQ5673,[$__]);
          var $m=
           new _A_($UHC.$Base.$mNEW5814UNQ5672,[$__]);
          var $__7=
           new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$n,$k]);
          return new _A_($UHC.$Base.$encodeFloat,[$RealFloat__,$m,$__7]);});
$UHC.$Base.$__76__21210__2__0NEW5850UNQ5514=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._2;});
$UHC.$Base.$__76__21020__1__0NEW5856UNQ5511=
 new _F_(function($RealFloat__)
         {var $Floating__=
           _e_($RealFloat__);
          return $Floating__._1;});
$UHC.$Base.$__76__20825__2__5NEW5831UNQ5619=
 new _F_(function($RealFloat__)
         {var $RealFrac__=
           _e_($RealFloat__);
          return $RealFrac__._2;});
$UHC.$Base.$__76__20825__2__4NEW5834UNQ5507=
 new _F_(function($__)
         {var $Fractional__=
           _e_($__);
          return $Fractional__._1;});
$UHC.$Base.$__76__20825__2__7NEW5837UNQ5617=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Base.$encodeFloat=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._5;});
$UHC.$Base.$floatDigits=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._7;});
$UHC.$Base.$mNEW5845UNQ5691=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Base.$RealFloat__CLS74__30__0DFLUHC_2eBase_2esignificand=
 new _F_(function($RealFloat__,$x)
         {var $__=
           new _A_($UHC.$Base.$decodeFloat,[$RealFloat__,$x]);
          var $m=
           new _A_($UHC.$Base.$mNEW5845UNQ5691,[$__]);
          var $__5=
           new _A_($UHC.$Base.$floatDigits,[$RealFloat__,$x]);
          var $__6=
           new _A_($UHC.$Base.$negate,[$UHC.$Base.$Num__DCT74__101__0,$__5]);
          return new _A_($UHC.$Base.$encodeFloat,[$RealFloat__,$m,$__6]);});
$UHC.$Base.$__76__21200__2__0NEW5840UNQ5515=
 new _F_(function($__)
         {var $Ord__=
           _e_($__);
          return $Ord__._2;});
$UHC.$Base.$__76__21455__2__0NEW5853UNQ5531=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$UHC.$Base.$_7c_7c=
 new _F_(function($x1,$x2)
         {var $x13=
           _e_($x1);
          var $__swJSW649__0;
          switch($x13._tag_)
           {case 0:
             $__swJSW649__0=
              $x2;
             break;
            case 1:
             $__swJSW649__0=
              $UHC.$Base.$True__;
             break;}
          return $__swJSW649__0;});
$UHC.$Base.$pi=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._14;});
$UHC.$Base.$_3c=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$UHC.$Base.$isNegativeZero=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._14;});
$UHC.$Base.$atan2=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._3;});
$UHC.$Base.$RealFloat__CLS74__30__0DFLUHC_2eBase_2eatan2=
 new _F_(function($__,$__2,$__3,$RealFloat__,$__5,$__6,$y,$x)
         {var $__9=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__10=
           new _A_($UHC.$Base.$fromInteger,[$__2,$__9]);
          var $__11=
           new _A_($UHC.$Base.$_3e,[$__5,$x,$__10]);
          var $__12=
           _e_($__11);
          var $__swJSW654__0;
          switch($__12._tag_)
           {case 0:
             var $__13=
              new _A_($UHC.$Base.$packedStringToInteger,["0"]);
             var $__14=
              new _A_($UHC.$Base.$fromInteger,[$__2,$__13]);
             var $__15=
              new _A_($UHC.$Base.$_3e,[$__5,$y,$__14]);
             var $__16=
              new _A_($UHC.$Base.$packedStringToInteger,["0"]);
             var $__17=
              new _A_($UHC.$Base.$fromInteger,[$__2,$__16]);
             var $__18=
              new _A_($UHC.$Base.$_3d_3d,[$__6,$x,$__17]);
             var $__19=
              new _A_($UHC.$Base.$_26_26,[$__18,$__15]);
             var $__20=
              _e_($__19);
             var $__swJSW655__0;
             switch($__20._tag_)
              {case 0:
                var $__21=
                 new _A_($UHC.$Base.$packedStringToInteger,["0"]);
                var $__22=
                 new _A_($UHC.$Base.$fromInteger,[$__2,$__21]);
                var $__23=
                 new _A_($UHC.$Base.$_3e,[$__5,$y,$__22]);
                var $__24=
                 new _A_($UHC.$Base.$packedStringToInteger,["0"]);
                var $__25=
                 new _A_($UHC.$Base.$fromInteger,[$__2,$__24]);
                var $__26=
                 new _A_($UHC.$Base.$_3c,[$__5,$x,$__25]);
                var $__27=
                 new _A_($UHC.$Base.$_26_26,[$__26,$__23]);
                var $__28=
                 _e_($__27);
                var $__swJSW656__0;
                switch($__28._tag_)
                 {case 0:
                   var $__29=
                    new _A_($UHC.$Base.$isNegativeZero,[$RealFloat__,$y]);
                   var $__30=
                    new _A_($UHC.$Base.$isNegativeZero,[$RealFloat__,$x]);
                   var $__31=
                    new _A_($UHC.$Base.$_26_26,[$__30,$__29]);
                   var $__32=
                    new _A_($UHC.$Base.$isNegativeZero,[$RealFloat__,$y]);
                   var $__33=
                    new _A_($UHC.$Base.$packedStringToInteger,["0"]);
                   var $__34=
                    new _A_($UHC.$Base.$fromInteger,[$__2,$__33]);
                   var $__35=
                    new _A_($UHC.$Base.$_3c,[$__5,$x,$__34]);
                   var $__36=
                    new _A_($UHC.$Base.$_26_26,[$__35,$__32]);
                   var $__37=
                    new _A_($UHC.$Base.$_7c_7c,[$__36,$__31]);
                   var $__38=
                    new _A_($UHC.$Base.$packedStringToInteger,["0"]);
                   var $__39=
                    new _A_($UHC.$Base.$fromInteger,[$__2,$__38]);
                   var $__40=
                    new _A_($UHC.$Base.$_3c,[$__5,$y,$__39]);
                   var $__41=
                    new _A_($UHC.$Base.$packedStringToInteger,["0"]);
                   var $__42=
                    new _A_($UHC.$Base.$fromInteger,[$__2,$__41]);
                   var $__43=
                    new _A_($UHC.$Base.$_3c_3d,[$__5,$x,$__42]);
                   var $__44=
                    new _A_($UHC.$Base.$_26_26,[$__43,$__40]);
                   var $__45=
                    new _A_($UHC.$Base.$_7c_7c,[$__44,$__37]);
                   var $__46=
                    _e_($__45);
                   var $__swJSW657__0;
                   switch($__46._tag_)
                    {case 0:
                      var $__47=
                       new _A_($UHC.$Base.$isNegativeZero,[$RealFloat__,$x]);
                      var $__48=
                       new _A_($UHC.$Base.$packedStringToInteger,["0"]);
                      var $__49=
                       new _A_($UHC.$Base.$fromInteger,[$__2,$__48]);
                      var $__50=
                       new _A_($UHC.$Base.$_3c,[$__5,$x,$__49]);
                      var $__51=
                       new _A_($UHC.$Base.$_7c_7c,[$__50,$__47]);
                      var $__52=
                       new _A_($UHC.$Base.$packedStringToInteger,["0"]);
                      var $__53=
                       new _A_($UHC.$Base.$fromInteger,[$__2,$__52]);
                      var $__54=
                       new _A_($UHC.$Base.$_3d_3d,[$__6,$y,$__53]);
                      var $__55=
                       new _A_($UHC.$Base.$_26_26,[$__54,$__51]);
                      var $__56=
                       _e_($__55);
                      var $__swJSW658__0;
                      switch($__56._tag_)
                       {case 0:
                         var $__57=
                          new _A_($UHC.$Base.$packedStringToInteger,["0"]);
                         var $__58=
                          new _A_($UHC.$Base.$fromInteger,[$__2,$__57]);
                         var $__59=
                          new _A_($UHC.$Base.$_3d_3d,[$__6,$y,$__58]);
                         var $__60=
                          new _A_($UHC.$Base.$packedStringToInteger,["0"]);
                         var $__61=
                          new _A_($UHC.$Base.$fromInteger,[$__2,$__60]);
                         var $__62=
                          new _A_($UHC.$Base.$_3d_3d,[$__6,$x,$__61]);
                         var $__63=
                          new _A_($UHC.$Base.$_26_26,[$__62,$__59]);
                         var $__64=
                          _e_($__63);
                         var $__swJSW659__0;
                         switch($__64._tag_)
                          {case 0:
                            var $__65=
                             _e_($UHC.$Base.$otherwise);
                            var $__swJSW660__0;
                            switch($__65._tag_)
                             {case 0:
                               var $__66=
                                new _A_($UHC.$Base.$packedStringToString,["FAIL 75_98_0"]);
                               var $__67=
                                new _A_($UHC.$Base.$error,[$__66]);
                               $__swJSW660__0=
                                $__67;
                               break;
                              case 1:
                               var $__68=
                                new _A_($UHC.$Base.$_2b,[$__2,$x,$y]);
                               $__swJSW660__0=
                                $__68;
                               break;}
                            $__swJSW659__0=
                             $__swJSW660__0;
                            break;
                           case 1:
                            $__swJSW659__0=
                             $y;
                            break;}
                         $__swJSW658__0=
                          $__swJSW659__0;
                         break;
                        case 1:
                         var $__69=
                          new _A_($UHC.$Base.$pi,[$__]);
                         $__swJSW658__0=
                          $__69;
                         break;}
                      $__swJSW657__0=
                       $__swJSW658__0;
                      break;
                     case 1:
                      var $__70=
                       new _A_($UHC.$Base.$negate,[$__2,$y]);
                      var $__71=
                       new _A_($UHC.$Base.$atan2,[$RealFloat__,$__70,$x]);
                      var $__72=
                       new _A_($UHC.$Base.$negate,[$__2,$__71]);
                      $__swJSW657__0=
                       $__72;
                      break;}
                   $__swJSW656__0=
                    $__swJSW657__0;
                   break;
                  case 1:
                   var $__73=
                    new _A_($UHC.$Base.$_2f,[$__3,$y,$x]);
                   var $__74=
                    new _A_($UHC.$Base.$atan,[$__,$__73]);
                   var $__75=
                    new _A_($UHC.$Base.$pi,[$__]);
                   var $__76=
                    new _A_($UHC.$Base.$_2b,[$__2,$__75,$__74]);
                   $__swJSW656__0=
                    $__76;
                   break;}
                $__swJSW655__0=
                 $__swJSW656__0;
                break;
               case 1:
                var $__77=
                 new _A_($UHC.$Base.$packedStringToInteger,["2"]);
                var $__78=
                 new _A_($UHC.$Base.$fromInteger,[$__2,$__77]);
                var $__79=
                 new _A_($UHC.$Base.$pi,[$__]);
                var $__80=
                 new _A_($UHC.$Base.$_2f,[$__3,$__79,$__78]);
                $__swJSW655__0=
                 $__80;
                break;}
             $__swJSW654__0=
              $__swJSW655__0;
             break;
            case 1:
             var $__81=
              new _A_($UHC.$Base.$_2f,[$__3,$y,$x]);
             var $__82=
              new _A_($UHC.$Base.$atan,[$__,$__81]);
             $__swJSW654__0=
              $__82;
             break;}
          return $__swJSW654__0;});
$UHC.$Base.$RealFloat__CLS74__30__0=
 new _F_(function($RealFloat__)
         {var $__=
           new _A_($UHC.$Base.$__76__20825__2__5NEW5831UNQ5619,[$RealFloat__]);
          var $__3=
           new _A_($UHC.$Base.$__76__20825__2__4NEW5834UNQ5507,[$__]);
          var $__4=
           new _A_($UHC.$Base.$__76__20825__2__7NEW5837UNQ5617,[$__]);
          var $__5=
           new _A_($UHC.$Base.$__76__21200__2__0NEW5840UNQ5515,[$__4]);
          var $__6=
           new _A_($UHC.$Base.$__76__21210__2__0NEW5850UNQ5514,[$__3]);
          var $__7=
           new _A_($UHC.$Base.$__76__21455__2__0NEW5853UNQ5531,[$__6]);
          var $__8=
           new _A_($UHC.$Base.$__76__21020__1__0NEW5856UNQ5511,[$RealFloat__]);
          var $__9=
           new _A_($UHC.$Base.$RealFloat__CLS74__30__0DFLUHC_2eBase_2esignificand,[$RealFloat__]);
          var $__10=
           new _A_($UHC.$Base.$RealFloat__CLS74__30__0DFLUHC_2eBase_2escaleFloat,[$RealFloat__]);
          var $__11=
           new _A_($UHC.$Base.$RealFloat__CLS74__30__0DFLUHC_2eBase_2eexponent,[$RealFloat__]);
          var $__12=
           new _A_($UHC.$Base.$RealFloat__CLS74__30__0DFLUHC_2eBase_2eatan2,[$__8,$__6,$__3,$RealFloat__,$__5,$__7]);
          var $RealFloat__13=
           {_tag_:0,_1:$UHC.$Base.$undefined,_2:$UHC.$Base.$undefined,_3:$__12,_4:$UHC.$Base.$undefined,_5:$UHC.$Base.$undefined,_6:$__11,_7:$UHC.$Base.$undefined,_8:$UHC.$Base.$undefined,_9:$UHC.$Base.$undefined,_10:$UHC.$Base.$undefined,_11:$UHC.$Base.$undefined,_12:$UHC.$Base.$undefined,_13:$UHC.$Base.$undefined,_14:$UHC.$Base.$undefined,_15:$__10,_16:$__9};
          return $RealFloat__13;});
$UHC.$Base.$primEncodeDouble=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primEncodeDouble($__3,$__4);});
$UHC.$Base.$__76__25108__2__0NEW3511UNQ6736=
 new _F_(function($Floating__)
         {var $Fractional__=
           _e_($Floating__);
          return $Fractional__._2;});
$UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2eatanh=
 new _F_(function($__,$__2,$Floating__,$x)
         {var $__5=
           new _A_($UHC.$Base.$packedStringToInteger,["2"]);
          var $__6=
           new _A_($UHC.$Base.$fromInteger,[$__2,$__5]);
          var $__7=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__8=
           new _A_($UHC.$Base.$fromInteger,[$__2,$__7]);
          var $__9=
           new _A_($UHC.$Base.$_2d,[$__2,$__8,$x]);
          var $__10=
           new _A_($UHC.$Base.$log,[$Floating__,$__9]);
          var $__11=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__12=
           new _A_($UHC.$Base.$fromInteger,[$__2,$__11]);
          var $__13=
           new _A_($UHC.$Base.$_2b,[$__2,$__12,$x]);
          var $__14=
           new _A_($UHC.$Base.$log,[$Floating__,$__13]);
          var $__15=
           new _A_($UHC.$Base.$_2d,[$__2,$__14,$__10]);
          return new _A_($UHC.$Base.$_2f,[$__,$__15,$__6]);});
$UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2e_2a_2a=
 new _F_(function($__,$Floating__,$x,$y)
         {var $__5=
           new _A_($UHC.$Base.$log,[$Floating__,$x]);
          var $__6=
           new _A_($UHC.$Base.$_2a,[$__,$__5,$y]);
          return new _A_($UHC.$Base.$exp,[$Floating__,$__6]);});
$UHC.$Base.$cos=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._9;});
$UHC.$Base.$sin=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._15;});
$UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2etan=
 new _F_(function($__,$Floating__,$x)
         {var $__4=
           new _A_($UHC.$Base.$cos,[$Floating__,$x]);
          var $__5=
           new _A_($UHC.$Base.$sin,[$Floating__,$x]);
          return new _A_($UHC.$Base.$_2f,[$__,$__5,$__4]);});
$UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2eacosh=
 new _F_(function($__,$Floating__,$x)
         {var $__4=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__5=
           new _A_($UHC.$Base.$fromInteger,[$__,$__4]);
          var $__6=
           new _A_($UHC.$Base.$_2a,[$__,$x,$x]);
          var $__7=
           new _A_($UHC.$Base.$_2d,[$__,$__6,$__5]);
          var $__8=
           new _A_($UHC.$Base.$sqrt,[$Floating__,$__7]);
          var $__9=
           new _A_($UHC.$Base.$_2b,[$__,$x,$__8]);
          return new _A_($UHC.$Base.$log,[$Floating__,$__9]);});
$UHC.$Base.$atan=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._7;});
$UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2ecosh=
 new _F_(function($__,$__2,$Floating__,$x)
         {var $__5=
           new _A_($UHC.$Base.$packedStringToInteger,["2"]);
          var $__6=
           new _A_($UHC.$Base.$fromInteger,[$__2,$__5]);
          var $__7=
           new _A_($UHC.$Base.$negate,[$__2,$x]);
          var $__8=
           new _A_($UHC.$Base.$exp,[$Floating__,$__7]);
          var $__9=
           new _A_($UHC.$Base.$exp,[$Floating__,$x]);
          var $__10=
           new _A_($UHC.$Base.$_2b,[$__2,$__9,$__8]);
          return new _A_($UHC.$Base.$_2f,[$__,$__10,$__6]);});
$UHC.$Base.$fromRational=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._4;});
$UHC.$Base.$_2a_2a=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2esqrt=
 new _F_(function($__,$Floating__,$x)
         {var $__4=
           new _A_($UHC.$Base.$packedStringToInteger,["2"]);
          var $__5=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__6=
           new _A_($UHC.$Base.$_3a_25,[$__5,$__4]);
          var $__7=
           new _A_($UHC.$Base.$fromRational,[$__,$__6]);
          return new _A_($UHC.$Base.$_2a_2a,[$Floating__,$x,$__7]);});
$UHC.$Base.$exp=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._11;});
$UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2esinh=
 new _F_(function($__,$__2,$Floating__,$x)
         {var $__5=
           new _A_($UHC.$Base.$packedStringToInteger,["2"]);
          var $__6=
           new _A_($UHC.$Base.$fromInteger,[$__2,$__5]);
          var $__7=
           new _A_($UHC.$Base.$negate,[$__2,$x]);
          var $__8=
           new _A_($UHC.$Base.$exp,[$Floating__,$__7]);
          var $__9=
           new _A_($UHC.$Base.$exp,[$Floating__,$x]);
          var $__10=
           new _A_($UHC.$Base.$_2d,[$__2,$__9,$__8]);
          return new _A_($UHC.$Base.$_2f,[$__,$__10,$__6]);});
$UHC.$Base.$cosh=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._10;});
$UHC.$Base.$sinh=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._16;});
$UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2etanh=
 new _F_(function($__,$Floating__,$x)
         {var $__4=
           new _A_($UHC.$Base.$cosh,[$Floating__,$x]);
          var $__5=
           new _A_($UHC.$Base.$sinh,[$Floating__,$x]);
          return new _A_($UHC.$Base.$_2f,[$__,$__5,$__4]);});
$UHC.$Base.$__76__24790__2__0NEW3514UNQ6738=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._2;});
$UHC.$Base.$sqrt=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._17;});
$UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2easinh=
 new _F_(function($__,$Floating__,$x)
         {var $__4=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__5=
           new _A_($UHC.$Base.$fromInteger,[$__,$__4]);
          var $__6=
           new _A_($UHC.$Base.$_2a,[$__,$x,$x]);
          var $__7=
           new _A_($UHC.$Base.$_2b,[$__,$__6,$__5]);
          var $__8=
           new _A_($UHC.$Base.$sqrt,[$Floating__,$__7]);
          var $__9=
           new _A_($UHC.$Base.$_2b,[$__,$x,$__8]);
          return new _A_($UHC.$Base.$log,[$Floating__,$__9]);});
$UHC.$Base.$_2f=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$UHC.$Base.$log=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._12;});
$UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2elogBase=
 new _F_(function($__,$Floating__,$x,$y)
         {var $__5=
           new _A_($UHC.$Base.$log,[$Floating__,$x]);
          var $__6=
           new _A_($UHC.$Base.$log,[$Floating__,$y]);
          return new _A_($UHC.$Base.$_2f,[$__,$__6,$__5]);});
$UHC.$Base.$Floating__CLS74__22__0=
 new _F_(function($Floating__)
         {var $__=
           new _A_($UHC.$Base.$__76__25108__2__0NEW3511UNQ6736,[$Floating__]);
          var $__3=
           new _A_($UHC.$Base.$__76__24790__2__0NEW3514UNQ6738,[$__]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__5=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__4]);
          var $__6=
           new _A_($UHC.$Base.$atan,[$Floating__,$__5]);
          var $__7=
           new _A_($UHC.$Base.$packedStringToInteger,["4"]);
          var $__8=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__7]);
          var $Floating__CLS74__22__0DFLUHC_2eBase_2epi=
           new _A_($UHC.$Base.$_2a,[$__3,$__8,$__6]);
          var $__9=
           new _A_($UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2etanh,[$__,$Floating__]);
          var $__10=
           new _A_($UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2etan,[$__,$Floating__]);
          var $__11=
           new _A_($UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2esqrt,[$__,$Floating__]);
          var $__12=
           new _A_($UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2esinh,[$__,$__3,$Floating__]);
          var $__13=
           new _A_($UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2elogBase,[$__,$Floating__]);
          var $__14=
           new _A_($UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2ecosh,[$__,$__3,$Floating__]);
          var $__15=
           new _A_($UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2eatanh,[$__,$__3,$Floating__]);
          var $__16=
           new _A_($UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2easinh,[$__3,$Floating__]);
          var $__17=
           new _A_($UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2eacosh,[$__3,$Floating__]);
          var $__18=
           new _A_($UHC.$Base.$Floating__CLS74__22__0DFLUHC_2eBase_2e_2a_2a,[$__3,$Floating__]);
          var $Floating__19=
           {_tag_:0,_1:$__18,_2:$UHC.$Base.$undefined,_3:$UHC.$Base.$undefined,_4:$__17,_5:$UHC.$Base.$undefined,_6:$__16,_7:$UHC.$Base.$undefined,_8:$__15,_9:$UHC.$Base.$undefined,_10:$__14,_11:$UHC.$Base.$undefined,_12:$UHC.$Base.$undefined,_13:$__13,_14:$Floating__CLS74__22__0DFLUHC_2eBase_2epi,_15:$UHC.$Base.$undefined,_16:$__12,_17:$__11,_18:$__10,_19:$__9};
          return $Floating__19;});
$UHC.$Base.$floatRadix=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._8;});
$UHC.$Base.$primSinhDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primSinhDouble($__2);});
$UHC.$Base.$primIsDenormalizedDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primIsDenormalizedDouble($__2);});
$UHC.$Base.$primIsNaNDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primIsNaNDouble($__2);});
$UHC.$Base.$__76__20479__2__0=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Num__DCT74__271__0,[$UHC.$Base.$Integral__DCT74__143__0]);}),[]);
$UHC.$Base.$recip=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._5;});
$UHC.$Base.$primTanDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primTanDouble($__2);});
$UHC.$Base.$primAcosDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primAcosDouble($__2);});
$UHC.$Base.$primAsinDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primAsinDouble($__2);});
$UHC.$Base.$decodeFloat=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._4;});
$UHC.$Base.$primTanhDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primTanhDouble($__2);});
$UHC.$Base.$primRadixDoubleFloat=
 new _A_(new _F_(function()
                 {return primRadixDoubleFloat();}),[]);
$UHC.$Base.$primDecodeDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primDecodeDouble($__2);});
$UHC.$Base.$primRationalToDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primRationalToDouble($__2);});
$UHC.$Base.$primIsNegativeZeroDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primIsNegativeZeroDouble($__2);});
$UHC.$Base.$primExpDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primExpDouble($__2);});
$UHC.$Base.$primDigitsDouble=
 new _A_(new _F_(function()
                 {return primDigitsDouble();}),[]);
$UHC.$Base.$primSinDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primSinDouble($__2);});
$UHC.$Base.$primDivideDouble=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primDivideDouble($__3,$__4);});
$UHC.$Base.$round=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._6;});
$UHC.$Base.$fromDouble=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._3;});
$UHC.$Base.$__76__45168__2__3NEW5484UNQ10489=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Base.$__76__45168__2__2NEW5487UNQ10490=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Base.$__76__45168__2__1NEW5490UNQ10491=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$UHC.$Base.$__Rep0RatioDFLUHC_2eBase_2eto0GENRepresentable0=
 new _F_(function($proj__1)
         {var $proj__3=
           _e_($proj__1);
          var $__=
           new _A_($UHC.$Base.$_3a_25,[$proj__3._1,$proj__3._2]);
          return $__;});
$UHC.$Base.$K1__=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$id;}),[]);
$UHC.$Base.$_3a_2a_3a=
 new _F_(function($x1,$x2)
         {return {_tag_:0,_1:$x1,_2:$x2};});
$UHC.$Base.$__Rep0RatioDFLUHC_2eBase_2efrom0GENRepresentable0=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          var $__5=
           new _A_($UHC.$Base.$K1__,[$x2._2]);
          var $__6=
           new _A_($UHC.$Base.$M1__,[$__5]);
          var $__7=
           new _A_($UHC.$Base.$K1__,[$x2._1]);
          var $__8=
           new _A_($UHC.$Base.$M1__,[$__7]);
          var $__9=
           new _A_($UHC.$Base.$_3a_2a_3a,[$__8,$__6]);
          var $__10=
           new _A_($UHC.$Base.$M1__,[$__9]);
          var $__11=
           new _A_($UHC.$Base.$M1__,[$__10]);
          return $__11;});
$UHC.$Base.$__Rep0RatioNEW1480UNQ2425EVLSDCGENRepresentable0=
 new _F_(function($__)
         {var $Representable0__=
           _e_(new _A_($UHC.$Base.$Representable0__CLS74__369__0,[$__]));
          var $__5=
           {_tag_:0,_1:$UHC.$Base.$__Rep0RatioDFLUHC_2eBase_2efrom0GENRepresentable0,_2:$UHC.$Base.$__Rep0RatioDFLUHC_2eBase_2eto0GENRepresentable0};
          return $__5;});
$UHC.$Base.$__Rep0RatioNEW1478UNQ2424SDCGENRepresentable0=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$__Rep0RatioNEW1480UNQ2425EVLSDCGENRepresentable0,[$__]);
          return $__2;});
$UHC.$Base.$__Rep0RatioUNQ2424SDCGENRepresentable0=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$__Rep0RatioNEW1478UNQ2424SDCGENRepresentable0,[$UHC.$Base.$__Rep0RatioUNQ2424SDCGENRepresentable0]);}),[]);
$UHC.$Base.$__Rep0RatioGENRepresentable0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$__Rep0RatioUNQ2424SDCGENRepresentable0;}),[]);
$UHC.$Base.$__74__268__0NEW2904UNQ10047EVLRDC=
 new _F_(function($__,$__2)
         {var $Eq__=
           _e_(new _A_($UHC.$Base.$Eq__CLS74__4__0,[$__]));
          var $__6=
           {_tag_:0,_1:$Eq__._1,_2:$__2};
          return $__6;});
$UHC.$Base.$__74__268__0NEW2901UNQ10038RDC=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($UHC.$Base.$__74__268__0NEW2904UNQ10047EVLRDC,[$__,$__2]);
          return $__3;});
$UHC.$Base.$Eq_27__DCT74__390__0DFLUHC_2eBase_2egeq_27=
 new _F_(function($__,$__2,$__3)
         {return new _A_($UHC.$Base.$_3d_3d,[$__,$__2,$__3]);});
$UHC.$Base.$Eq_27__NEW1652UNQ10212EVLDCT74__390__0RDC=
 new _F_(function($Eq_27__,$__)
         {var $Eq_27__3=
           _e_(new _A_($UHC.$Base.$Eq_27__CLS74__388__0,[$Eq_27__]));
          var $__5=
           new _A_($UHC.$Base.$Eq_27__DCT74__390__0DFLUHC_2eBase_2egeq_27,[$__]);
          var $__6=
           {_tag_:0,_1:$__5};
          return $__6;});
$UHC.$Base.$Eq_27__NEW1649UNQ10210DCT74__390__0RDC=
 new _F_(function($Eq_27__,$__)
         {var $Eq_27__3=
           new _A_($UHC.$Base.$Eq_27__NEW1652UNQ10212EVLDCT74__390__0RDC,[$Eq_27__,$__]);
          return $Eq_27__3;});
$UHC.$Base.$Eq_27__DCT74__390__0=
 new _F_(function($__)
         {var $Eq_27__=
           _i_();
          _i_set_($Eq_27__,new _A_($UHC.$Base.$Eq_27__NEW1649UNQ10210DCT74__390__0RDC,[$Eq_27__,$__]));
          return $Eq_27__;});
$UHC.$Base.$_26_26=
 new _F_(function($x1,$x2)
         {var $x13=
           _e_($x1);
          var $__swJSW687__0;
          switch($x13._tag_)
           {case 0:
             $__swJSW687__0=
              $UHC.$Base.$False__;
             break;
            case 1:
             $__swJSW687__0=
              $x2;
             break;}
          return $__swJSW687__0;});
$UHC.$Base.$__78__4183__0=
 new _F_(function($__,$__2,$a1,$b1,$__5)
         {var $__6=
           _e_($__5);
          var $__9=
           new _A_($UHC.$Base.$geq_27,[$__2,$b1,$__6._2]);
          var $__10=
           new _A_($UHC.$Base.$geq_27,[$__,$a1,$__6._1]);
          var $__11=
           new _A_($UHC.$Base.$_26_26,[$__10,$__9]);
          return $__11;});
$UHC.$Base.$Eq_27__DCT74__393__0DFLUHC_2eBase_2egeq_27=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__3);
          return new _A_($UHC.$Base.$__78__4183__0,[$__,$__2,$__4._1,$__4._2]);});
$UHC.$Base.$Eq_27__NEW1939UNQ10194EVLDCT74__393__0RDC=
 new _F_(function($__,$__2,$Eq_27__)
         {var $Eq_27__4=
           _e_(new _A_($UHC.$Base.$Eq_27__CLS74__388__0,[$Eq_27__]));
          var $__6=
           new _A_($UHC.$Base.$Eq_27__DCT74__393__0DFLUHC_2eBase_2egeq_27,[$__,$__2]);
          var $__7=
           {_tag_:0,_1:$__6};
          return $__7;});
$UHC.$Base.$Eq_27__NEW1935UNQ10191DCT74__393__0RDC=
 new _F_(function($__,$__2,$Eq_27__)
         {var $Eq_27__4=
           new _A_($UHC.$Base.$Eq_27__NEW1939UNQ10194EVLDCT74__393__0RDC,[$__,$__2,$Eq_27__]);
          return $Eq_27__4;});
$UHC.$Base.$Eq_27__DCT74__393__0=
 new _F_(function($__,$__2)
         {var $Eq_27__=
           _i_();
          _i_set_($Eq_27__,new _A_($UHC.$Base.$Eq_27__NEW1935UNQ10191DCT74__393__0RDC,[$__,$__2,$Eq_27__]));
          return $Eq_27__;});
$UHC.$Base.$__74__268__0=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$Eq_27__DCT74__390__0,[$__]);
          var $__3=
           new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$__2]);
          var $__4=
           new _A_($UHC.$Base.$Eq_27__DCT74__393__0,[$__3,$__3]);
          var $__5=
           new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$__4]);
          var $__6=
           new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$__5]);
          var $__74__268__0DFLUHC_2eBase_2e_3d_3d=
           new _A_($UHC.$Base.$geqdefault,[$UHC.$Base.$__Rep0RatioGENRepresentable0,$__6,$UHC.$Base.$undefined]);
          var $__7=
           _i_();
          _i_set_($__7,new _A_($UHC.$Base.$__74__268__0NEW2901UNQ10038RDC,[$__7,$__74__268__0DFLUHC_2eBase_2e_3d_3d]));
          return $__7;});
$UHC.$Base.$Num__DCT74__271__0DFLUHC_2eBase_2enegate=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          var $__6=
           new _A_($UHC.$Base.$negate,[$__,$__3._1]);
          var $__7=
           new _A_($UHC.$Base.$_3a_25,[$__6,$__3._2]);
          return $__7;});
$UHC.$Base.$Num__DCT74__271__0DFLUHC_2eBase_2efromInteger=
 new _F_(function($__,$x)
         {var $__3=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__4=
           new _A_($UHC.$Base.$fromInteger,[$__,$__3]);
          var $__5=
           new _A_($UHC.$Base.$fromInteger,[$__,$x]);
          return new _A_($UHC.$Base.$_3a_25,[$__5,$__4]);});
$UHC.$Base.$Num__DCT74__271__0DFLUHC_2eBase_2eabs=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          var $__6=
           new _A_($UHC.$Base.$abs,[$__,$__3._1]);
          var $__7=
           new _A_($UHC.$Base.$_3a_25,[$__6,$__3._2]);
          return $__7;});
$UHC.$Base.$fromInt=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._6;});
$UHC.$Base.$Num__DCT74__271__0DFLUHC_2eBase_2efromInt=
 new _F_(function($__,$x)
         {var $__3=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__4=
           new _A_($UHC.$Base.$fromInteger,[$__,$__3]);
          var $__5=
           new _A_($UHC.$Base.$fromInt,[$__,$x]);
          return new _A_($UHC.$Base.$_3a_25,[$__5,$__4]);});
$UHC.$Base.$Num__DCT74__271__0DFLUHC_2eBase_2esignum=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          var $__6=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__7=
           new _A_($UHC.$Base.$fromInteger,[$__,$__6]);
          var $__8=
           new _A_($UHC.$Base.$signum,[$__,$__3._1]);
          var $__9=
           new _A_($UHC.$Base.$_3a_25,[$__8,$__7]);
          return $__9;});
$UHC.$Base.$__78__11116__0=
 new _F_(function($__,$__2,$x,$y,$__5)
         {var $__6=
           _e_($__5);
          var $__9=
           new _A_($UHC.$Base.$_2a,[$__,$y,$__6._2]);
          var $__10=
           new _A_($UHC.$Base.$_2a,[$__,$x,$__6._1]);
          var $__11=
           new _A_($UHC.$Base.$reduce,[$__2,$__10,$__9]);
          return $__11;});
$UHC.$Base.$Num__DCT74__271__0DFLUHC_2eBase_2e_2a=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__3);
          return new _A_($UHC.$Base.$__78__11116__0,[$__,$__2,$__4._1,$__4._2]);});
$UHC.$Base.$__78__11134__0=
 new _F_(function($__,$__2,$x,$y,$__5)
         {var $__6=
           _e_($__5);
          var $__9=
           new _A_($UHC.$Base.$_2a,[$__,$y,$__6._2]);
          var $__10=
           new _A_($UHC.$Base.$_2a,[$__,$__6._1,$y]);
          var $__11=
           new _A_($UHC.$Base.$_2a,[$__,$x,$__6._2]);
          var $__12=
           new _A_($UHC.$Base.$_2b,[$__,$__11,$__10]);
          var $__13=
           new _A_($UHC.$Base.$reduce,[$__2,$__12,$__9]);
          return $__13;});
$UHC.$Base.$Num__DCT74__271__0DFLUHC_2eBase_2e_2b=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__3);
          return new _A_($UHC.$Base.$__78__11134__0,[$__,$__2,$__4._1,$__4._2]);});
$UHC.$Base.$Num__NEW5543UNQ10522EVLDCT74__271__0RDC=
 new _F_(function($Num__,$__,$__3,$__4)
         {var $Num__5=
           _e_(new _A_($UHC.$Base.$Num__CLS74__11__0,[$Num__]));
          var $__15=
           new _A_($UHC.$Base.$Num__DCT74__271__0DFLUHC_2eBase_2esignum,[$__3]);
          var $__16=
           new _A_($UHC.$Base.$Num__DCT74__271__0DFLUHC_2eBase_2enegate,[$__3]);
          var $__17=
           new _A_($UHC.$Base.$Num__DCT74__271__0DFLUHC_2eBase_2efromInteger,[$__3]);
          var $__18=
           new _A_($UHC.$Base.$Num__DCT74__271__0DFLUHC_2eBase_2efromInt,[$__3]);
          var $__19=
           new _A_($UHC.$Base.$Num__DCT74__271__0DFLUHC_2eBase_2eabs,[$__3]);
          var $__20=
           new _A_($UHC.$Base.$Num__DCT74__271__0DFLUHC_2eBase_2e_2b,[$__3,$__4]);
          var $__21=
           new _A_($UHC.$Base.$Num__DCT74__271__0DFLUHC_2eBase_2e_2a,[$__3,$__4]);
          var $__22=
           {_tag_:0,_1:$__21,_2:$__20,_3:$Num__5._3,_4:$__,_5:$__19,_6:$__18,_7:$__17,_8:$__16,_9:$__15};
          return $__22;});
$UHC.$Base.$Num__NEW5538UNQ10488DCT74__271__0RDC=
 new _F_(function($Num__,$__,$__3,$__4)
         {var $Num__5=
           new _A_($UHC.$Base.$Num__NEW5543UNQ10522EVLDCT74__271__0RDC,[$Num__,$__,$__3,$__4]);
          return $Num__5;});
$UHC.$Base.$__76__45170__2__1NEW5494UNQ10493=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Base.$__76__45597__2__0NEW5497UNQ10494=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Base.$Num__DCT74__271__0=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$__76__45168__2__3NEW5484UNQ10489,[$__]);
          var $__3=
           new _A_($UHC.$Base.$__76__45168__2__2NEW5487UNQ10490,[$__2]);
          var $__4=
           new _A_($UHC.$Base.$__76__45168__2__1NEW5490UNQ10491,[$__3]);
          var $__76__45169__3=
           new _A_($UHC.$Base.$__74__268__0,[$__4]);
          var $__5=
           new _A_($UHC.$Base.$__76__45170__2__1NEW5494UNQ10493,[$__]);
          var $__6=
           new _A_($UHC.$Base.$__76__45597__2__0NEW5497UNQ10494,[$__5]);
          var $Num__=
           _i_();
          _i_set_($Num__,new _A_($UHC.$Base.$Num__NEW5538UNQ10488DCT74__271__0RDC,[$Num__,$__76__45169__3,$__6,$__]));
          return $Num__;});
$UHC.$Base.$primIsInfiniteDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primIsInfiniteDouble($__2);});
$UHC.$Base.$primLogDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primLogDouble($__2);});
$UHC.$Base.$nNEW6614UNQ5230=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Base.$mNEW6617UNQ5229=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Base.$Fractional__DCT74__273__0DFLUHC_2eBase_2erecip=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          var $__6=
           new _A_($UHC.$Base.$_25,[$__,$__3._2,$__3._1]);
          return $__6;});
$UHC.$Base.$__76__46844__2__1NEW6630UNQ10966=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Base.$__76__47043__2__0NEW6633UNQ10968=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Base.$Fractional__DCT74__273__0DFLUHC_2eBase_2efromRational=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          var $__6=
           new _A_($UHC.$Base.$fromInteger,[$__,$__3._2]);
          var $__7=
           new _A_($UHC.$Base.$fromInteger,[$__,$__3._1]);
          var $__8=
           new _A_($UHC.$Base.$_3a_25,[$__7,$__6]);
          return $__8;});
$UHC.$Base.$Fractional__DCT74__273__0DFLUHC_2eBase_2e_2f=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__3);
          return new _A_($UHC.$Base.$__78__13374__0,[$__,$__2,$__4._2,$__4._1]);});
$UHC.$Base.$__78__13374__0=
 new _F_(function($__,$__2,$y,$x,$__5)
         {var $__6=
           _e_($__5);
          var $__9=
           new _A_($UHC.$Base.$_2a,[$__,$y,$__6._1]);
          var $__10=
           new _A_($UHC.$Base.$_2a,[$__,$x,$__6._2]);
          var $__11=
           new _A_($UHC.$Base.$_25,[$__2,$__10,$__9]);
          return $__11;});
$UHC.$Base.$Fractional__NEW6650UNQ10964DCT74__273__0RDC=
 new _F_(function($__,$__2,$Fractional__,$Fractional__4,$__5)
         {var $Fractional__6=
           new _A_($UHC.$Base.$Fractional__NEW6656UNQ10976EVLDCT74__273__0RDC,[$__,$__2,$Fractional__,$Fractional__4,$__5]);
          return $Fractional__6;});
$UHC.$Base.$Fractional__NEW6656UNQ10976EVLDCT74__273__0RDC=
 new _F_(function($__,$__2,$Fractional__,$Fractional__4,$__5)
         {var $Fractional__6=
           _e_(new _A_($UHC.$Base.$Fractional__CLS74__21__0,[$Fractional__4]));
          var $__12=
           new _A_($UHC.$Base.$Fractional__DCT74__273__0DFLUHC_2eBase_2erecip,[$__5]);
          var $__13=
           new _A_($UHC.$Base.$Fractional__DCT74__273__0DFLUHC_2eBase_2efromRational,[$__2]);
          var $__14=
           new _A_($UHC.$Base.$Fractional__DCT74__273__0DFLUHC_2eBase_2e_2f,[$__2,$__5]);
          var $__15=
           {_tag_:0,_1:$__14,_2:$__,_3:$Fractional__,_4:$__13,_5:$__12};
          return $__15;});
$UHC.$Base.$__76__17006__2__56NEW6679UNQ6082=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Base.$__76__23132__2__0NEW6682UNQ6075=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Base.$__78__13447__0=
 new _F_(function($__,$__2,$__3,$x)
         {var $radix=
           new _A_($UHC.$Base.$floatRadix,[$UHC.$Base.$RealFloat__DCT74__240__0,$x]);
          var $__6=
           new _A_($UHC.$Base.$decodeFloat,[$UHC.$Base.$RealFloat__DCT74__240__0,$x]);
          var $n=
           new _A_($UHC.$Base.$nNEW6688UNQ6102,[$__6]);
          var $m=
           new _A_($UHC.$Base.$mNEW6691UNQ6101,[$__6]);
          var $__9=
           new _A_($UHC.$Base.$negate,[$UHC.$Base.$Num__DCT74__101__0,$n]);
          var $denom=
           new _A_($UHC.$Base.$_5e,[$UHC.$Base.$Num__DCT74__134__0,$UHC.$Base.$Integral__DCT74__110__0,$radix,$__9]);
          var $pow=
           new _A_($UHC.$Base.$_5e,[$UHC.$Base.$Num__DCT74__134__0,$UHC.$Base.$Integral__DCT74__110__0,$radix,$n]);
          var $__12=
           new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__91__0,$n,0]);
          var $__13=
           _e_($__12);
          var $__swJSW713__0;
          switch($__13._tag_)
           {case 0:
             var $__14=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW714__0;
             switch($__14._tag_)
              {case 0:
                var $__15=
                 new _A_($UHC.$Base.$packedStringToString,["FAIL 75_466_0"]);
                var $__16=
                 new _A_($UHC.$Base.$error,[$__15]);
                $__swJSW714__0=
                 $__16;
                break;
               case 1:
                var $__17=
                 new _A_($UHC.$Base.$fromInteger,[$UHC.$Base.$Num__DCT74__177__0,$denom]);
                var $__18=
                 new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__177__0,$x,$__17]);
                var $__19=
                 new _A_($UHC.$Base.$round,[$UHC.$Base.$RealFrac__DCT74__227__0,$UHC.$Base.$Integral__DCT74__143__0,$__18]);
                var $__20=
                 new _A_($UHC.$Base.$_25,[$UHC.$Base.$Integral__DCT74__143__0,$__19,$denom]);
                var $__21=
                 new _A_($UHC.$Base.$fromRational,[$__,$__20]);
                $__swJSW714__0=
                 $__21;
                break;}
             $__swJSW713__0=
              $__swJSW714__0;
             break;
            case 1:
             var $__22=
              new _A_($UHC.$Base.$packedStringToInteger,["1"]);
             var $__23=
              new _A_($UHC.$Base.$fromInteger,[$__2,$__22]);
             var $__24=
              new _A_($UHC.$Base.$fromInteger,[$__2,$pow]);
             var $__25=
              new _A_($UHC.$Base.$fromInteger,[$UHC.$Base.$Num__DCT74__177__0,$pow]);
             var $__26=
              new _A_($UHC.$Base.$_2f,[$UHC.$Base.$Fractional__DCT74__197__0,$x,$__25]);
             var $__27=
              new _A_($UHC.$Base.$round,[$UHC.$Base.$RealFrac__DCT74__227__0,$__3,$__26]);
             var $__28=
              new _A_($UHC.$Base.$_2a,[$__2,$__27,$__24]);
             var $__29=
              new _A_($UHC.$Base.$_25,[$__3,$__28,$__23]);
             $__swJSW713__0=
              $__29;
             break;}
          return $__swJSW713__0;});
$UHC.$Base.$nNEW6688UNQ6102=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Base.$mNEW6691UNQ6101=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Base.$RealFloat__DCT74__240__0DFLUHC_2eBase_2efloatDigits=
 new _F_(function($__)
         {return $UHC.$Base.$primDigitsDouble;});
$UHC.$Base.$RealFloat__DCT74__240__0DFLUHC_2eBase_2efloatRange=
 new _F_(function($__)
         {return [$UHC.$Base.$primMinExpDouble,$UHC.$Base.$primMaxExpDouble];});
$UHC.$Base.$RealFloat__DCT74__240__0DFLUHC_2eBase_2efloatRadix=
 new _F_(function($__)
         {return new _A_($UHC.$Base.$toInteger,[$UHC.$Base.$Integral__DCT74__110__0,$UHC.$Base.$primRadixDoubleFloat]);});
$UHC.$Base.$RealFloat__DCT74__240__0DFLUHC_2eBase_2eisIEEE=
 new _F_(function($__)
         {return $UHC.$Base.$primIsIEEE;});
$UHC.$Base.$RealFloat__NEW6720UNQ11195DCT74__240__0RDC=
 new _F_(function($RealFloat__)
         {var $RealFloat__2=
           new _A_($UHC.$Base.$RealFloat__NEW6722UNQ11197EVLDCT74__240__0RDC,[$RealFloat__]);
          return $RealFloat__2;});
$UHC.$Base.$RealFloat__NEW6722UNQ11197EVLDCT74__240__0RDC=
 new _F_(function($RealFloat__)
         {var $RealFloat__2=
           _e_(new _A_($UHC.$Base.$RealFloat__CLS74__30__0,[$RealFloat__]));
          var $__19=
           {_tag_:0,_1:$UHC.$Base.$Floating__DCT74__212__0,_2:$UHC.$Base.$RealFrac__DCT74__227__0,_3:$UHC.$Base.$primAtan2Double,_4:$UHC.$Base.$primDecodeDouble,_5:$UHC.$Base.$primEncodeDouble,_6:$RealFloat__2._6,_7:$UHC.$Base.$RealFloat__DCT74__240__0DFLUHC_2eBase_2efloatDigits,_8:$UHC.$Base.$RealFloat__DCT74__240__0DFLUHC_2eBase_2efloatRadix,_9:$UHC.$Base.$RealFloat__DCT74__240__0DFLUHC_2eBase_2efloatRange,_10:$UHC.$Base.$primIsDenormalizedDouble,_11:$UHC.$Base.$RealFloat__DCT74__240__0DFLUHC_2eBase_2eisIEEE,_12:$UHC.$Base.$primIsInfiniteDouble,_13:$UHC.$Base.$primIsNaNDouble,_14:$UHC.$Base.$primIsNegativeZeroDouble,_15:$RealFloat__2._15,_16:$RealFloat__2._16};
          return $__19;});
$UHC.$Base.$RealFloat__UNQ11195DCT74__240__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$RealFloat__NEW6720UNQ11195DCT74__240__0RDC,[$UHC.$Base.$RealFloat__UNQ11195DCT74__240__0RDC]);}),[]);
$UHC.$Base.$Floating__NEW6727UNQ11222DCT74__212__0RDC=
 new _F_(function($Floating__)
         {var $Floating__2=
           new _A_($UHC.$Base.$Floating__NEW6729UNQ11223EVLDCT74__212__0RDC,[$Floating__]);
          return $Floating__2;});
$UHC.$Base.$Floating__NEW6729UNQ11223EVLDCT74__212__0RDC=
 new _F_(function($Floating__)
         {var $Floating__2=
           _e_(new _A_($UHC.$Base.$Floating__CLS74__22__0,[$Floating__]));
          var $__22=
           {_tag_:0,_1:$Floating__2._1,_2:$UHC.$Base.$Fractional__DCT74__197__0,_3:$UHC.$Base.$primAcosDouble,_4:$Floating__2._4,_5:$UHC.$Base.$primAsinDouble,_6:$Floating__2._6,_7:$UHC.$Base.$primAtanDouble,_8:$Floating__2._8,_9:$UHC.$Base.$primCosDouble,_10:$UHC.$Base.$primCoshDouble,_11:$UHC.$Base.$primExpDouble,_12:$UHC.$Base.$primLogDouble,_13:$Floating__2._13,_14:$Floating__2._14,_15:$UHC.$Base.$primSinDouble,_16:$UHC.$Base.$primSinhDouble,_17:$UHC.$Base.$primSqrtDouble,_18:$UHC.$Base.$primTanDouble,_19:$UHC.$Base.$primTanhDouble};
          return $__22;});
$UHC.$Base.$Floating__UNQ11222DCT74__212__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Floating__NEW6727UNQ11222DCT74__212__0RDC,[$UHC.$Base.$Floating__UNQ11222DCT74__212__0RDC]);}),[]);
$UHC.$Base.$Fractional__DCT74__197__0DFLUHC_2eBase_2efromDouble=
 new _F_(function($x)
         {return $x;});
$UHC.$Base.$Fractional__NEW6735UNQ10954DCT74__197__0RDC=
 new _F_(function($Fractional__)
         {var $Fractional__2=
           new _A_($UHC.$Base.$Fractional__NEW6737UNQ10955EVLDCT74__197__0RDC,[$Fractional__]);
          return $Fractional__2;});
$UHC.$Base.$Fractional__NEW6737UNQ10955EVLDCT74__197__0RDC=
 new _F_(function($Fractional__)
         {var $Fractional__2=
           _e_(new _A_($UHC.$Base.$Fractional__CLS74__21__0,[$Fractional__]));
          var $__8=
           {_tag_:0,_1:$UHC.$Base.$primDivideDouble,_2:$UHC.$Base.$Num__DCT74__177__0,_3:$UHC.$Base.$Fractional__DCT74__197__0DFLUHC_2eBase_2efromDouble,_4:$UHC.$Base.$primRationalToDouble,_5:$UHC.$Base.$primRecipDouble};
          return $__8;});
$UHC.$Base.$Fractional__UNQ10954DCT74__197__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Fractional__NEW6735UNQ10954DCT74__197__0RDC,[$UHC.$Base.$Fractional__UNQ10954DCT74__197__0RDC]);}),[]);
$UHC.$Base.$__76__22798__2__0NEW6665UNQ6058=
 new _F_(function($Fractional__)
         {var $Num__=
           _e_($Fractional__);
          return $Num__._2;});
$UHC.$Base.$Fractional__CLS74__21__0DFLUHC_2eBase_2e_2f=
 new _F_(function($Fractional__,$__,$x,$y)
         {var $__5=
           new _A_($UHC.$Base.$recip,[$Fractional__,$y]);
          return new _A_($UHC.$Base.$_2a,[$__,$x,$__5]);});
$UHC.$Base.$Fractional__CLS74__21__0DFLUHC_2eBase_2efromDouble=
 new _F_(function($__,$Fractional__,$x)
         {var $__4=
           new _A_($UHC.$Base.$fromDouble,[$__,$x]);
          return new _A_($UHC.$Base.$fromRational,[$Fractional__,$__4]);});
$UHC.$Base.$__76__47194__2__1NEW6576UNQ11075=
 new _F_(function($__,$__2,$RealFrac__)
         {var $Fractional__=
           _e_($RealFrac__);
          return $Fractional__._1;});
$UHC.$Base.$RealFrac__NEW6581UNQ11074DCT74__227__0RDC=
 new _F_(function($__,$__2,$RealFrac__)
         {var $RealFrac__4=
           new _A_($UHC.$Base.$RealFrac__NEW6585UNQ11079EVLDCT74__227__0RDC,[$__2,$RealFrac__]);
          return $RealFrac__4;});
$UHC.$Base.$RealFrac__NEW6585UNQ11079EVLDCT74__227__0RDC=
 new _F_(function($__,$RealFrac__)
         {var $RealFrac__3=
           _e_(new _A_($UHC.$Base.$RealFrac__CLS74__24__0,[$RealFrac__]));
          var $__11=
           new _A_($UHC.$Base.$RealFrac__DCT74__227__0DFLUHC_2eBase_2eproperFraction,[$__]);
          var $__12=
           {_tag_:0,_1:$UHC.$Base.$Fractional__DCT74__197__0,_2:$UHC.$Base.$Real__DCT74__188__0,_3:$RealFrac__3._3,_4:$RealFrac__3._4,_5:$__11,_6:$RealFrac__3._6,_7:$RealFrac__3._7};
          return $__12;});
$UHC.$Base.$__76__47212__13__0NEW6598UNQ11076=
 new _F_(function($__,$__2,$RealFrac__)
         {var $Num__=
           _e_($__);
          return $Num__._2;});
$UHC.$Base.$RealFrac__DCT74__227__0DFLUHC_2eBase_2eproperFraction=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($UHC.$Base.$__76__47194__2__3NEW6591UNQ11089,[$__2]);
          var $__4=
           new _A_($UHC.$Base.$__76__47212__10__0NEW6594UNQ11090,[$__3]);
          return new _A_($UHC.$Base.$__78__13292__0,[$__,$__4]);});
$UHC.$Base.$__76__47194__2__3NEW6591UNQ11089=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Base.$__76__47212__10__0NEW6594UNQ11090=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Base.$__78__13292__0=
 new _F_(function($__,$__2,$__3)
         {return new _A_($UHC.$Base.$floatProperFraction,[$UHC.$Base.$RealFloat__DCT74__240__0,$__3,$UHC.$Base.$RealFloat__DCT74__240__0,$__2,$__]);});
$UHC.$Base.$__76__47194__2__1UNQ11075=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$__76__47194__2__1NEW6576UNQ11075,[$UHC.$Base.$__76__47194__2__1UNQ11075,$UHC.$Base.$__76__47212__13__0UNQ11076,$UHC.$Base.$RealFrac__UNQ11074DCT74__227__0RDC]);}),[]);
$UHC.$Base.$__76__47212__13__0UNQ11076=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$__76__47212__13__0NEW6598UNQ11076,[$UHC.$Base.$__76__47194__2__1UNQ11075,$UHC.$Base.$__76__47212__13__0UNQ11076,$UHC.$Base.$RealFrac__UNQ11074DCT74__227__0RDC]);}),[]);
$UHC.$Base.$RealFrac__UNQ11074DCT74__227__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$RealFrac__NEW6581UNQ11074DCT74__227__0RDC,[$UHC.$Base.$__76__47194__2__1UNQ11075,$UHC.$Base.$__76__47212__13__0UNQ11076,$UHC.$Base.$RealFrac__UNQ11074DCT74__227__0RDC]);}),[]);
$UHC.$Base.$Real__NEW6604UNQ10924DCT74__188__0RDC=
 new _F_(function($Real__)
         {var $Real__2=
           new _A_($UHC.$Base.$Real__NEW6606UNQ10925EVLDCT74__188__0RDC,[$Real__]);
          return $Real__2;});
$UHC.$Base.$Real__NEW6606UNQ10925EVLDCT74__188__0RDC=
 new _F_(function($Real__)
         {var $Real__2=
           _e_(new _A_($UHC.$Base.$Real__CLS74__13__0,[$Real__]));
          var $__6=
           {_tag_:0,_1:$UHC.$Base.$Num__DCT74__177__0,_2:$UHC.$Base.$Ord__DCT74__166__0,_3:$UHC.$Base.$doubleToRational};
          return $__6;});
$UHC.$Base.$Real__UNQ10924DCT74__188__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Real__NEW6604UNQ10924DCT74__188__0RDC,[$UHC.$Base.$Real__UNQ10924DCT74__188__0RDC]);}),[]);
$UHC.$Base.$RealFloat__DCT74__240__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$RealFloat__UNQ11195DCT74__240__0RDC;}),[]);
$UHC.$Base.$Floating__DCT74__212__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Floating__UNQ11222DCT74__212__0RDC;}),[]);
$UHC.$Base.$Fractional__DCT74__197__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Fractional__UNQ10954DCT74__197__0RDC;}),[]);
$UHC.$Base.$RealFrac__DCT74__227__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$RealFrac__UNQ11074DCT74__227__0RDC;}),[]);
$UHC.$Base.$Real__DCT74__188__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Real__UNQ10924DCT74__188__0RDC;}),[]);
$UHC.$Base.$doubleToRational=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$fromRat,[$UHC.$Base.$RealFloat__DCT74__240__0]);}),[]);
$UHC.$Base.$__76__20513__2__0=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Fractional__DCT74__273__0,[$UHC.$Base.$Integral__DCT74__143__0]);}),[]);
$UHC.$Base.$fromRat=
 new _F_(function($__,$x)
         {var $b=
           new _A_($UHC.$Base.$floatRadix,[$__,$x]);
          var $__4=
           new _A_($UHC.$Base.$decodeFloat,[$__,$x]);
          var $n=
           new _A_($UHC.$Base.$nNEW6614UNQ5230,[$__4]);
          var $m=
           new _A_($UHC.$Base.$mNEW6617UNQ5229,[$__4]);
          var $__7=
           new _A_($UHC.$Base.$primIntToInteger,[1]);
          var $__8=
           new _A_($UHC.$Base.$_25,[$UHC.$Base.$Integral__DCT74__143__0,$b,$__7]);
          var $__9=
           new _A_($UHC.$Base.$_5e_5e,[$UHC.$Base.$__76__20513__2__0,$UHC.$Base.$Integral__DCT74__110__0,$__8,$n]);
          var $__10=
           new _A_($UHC.$Base.$primIntToInteger,[1]);
          var $__11=
           new _A_($UHC.$Base.$_25,[$UHC.$Base.$Integral__DCT74__143__0,$m,$__10]);
          return new _A_($UHC.$Base.$_2a,[$UHC.$Base.$__76__20479__2__0,$__11,$__9]);});
$UHC.$Base.$Fractional__DCT74__273__0=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$__76__46844__2__1NEW6630UNQ10966,[$__]);
          var $__3=
           new _A_($UHC.$Base.$__76__47043__2__0NEW6633UNQ10968,[$__2]);
          var $__76__46843__3=
           new _A_($UHC.$Base.$Num__DCT74__271__0,[$__]);
          var $Fractional__DCT74__273__0DFLUHC_2eBase_2efromDouble=
           new _A_($UHC.$Base.$doubleToRatio,[$__]);
          var $Fractional__=
           _i_();
          _i_set_($Fractional__,new _A_($UHC.$Base.$Fractional__NEW6650UNQ10964DCT74__273__0RDC,[$__76__46843__3,$__3,$Fractional__DCT74__273__0DFLUHC_2eBase_2efromDouble,$Fractional__,$__]));
          return $Fractional__;});
$UHC.$Base.$doubleToRatio=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$Fractional__DCT74__273__0,[$__]);
          var $__3=
           new _A_($UHC.$Base.$__76__17006__2__56NEW6679UNQ6082,[$__]);
          var $__4=
           new _A_($UHC.$Base.$__76__23132__2__0NEW6682UNQ6075,[$__3]);
          return new _A_($UHC.$Base.$__78__13447__0,[$__2,$__4,$__]);});
$UHC.$Base.$Fractional__CLS74__21__0=
 new _F_(function($Fractional__)
         {var $__=
           new _A_($UHC.$Base.$__76__22798__2__0NEW6665UNQ6058,[$Fractional__]);
          var $__3=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__4=
           new _A_($UHC.$Base.$fromInteger,[$__,$__3]);
          var $Fractional__CLS74__21__0DFLUHC_2eBase_2erecip=
           new _A_($UHC.$Base.$_2f,[$Fractional__,$__4]);
          var $__5=
           new _A_($UHC.$Base.$Fractional__DCT74__273__0,[$UHC.$Base.$Integral__DCT74__143__0]);
          var $__6=
           new _A_($UHC.$Base.$Fractional__CLS74__21__0DFLUHC_2eBase_2efromDouble,[$__5,$Fractional__]);
          var $__7=
           new _A_($UHC.$Base.$Fractional__CLS74__21__0DFLUHC_2eBase_2e_2f,[$Fractional__,$__]);
          var $Fractional__8=
           {_tag_:0,_1:$__7,_2:$UHC.$Base.$undefined,_3:$__6,_4:$UHC.$Base.$undefined,_5:$Fractional__CLS74__21__0DFLUHC_2eBase_2erecip};
          return $Fractional__8;});
$Asteroids.$__55__137=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToInteger,["10"]);}),[]);
$Asteroids.$__55__136=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToInteger,["1"]);}),[]);
$Asteroids.$__55__134=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3a_25,[$Asteroids.$__55__136,$Asteroids.$__55__137]);}),[]);
$Asteroids.$chance=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$fromRational,[$UHC.$Base.$Fractional__DCT74__197__0,$Asteroids.$__55__134]);}),[]);
$UHC.$Base.$primIntegerToDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.doubleValue();});
$UHC.$Base.$primSubDouble=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primSubInt($__3,$__4);});
$UHC.$Base.$primIntToDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primUnsafeId($__2);});
$UHC.$Base.$primMulDouble=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primMulInt($__3,$__4);});
$UHC.$Base.$enumFromTo=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._4;});
$UHC.$Base.$primDivInt=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primDivInt($__3,$__4);});
$UHC.$Base.$primAddInt=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primAddInt($__3,$__4);});
$UHC.$Base.$primSubInt=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primSubInt($__3,$__4);});
$UHC.$Base.$__78__3041__0=
 new _F_(function($__,$_24x__75__36__0)
         {var $__3=
           new _A_($UHC.$Base.$packedStringToInteger,["1"]);
          var $__4=
           new _A_($UHC.$Base.$fromInteger,[$__,$__3]);
          return new _A_($UHC.$Base.$_2b,[$__,$_24x__75__36__0,$__4]);});
$UHC.$Base.$numericEnumFrom=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$__78__3041__0,[$__]);
          return new _A_($UHC.$Base.$iterate_27,[$__2]);});
$UHC.$Base.$primNegInt=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primNegInt($__2);});
$UHC.$Base.$primIntegerToInt=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primIntegerToInt($__2);});
$UHC.$Base.$flip=
 new _F_(function($f,$x,$y)
         {return new _A_($f,[$y,$x]);});
$UHC.$Base.$subtract=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$_2d,[$__]);
          return new _A_($UHC.$Base.$flip,[$__2]);});
$UHC.$Base.$__76__26391__2__0NEW3895UNQ6972=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$UHC.$Base.$__78__7922__0=
 new _F_(function($__,$__2,$__3,$x)
         {var $__5=
           new _A_($UHC.$Base.$minBound,[$__2]);
          var $__6=
           new _A_($UHC.$Base.$_3d_3d,[$__3,$x,$__5]);
          var $__7=
           _e_($__6);
          var $__swJSW729__0;
          switch($__7._tag_)
           {case 0:
             var $__8=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW730__0;
             switch($__8._tag_)
              {case 0:
                var $__9=
                 new _A_($UHC.$Base.$packedStringToString,["FAIL 75_22_0"]);
                var $__10=
                 new _A_($UHC.$Base.$error,[$__9]);
                $__swJSW730__0=
                 $__10;
                break;
               case 1:
                var $__11=
                 new _A_($UHC.$Base.$packedStringToInteger,["1"]);
                var $__12=
                 new _A_($UHC.$Base.$fromInteger,[$__,$__11]);
                var $__13=
                 new _A_($UHC.$Base.$_2d,[$__,$x,$__12]);
                $__swJSW730__0=
                 $__13;
                break;}
             $__swJSW729__0=
              $__swJSW730__0;
             break;
            case 1:
             var $__14=
              new _A_($UHC.$Base.$packedStringToString,["pred: applied to minBound"]);
             var $__15=
              new _A_($UHC.$Base.$error,[$__14]);
             $__swJSW729__0=
              $__15;
             break;}
          return $__swJSW729__0;});
$UHC.$Base.$boundedPred=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           new _A_($UHC.$Base.$__76__26391__2__0NEW3895UNQ6972,[$__]);
          return new _A_($UHC.$Base.$__78__7922__0,[$__,$__2,$__4]);});
$UHC.$Base.$enumFrom=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$UHC.$Base.$toInt=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._9;});
$UHC.$Base.$primRemInt=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primRemInt($__3,$__4);});
$UHC.$Base.$__76__20310__2__0NEW3938UNQ5199=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Base.$__76__17006__2__95NEW3935UNQ5203=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Base.$__76__17006__2__101NEW3911UNQ5063=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Base.$__76__19701__2__0NEW3914UNQ5057=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Base.$_3a_25=
 new _F_(function($x1,$x2)
         {return {_tag_:0,_1:$x1,_2:$x2};});
$UHC.$Base.$rem=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._8;});
$UHC.$Base.$gcd_27UNQ5156=
 new _F_(function($__,$__2,$__3,$x1,$x2)
         {var $__6=
           new _A_($UHC.$Base.$rem,[$__,$x1,$x2]);
          var $__7=
           new _A_($UHC.$Base.$gcd_27UNQ5156,[$__,$__2,$__3,$x2,$__6]);
          var $__8=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__9=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__8]);
          var $x210=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$__2,$__9,$x2]));
          var $__swJSW738__0;
          switch($x210._tag_)
           {case 0:
             $__swJSW738__0=
              $__7;
             break;
            case 1:
             $__swJSW738__0=
              $x1;
             break;}
          return $__swJSW738__0;});
$UHC.$Base.$__76__19858__0NEW2313UNQ5153CCN=
 new _F_(function($__,$__2,$__3,$x1,$x2)
         {var $__6=
           new _A_($UHC.$Base.$abs,[$__,$x2]);
          var $__7=
           new _A_($UHC.$Base.$abs,[$__,$x1]);
          return new _A_($UHC.$Base.$gcd_27UNQ5156,[$__3,$__2,$__,$__7,$__6]);});
$UHC.$Base.$__78__4950__0=
 new _F_(function($__,$__2,$__3,$x1,$x2)
         {var $__6=
           new _A_($UHC.$Base.$__76__19858__0NEW2313UNQ5153CCN,[$__,$__2,$__3,$x1,$x2]);
          var $__7=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__8=
           new _A_($UHC.$Base.$fromInteger,[$__,$__7]);
          var $x19=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$__2,$__8,$x1]));
          var $__swJSW739__0;
          switch($x19._tag_)
           {case 0:
             $__swJSW739__0=
              $__6;
             break;
            case 1:
             var $__10=
              new _A_($UHC.$Base.$packedStringToInteger,["0"]);
             var $__11=
              new _A_($UHC.$Base.$fromInteger,[$__,$__10]);
             var $x212=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$__2,$__11,$x2]));
             var $__swJSW740__0;
             switch($x212._tag_)
              {case 0:
                $__swJSW740__0=
                 $__6;
                break;
               case 1:
                var $__13=
                 new _A_($UHC.$Base.$packedStringToString,["Prelude.gcd: gcd 0 0 is undefined"]);
                var $__14=
                 new _A_($UHC.$Base.$error,[$__13]);
                $__swJSW740__0=
                 $__14;
                break;}
             $__swJSW739__0=
              $__swJSW740__0;
             break;}
          return $__swJSW739__0;});
$UHC.$Base.$__76__20029__8__0NEW2306UNQ5098=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Base.$__76__17006__2__3NEW2303UNQ5097=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$UHC.$Base.$__76__20029__5__0NEW2309UNQ5099=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$UHC.$Base.$gcd=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$__76__17006__2__3NEW2303UNQ5097,[$__]);
          var $__3=
           new _A_($UHC.$Base.$__76__20029__8__0NEW2306UNQ5098,[$__2]);
          var $__4=
           new _A_($UHC.$Base.$__76__20029__5__0NEW2309UNQ5099,[$__3]);
          return new _A_($UHC.$Base.$__78__4950__0,[$__3,$__4,$__]);});
$UHC.$Base.$quot=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._6;});
$UHC.$Base.$__78__7955__0=
 new _F_(function($__,$__2,$__3,$x,$y)
         {var $d=
           new _A_($UHC.$Base.$gcd,[$__2,$x,$y]);
          var $__7=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__8=
           new _A_($UHC.$Base.$fromInteger,[$__,$__7]);
          var $__9=
           new _A_($UHC.$Base.$_3d_3d,[$__3,$y,$__8]);
          var $__10=
           _e_($__9);
          var $__swJSW745__0;
          switch($__10._tag_)
           {case 0:
             var $__11=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW746__0;
             switch($__11._tag_)
              {case 0:
                var $__12=
                 new _A_($UHC.$Base.$packedStringToString,["FAIL 75_444_0"]);
                var $__13=
                 new _A_($UHC.$Base.$error,[$__12]);
                $__swJSW746__0=
                 $__13;
                break;
               case 1:
                var $__14=
                 new _A_($UHC.$Base.$quot,[$__2,$y,$d]);
                var $__15=
                 new _A_($UHC.$Base.$quot,[$__2,$x,$d]);
                var $__16=
                 new _A_($UHC.$Base.$_3a_25,[$__15,$__14]);
                $__swJSW746__0=
                 $__16;
                break;}
             $__swJSW745__0=
              $__swJSW746__0;
             break;
            case 1:
             var $__17=
              new _A_($UHC.$Base.$packedStringToString,["Ratio.%: zero denominator"]);
             var $__18=
              new _A_($UHC.$Base.$error,[$__17]);
             $__swJSW745__0=
              $__18;
             break;}
          return $__swJSW745__0;});
$UHC.$Base.$__76__19691__2__0NEW3917UNQ5058=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$UHC.$Base.$reduce=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$__76__17006__2__101NEW3911UNQ5063,[$__]);
          var $__3=
           new _A_($UHC.$Base.$__76__19701__2__0NEW3914UNQ5057,[$__2]);
          var $__4=
           new _A_($UHC.$Base.$__76__19691__2__0NEW3917UNQ5058,[$__3]);
          return new _A_($UHC.$Base.$__78__7955__0,[$__3,$__,$__4]);});
$UHC.$Base.$_2a=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$UHC.$Base.$abs=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._5;});
$UHC.$Base.$__78__7995__0=
 new _F_(function($__,$__2,$x,$y)
         {var $__5=
           new _A_($UHC.$Base.$abs,[$__,$y]);
          var $__6=
           new _A_($UHC.$Base.$signum,[$__,$y]);
          var $__7=
           new _A_($UHC.$Base.$_2a,[$__,$x,$__6]);
          return new _A_($UHC.$Base.$reduce,[$__2,$__7,$__5]);});
$UHC.$Base.$_25=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$__76__17006__2__95NEW3935UNQ5203,[$__]);
          var $__3=
           new _A_($UHC.$Base.$__76__20310__2__0NEW3938UNQ5199,[$__2]);
          return new _A_($UHC.$Base.$__78__7995__0,[$__3,$__]);});
$UHC.$Base.$_2e=
 new _F_(function($f,$g,$x)
         {var $__=
           new _A_($g,[$x]);
          return new _A_($f,[$__]);});
$UHC.$Base.$fromIntegral=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($UHC.$Base.$toInteger,[$__]);
          var $__4=
           new _A_($UHC.$Base.$fromInteger,[$__2]);
          return new _A_($UHC.$Base.$_2e,[$__4,$__3]);});
$UHC.$Base.$Bounded__CLS74__6__0=
 new _F_(function($Bounded__)
         {var $Bounded__2=
           {_tag_:0,_1:$UHC.$Base.$undefined,_2:$UHC.$Base.$undefined};
          return $Bounded__2;});
$UHC.$Base.$primMaxInt=
 new _A_(new _F_(function()
                 {return primMaxInt();}),[]);
$UHC.$Base.$primMinInt=
 new _A_(new _F_(function()
                 {return primMinInt();}),[]);
$UHC.$Base.$Bounded__NEW1337UNQ9657EVLDCT74__97__0RDC=
 new _F_(function($Bounded__)
         {var $Bounded__2=
           _e_(new _A_($UHC.$Base.$Bounded__CLS74__6__0,[$Bounded__]));
          var $__5=
           {_tag_:0,_1:$UHC.$Base.$primMaxInt,_2:$UHC.$Base.$primMinInt};
          return $__5;});
$UHC.$Base.$Bounded__NEW1335UNQ9656DCT74__97__0RDC=
 new _F_(function($Bounded__)
         {var $Bounded__2=
           new _A_($UHC.$Base.$Bounded__NEW1337UNQ9657EVLDCT74__97__0RDC,[$Bounded__]);
          return $Bounded__2;});
$UHC.$Base.$Bounded__UNQ9656DCT74__97__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Bounded__NEW1335UNQ9656DCT74__97__0RDC,[$UHC.$Base.$Bounded__UNQ9656DCT74__97__0RDC]);}),[]);
$UHC.$Base.$Bounded__DCT74__97__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Bounded__UNQ9656DCT74__97__0RDC;}),[]);
$UHC.$Base.$quotRem=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._7;});
$UHC.$Base.$primDivModInteger=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primDivModInteger($__3,$__4);});
$UHC.$Base.$__78__3884__0=
 new _F_(function($__,$m,$_24x__75__26__0)
         {return new _A_($UHC.$Base.$_3c_3d,[$__,$_24x__75__26__0,$m]);});
$UHC.$Base.$boundedEnumFromTo=
 new _F_(function($__,$__2,$__3,$n,$m)
         {var $__6=
           new _A_($UHC.$Base.$boundedEnumFrom,[$__,$__2,$__3,$n]);
          var $__7=
           new _A_($UHC.$Base.$__78__3884__0,[$__,$m]);
          return new _A_($UHC.$Base.$takeWhile,[$__7,$__6]);});
$UHC.$Base.$primModInt=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primModInt($__3,$__4);});
$UHC.$Base.$__78__3068__0=
 new _F_(function($__,$n,$m,$_24x__75__38__0)
         {var $__5=
           new _A_($UHC.$Base.$_2d,[$__,$m,$n]);
          return new _A_($UHC.$Base.$_2b,[$__,$_24x__75__38__0,$__5]);});
$UHC.$Base.$__78__1305NEW380=
 new _F_(function($f,$x)
         {var $fx=
           new _A_($f,[$x]);
          var $fx4=
           _e_($fx);
          return new _A_($UHC.$Base.$iterate_27,[$f,$fx]);});
$UHC.$Base.$iterate_27=
 new _F_(function($f,$x)
         {var $__=
           new _A_($UHC.$Base.$__78__1305NEW380,[$f,$x]);
          return new _A_($UHC.$Base.$_3a,[$x,$__]);});
$UHC.$Base.$numericEnumFromThen=
 new _F_(function($__,$n,$m)
         {var $__4=
           new _A_($UHC.$Base.$__78__3068__0,[$__,$n,$m]);
          return new _A_($UHC.$Base.$iterate_27,[$__4,$n]);});
$UHC.$Base.$__78__7616__0=
 new _F_(function($__,$__2,$__3,$x)
         {var $__5=
           new _A_($UHC.$Base.$maxBound,[$__2]);
          var $__6=
           new _A_($UHC.$Base.$_3d_3d,[$__3,$x,$__5]);
          var $__7=
           _e_($__6);
          var $__swJSW752__0;
          switch($__7._tag_)
           {case 0:
             var $__8=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW753__0;
             switch($__8._tag_)
              {case 0:
                var $__9=
                 new _A_($UHC.$Base.$packedStringToString,["FAIL 75_21_0"]);
                var $__10=
                 new _A_($UHC.$Base.$error,[$__9]);
                $__swJSW753__0=
                 $__10;
                break;
               case 1:
                var $__11=
                 new _A_($UHC.$Base.$packedStringToInteger,["1"]);
                var $__12=
                 new _A_($UHC.$Base.$fromInteger,[$__,$__11]);
                var $__13=
                 new _A_($UHC.$Base.$_2b,[$__,$x,$__12]);
                $__swJSW753__0=
                 $__13;
                break;}
             $__swJSW752__0=
              $__swJSW753__0;
             break;
            case 1:
             var $__14=
              new _A_($UHC.$Base.$packedStringToString,["succ: applied to maxBound"]);
             var $__15=
              new _A_($UHC.$Base.$error,[$__14]);
             $__swJSW752__0=
              $__15;
             break;}
          return $__swJSW752__0;});
$UHC.$Base.$__76__26534__2__0NEW3739UNQ6994=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$UHC.$Base.$boundedSucc=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           new _A_($UHC.$Base.$__76__26534__2__0NEW3739UNQ6994,[$__]);
          return new _A_($UHC.$Base.$__78__7616__0,[$__,$__2,$__4]);});
$UHC.$Base.$_3e=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._3;});
$UHC.$Base.$__78__5471__0=
 new _F_(function($__,$__2,$__3,$x)
         {var $__5=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__6=
           new _A_($UHC.$Base.$fromInteger,[$__2,$__5]);
          var $__7=
           new _A_($UHC.$Base.$_3d_3d,[$__3,$x,$__6]);
          var $__8=
           _e_($__7);
          var $__swJSW756__0;
          switch($__8._tag_)
           {case 0:
             var $__9=
              new _A_($UHC.$Base.$packedStringToInteger,["0"]);
             var $__10=
              new _A_($UHC.$Base.$fromInteger,[$__2,$__9]);
             var $__11=
              new _A_($UHC.$Base.$_3e,[$__,$x,$__10]);
             var $__12=
              _e_($__11);
             var $__swJSW757__0;
             switch($__12._tag_)
              {case 0:
                var $__13=
                 _e_($UHC.$Base.$otherwise);
                var $__swJSW758__0;
                switch($__13._tag_)
                 {case 0:
                   var $__14=
                    new _A_($UHC.$Base.$packedStringToString,["FAIL 75_119_0"]);
                   var $__15=
                    new _A_($UHC.$Base.$error,[$__14]);
                   $__swJSW758__0=
                    $__15;
                   break;
                  case 1:
                   var $__16=
                    new _A_($UHC.$Base.$packedStringToInteger,["1"]);
                   var $__17=
                    new _A_($UHC.$Base.$fromInteger,[$__2,$__16]);
                   var $__18=
                    new _A_($UHC.$Base.$negate,[$__2,$__17]);
                   $__swJSW758__0=
                    $__18;
                   break;}
                $__swJSW757__0=
                 $__swJSW758__0;
                break;
               case 1:
                var $__19=
                 new _A_($UHC.$Base.$packedStringToInteger,["1"]);
                var $__20=
                 new _A_($UHC.$Base.$fromInteger,[$__2,$__19]);
                $__swJSW757__0=
                 $__20;
                break;}
             $__swJSW756__0=
              $__swJSW757__0;
             break;
            case 1:
             var $__21=
              new _A_($UHC.$Base.$packedStringToInteger,["0"]);
             var $__22=
              new _A_($UHC.$Base.$fromInteger,[$__2,$__21]);
             $__swJSW756__0=
              $__22;
             break;}
          return $__swJSW756__0;});
$UHC.$Base.$__76__18754__2__0NEW2564UNQ4907=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$UHC.$Base.$signumReal=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($UHC.$Base.$__76__18754__2__0NEW2564UNQ4907,[$__2]);
          return new _A_($UHC.$Base.$__78__5471__0,[$__,$__2,$__3]);});
$UHC.$Base.$primMulInteger=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return $__3.multiply($__4);});
$UHC.$Base.$primQuotRemInt=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primQuotRemInt($__3,$__4);});
$UHC.$Base.$minBound=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._2;});
$UHC.$Base.$__78__8456NEW4292=
 new _F_(function($__,$__2,$n,$m)
         {var $__5=
           new _A_($UHC.$Base.$_3c_3d,[$__,$n,$m]);
          var $__6=
           _e_($__5);
          var $__swJSW761__0;
          switch($__6._tag_)
           {case 0:
             var $__7=
              new _A_($UHC.$Base.$minBound,[$__2]);
             $__swJSW761__0=
              $__7;
             break;
            case 1:
             var $__8=
              new _A_($UHC.$Base.$maxBound,[$__2]);
             $__swJSW761__0=
              $__8;
             break;}
          return $__swJSW761__0;});
$UHC.$Base.$boundedEnumFromThen=
 new _F_(function($__,$__2,$__3,$n,$m)
         {var $__6=
           new _A_($UHC.$Base.$__78__8456NEW4292,[$__,$__2,$n,$m]);
          return new _A_($UHC.$Base.$enumFromThenTo,[$__3,$n,$m,$__6]);});
$UHC.$Base.$enumFromThen=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._2;});
$UHC.$Base.$primQuotRemInteger=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primQuotRemInteger($__3,$__4);});
$UHC.$Base.$primNegInteger=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.negate();});
$UHC.$Base.$primIntToInteger=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primIntToInteger($__2);});
$UHC.$Base.$primAddInteger=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return $__3.add($__4);});
$UHC.$Base.$_24okUNQ3572=
 new _F_(function($f,$_24x)
         {var $__=
           new _A_($f,[$_24x]);
          return new _A_($UHC.$Base.$_3a,[$__,$UHC.$Base.$_5b_5d]);});
$UHC.$Base.$_2b_2b=
 new _F_(function($x1,$x2)
         {var $x13=
           _e_($x1);
          var $__swJSW763__0;
          switch($x13._tag_)
           {case 0:
             var $__=
              new _A_($UHC.$Base.$_2b_2b,[$x13._2,$x2]);
             var $__7=
              new _A_($UHC.$Base.$_3a,[$x13._1,$__]);
             $__swJSW763__0=
              $__7;
             break;
            case 1:
             $__swJSW763__0=
              $x2;
             break;}
          return $__swJSW763__0;});
$UHC.$Base.$concatMap=
 new _F_(function($x1,$x2)
         {var $x23=
           _e_($x2);
          var $__swJSW764__0;
          switch($x23._tag_)
           {case 0:
             var $__=
              new _A_($UHC.$Base.$concatMap,[$x1,$x23._2]);
             var $__7=
              new _A_($x1,[$x23._1]);
             var $__8=
              new _A_($UHC.$Base.$_2b_2b,[$__7,$__]);
             $__swJSW764__0=
              $__8;
             break;
            case 1:
             $__swJSW764__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW764__0;});
$UHC.$Base.$map=
 new _F_(function($f,$xs)
         {var $__=
           new _A_($UHC.$Base.$_24okUNQ3572,[$f]);
          return new _A_($UHC.$Base.$concatMap,[$__,$xs]);});
$UHC.$Base.$Real__CLS74__13__0=
 new _F_(function($Real__)
         {var $Real__2=
           {_tag_:0,_1:$UHC.$Base.$undefined,_2:$UHC.$Base.$undefined,_3:$UHC.$Base.$undefined};
          return $Real__2;});
$UHC.$Base.$primMulInt=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primMulInt($__3,$__4);});
$UHC.$Base.$divMod=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._4;});
$UHC.$Base.$maxBound=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$UHC.$Base.$__78__3862__0=
 new _F_(function($__,$__2,$_24x__75__24__0)
         {var $__4=
           new _A_($UHC.$Base.$maxBound,[$__]);
          return new _A_($UHC.$Base.$_2f_3d,[$__2,$_24x__75__24__0,$__4]);});
$UHC.$Base.$succ=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._7;});
$UHC.$Base.$__78__3858__0=
 new _F_(function($__,$__2,$__3,$n)
         {var $__5=
           new _A_($UHC.$Base.$succ,[$__2]);
          var $__6=
           new _A_($UHC.$Base.$iterate,[$__5,$n]);
          var $__7=
           new _A_($UHC.$Base.$__78__3862__0,[$__,$__3]);
          return new _A_($UHC.$Base.$takeWhile1,[$__7,$__6]);});
$UHC.$Base.$__76__26692__2__0NEW1767UNQ7014=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._5;});
$UHC.$Base.$boundedEnumFrom=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           new _A_($UHC.$Base.$__76__26692__2__0NEW1767UNQ7014,[$__]);
          return new _A_($UHC.$Base.$__78__3858__0,[$__2,$__3,$__4]);});
$UHC.$Base.$toInteger=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._10;});
$UHC.$Base.$primQuotInteger=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return $__3.divide($__4);});
$UHC.$Base.$primModInteger=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primModInteger($__3,$__4);});
$UHC.$Base.$primDivInteger=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primDivInteger($__3,$__4);});
$UHC.$Base.$primSubInteger=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return $__3.subtract($__4);});
$UHC.$Base.$_2b=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._2;});
$UHC.$Base.$__78__5158__0=
 new _F_(function($__,$delta,$_24x__75__33__0)
         {return new _A_($UHC.$Base.$_2b,[$__,$_24x__75__33__0,$delta]);});
$UHC.$Base.$__78__5181__0=
 new _F_(function($__,$__2,$m,$delta,$_24x__75__30__0)
         {var $__6=
           new _A_($UHC.$Base.$_2d,[$__2,$m,$delta]);
          return new _A_($UHC.$Base.$_3e_3d,[$__,$_24x__75__30__0,$__6]);});
$UHC.$Base.$iterate=
 new _F_(function($f,$x)
         {var $__=
           new _A_($f,[$x]);
          var $__4=
           new _A_($UHC.$Base.$iterate,[$f,$__]);
          return new _A_($UHC.$Base.$_3a,[$x,$__4]);});
$UHC.$Base.$__78__2091NEW800=
 new _F_(function($p,$x,$xs)
         {var $__=
           new _A_($p,[$x]);
          var $__5=
           _e_($__);
          var $__swJSW771__0;
          switch($__5._tag_)
           {case 0:
             $__swJSW771__0=
              $UHC.$Base.$_5b_5d;
             break;
            case 1:
             var $__6=
              new _A_($UHC.$Base.$takeWhile1,[$p,$xs]);
             $__swJSW771__0=
              $__6;
             break;}
          return $__swJSW771__0;});
$UHC.$Base.$takeWhile1=
 new _F_(function($p,$__)
         {var $__3=
           _e_($__);
          var $__swJSW772__0;
          switch($__3._tag_)
           {case 0:
             var $__6=
              new _A_($UHC.$Base.$__78__2091NEW800,[$p,$__3._1,$__3._2]);
             var $__7=
              new _A_($UHC.$Base.$_3a,[$__3._1,$__6]);
             $__swJSW772__0=
              $__7;
             break;
            case 1:
             $__swJSW772__0=
              $UHC.$Base.$undefined;
             break;}
          return $__swJSW772__0;});
$UHC.$Base.$_2d=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._3;});
$UHC.$Base.$__78__5197__0=
 new _F_(function($__,$__2,$m,$delta,$_24x__75__29__0)
         {var $__6=
           new _A_($UHC.$Base.$_2d,[$__2,$m,$delta]);
          return new _A_($UHC.$Base.$_3c_3d,[$__,$_24x__75__29__0,$__6]);});
$UHC.$Base.$boundedEnumFromThenTo=
 new _F_(function($__,$__2,$__3,$__4,$n,$n_27,$m)
         {var $delta=
           new _A_($UHC.$Base.$_2d,[$__2,$n_27,$n]);
          var $__9=
           new _A_($UHC.$Base.$__78__5158__0,[$__2,$delta]);
          var $ns=
           new _A_($UHC.$Base.$iterate,[$__9,$n]);
          var $__11=
           new _A_($UHC.$Base.$_3e_3d,[$__,$n_27,$n]);
          var $__12=
           _e_($__11);
          var $__swJSW774__0;
          switch($__12._tag_)
           {case 0:
             var $__13=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW775__0;
             switch($__13._tag_)
              {case 0:
                var $__14=
                 new _A_($UHC.$Base.$packedStringToString,["FAIL 75_28_0"]);
                var $__15=
                 new _A_($UHC.$Base.$error,[$__14]);
                $__swJSW775__0=
                 $__15;
                break;
               case 1:
                var $__16=
                 new _A_($UHC.$Base.$_3e_3d,[$__,$n,$m]);
                var $__17=
                 _e_($__16);
                var $__swJSW776__0;
                switch($__17._tag_)
                 {case 0:
                   $__swJSW776__0=
                    $UHC.$Base.$_5b_5d;
                   break;
                  case 1:
                   var $__18=
                    new _A_($UHC.$Base.$__78__5181__0,[$__,$__2,$m,$delta]);
                   var $__19=
                    new _A_($UHC.$Base.$takeWhile1,[$__18,$ns]);
                   $__swJSW776__0=
                    $__19;
                   break;}
                $__swJSW775__0=
                 $__swJSW776__0;
                break;}
             $__swJSW774__0=
              $__swJSW775__0;
             break;
            case 1:
             var $__20=
              new _A_($UHC.$Base.$_3c_3d,[$__,$n,$m]);
             var $__21=
              _e_($__20);
             var $__swJSW777__0;
             switch($__21._tag_)
              {case 0:
                $__swJSW777__0=
                 $UHC.$Base.$_5b_5d;
                break;
               case 1:
                var $__22=
                 new _A_($UHC.$Base.$__78__5197__0,[$__,$__2,$m,$delta]);
                var $__23=
                 new _A_($UHC.$Base.$takeWhile1,[$__22,$ns]);
                $__swJSW777__0=
                 $__23;
                break;}
             $__swJSW774__0=
              $__swJSW777__0;
             break;}
          return $__swJSW774__0;});
$UHC.$Base.$signum=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._9;});
$UHC.$Base.$pNEW432UNQ1977CCN=
 new _F_(function($x1,$x2)
         {var $x23=
           _e_($x2);
          var $__swJSW779__0;
          switch($x23._tag_)
           {case 0:
             var $__=
              new _A_($x1,[$x23._1]);
             var $__7=
              _e_($__);
             var $__swJSW780__0;
             switch($__7._tag_)
              {case 0:
                var $__8=
                 _e_($UHC.$Base.$otherwise);
                var $__swJSW781__0;
                switch($__8._tag_)
                 {case 0:
                   $__swJSW781__0=
                    $UHC.$Base.$undefined;
                   break;
                  case 1:
                   $__swJSW781__0=
                    $UHC.$Base.$_5b_5d;
                   break;}
                $__swJSW780__0=
                 $__swJSW781__0;
                break;
               case 1:
                var $__9=
                 new _A_($UHC.$Base.$takeWhile,[$x1,$x23._2]);
                var $__10=
                 new _A_($UHC.$Base.$_3a,[$x23._1,$__9]);
                $__swJSW780__0=
                 $__10;
                break;}
             $__swJSW779__0=
              $__swJSW780__0;
             break;
            case 1:
             $__swJSW779__0=
              $UHC.$Base.$undefined;
             break;}
          return $__swJSW779__0;});
$UHC.$Base.$takeWhile=
 new _F_(function($x1,$x2)
         {var $p=
           new _A_($UHC.$Base.$pNEW432UNQ1977CCN,[$x1,$x2]);
          var $x24=
           _e_($x2);
          var $__swJSW782__0;
          switch($x24._tag_)
           {case 0:
             $__swJSW782__0=
              $p;
             break;
            case 1:
             $__swJSW782__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW782__0;});
$UHC.$Base.$primDivModInt=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primDivModInt($__3,$__4);});
$UHC.$Base.$fromEnum=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._5;});
$UHC.$Base.$primQuotInt=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primQuotInt($__3,$__4);});
$UHC.$Base.$primLeInt=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primLeInt($__3,$__4);});
$UHC.$Base.$primGeInt=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primGeInt($__3,$__4);});
$UHC.$Base.$primCmpInt=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primCmpInt($__3,$__4);});
$UHC.$Base.$primGtInt=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primGtInt($__3,$__4);});
$UHC.$Base.$primLtInt=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primLtInt($__3,$__4);});
$UHC.$Base.$primNeInt=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primNeInt($__3,$__4);});
$UHC.$Base.$primEqInt=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primEqInt($__3,$__4);});
$UHC.$Base.$Eq__NEW1762UNQ10107EVLDCT74__88__0RDC=
 new _F_(function($Eq__)
         {var $Eq__2=
           _e_(new _A_($UHC.$Base.$Eq__CLS74__4__0,[$Eq__]));
          var $__5=
           {_tag_:0,_1:$UHC.$Base.$primNeInt,_2:$UHC.$Base.$primEqInt};
          return $__5;});
$UHC.$Base.$Eq__NEW1760UNQ10106DCT74__88__0RDC=
 new _F_(function($Eq__)
         {var $Eq__2=
           new _A_($UHC.$Base.$Eq__NEW1762UNQ10107EVLDCT74__88__0RDC,[$Eq__]);
          return $Eq__2;});
$UHC.$Base.$Eq__UNQ10106DCT74__88__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq__NEW1760UNQ10106DCT74__88__0RDC,[$UHC.$Base.$Eq__UNQ10106DCT74__88__0RDC]);}),[]);
$UHC.$Base.$Eq__DCT74__88__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Eq__UNQ10106DCT74__88__0RDC;}),[]);
$UHC.$Base.$Ord__NEW2180UNQ10847EVLDCT74__91__0RDC=
 new _F_(function($Ord__)
         {var $Ord__2=
           _e_(new _A_($UHC.$Base.$Ord__CLS74__5__0,[$Ord__]));
          var $__11=
           {_tag_:0,_1:$UHC.$Base.$primLtInt,_2:$UHC.$Base.$primLeInt,_3:$UHC.$Base.$primGtInt,_4:$UHC.$Base.$primGeInt,_5:$UHC.$Base.$Eq__DCT74__88__0,_6:$UHC.$Base.$primCmpInt,_7:$Ord__2._7,_8:$Ord__2._8};
          return $__11;});
$UHC.$Base.$Ord__NEW2178UNQ10846DCT74__91__0RDC=
 new _F_(function($Ord__)
         {var $Ord__2=
           new _A_($UHC.$Base.$Ord__NEW2180UNQ10847EVLDCT74__91__0RDC,[$Ord__]);
          return $Ord__2;});
$UHC.$Base.$Ord__UNQ10846DCT74__91__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Ord__NEW2178UNQ10846DCT74__91__0RDC,[$UHC.$Base.$Ord__UNQ10846DCT74__91__0RDC]);}),[]);
$UHC.$Base.$Ord__DCT74__91__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Ord__UNQ10846DCT74__91__0RDC;}),[]);
$UHC.$Base.$primCmpInteger=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primCmpInteger($__3,$__4);});
$UHC.$Base.$primEqInteger=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primEqInteger($__3,$__4);});
$UHC.$Base.$Eq__NEW1748UNQ10097EVLDCT74__130__0RDC=
 new _F_(function($Eq__)
         {var $Eq__2=
           _e_(new _A_($UHC.$Base.$Eq__CLS74__4__0,[$Eq__]));
          var $__5=
           {_tag_:0,_1:$Eq__2._1,_2:$UHC.$Base.$primEqInteger};
          return $__5;});
$UHC.$Base.$Eq__NEW1746UNQ10096DCT74__130__0RDC=
 new _F_(function($Eq__)
         {var $Eq__2=
           new _A_($UHC.$Base.$Eq__NEW1748UNQ10097EVLDCT74__130__0RDC,[$Eq__]);
          return $Eq__2;});
$UHC.$Base.$Eq__UNQ10096DCT74__130__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq__NEW1746UNQ10096DCT74__130__0RDC,[$UHC.$Base.$Eq__UNQ10096DCT74__130__0RDC]);}),[]);
$UHC.$Base.$Eq__DCT74__130__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Eq__UNQ10096DCT74__130__0RDC;}),[]);
$UHC.$Base.$Ord__NEW2138UNQ10760EVLDCT74__132__0RDC=
 new _F_(function($Ord__)
         {var $Ord__2=
           _e_(new _A_($UHC.$Base.$Ord__CLS74__5__0,[$Ord__]));
          var $__11=
           {_tag_:0,_1:$Ord__2._1,_2:$Ord__2._2,_3:$Ord__2._3,_4:$Ord__2._4,_5:$UHC.$Base.$Eq__DCT74__130__0,_6:$UHC.$Base.$primCmpInteger,_7:$Ord__2._7,_8:$Ord__2._8};
          return $__11;});
$UHC.$Base.$Ord__NEW2136UNQ10759DCT74__132__0RDC=
 new _F_(function($Ord__)
         {var $Ord__2=
           new _A_($UHC.$Base.$Ord__NEW2138UNQ10760EVLDCT74__132__0RDC,[$Ord__]);
          return $Ord__2;});
$UHC.$Base.$Ord__UNQ10759DCT74__132__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Ord__NEW2136UNQ10759DCT74__132__0RDC,[$UHC.$Base.$Ord__UNQ10759DCT74__132__0RDC]);}),[]);
$UHC.$Base.$Ord__DCT74__132__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Ord__UNQ10759DCT74__132__0RDC;}),[]);
$UHC.$Base.$enumFromThenTo=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._3;});
$UHC.$Base.$primRemInteger=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return $__3.remainder($__4);});
$UHC.$Base.$toEnum=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._8;});
$UHC.$Base.$Num__DCT74__134__0DFLUHC_2eBase_2efromInteger=
 new _F_(function($x)
         {return $x;});
$UHC.$Base.$Num__NEW4659UNQ10649DCT74__134__0RDC=
 new _F_(function($Num__,$Num__2,$Num__3)
         {var $Num__4=
           new _A_($UHC.$Base.$Num__NEW4663UNQ10654EVLDCT74__134__0RDC,[$Num__,$Num__2,$Num__3]);
          return $Num__4;});
$UHC.$Base.$Num__NEW4663UNQ10654EVLDCT74__134__0RDC=
 new _F_(function($Num__,$Num__2,$Num__3)
         {var $Num__4=
           _e_(new _A_($UHC.$Base.$Num__CLS74__11__0,[$Num__2]));
          var $__14=
           {_tag_:0,_1:$UHC.$Base.$primMulInteger,_2:$UHC.$Base.$primAddInteger,_3:$UHC.$Base.$primSubInteger,_4:$UHC.$Base.$Eq__DCT74__130__0,_5:$Num__,_6:$UHC.$Base.$primIntToInteger,_7:$UHC.$Base.$Num__DCT74__134__0DFLUHC_2eBase_2efromInteger,_8:$UHC.$Base.$primNegInteger,_9:$Num__3};
          return $__14;});
$UHC.$Base.$Num__DCT74__134__0DFLUHC_2eBase_2eabs=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$absReal,[$UHC.$Base.$Ord__DCT74__132__0,$UHC.$Base.$Num__UNQ10649DCT74__134__0RDC]);}),[]);
$UHC.$Base.$Num__UNQ10649DCT74__134__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Num__NEW4659UNQ10649DCT74__134__0RDC,[$UHC.$Base.$Num__DCT74__134__0DFLUHC_2eBase_2eabs,$UHC.$Base.$Num__UNQ10649DCT74__134__0RDC,$UHC.$Base.$Num__DCT74__134__0DFLUHC_2eBase_2esignum]);}),[]);
$UHC.$Base.$Num__DCT74__134__0DFLUHC_2eBase_2esignum=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$signumReal,[$UHC.$Base.$Ord__DCT74__132__0,$UHC.$Base.$Num__UNQ10649DCT74__134__0RDC]);}),[]);
$UHC.$Base.$Num__CLS74__11__0DFLUHC_2eBase_2e_2d=
 new _F_(function($Num__,$x,$y)
         {var $__=
           new _A_($UHC.$Base.$negate,[$Num__,$y]);
          return new _A_($UHC.$Base.$_2b,[$Num__,$x,$__]);});
$UHC.$Base.$Integral__DCT74__110__0DFLUHC_2eBase_2etoInt=
 new _F_(function($x)
         {return $x;});
$UHC.$Base.$Integral__NEW4566UNQ10882DCT74__110__0RDC=
 new _F_(function($Integral__)
         {var $Integral__2=
           new _A_($UHC.$Base.$Integral__NEW4568UNQ10883EVLDCT74__110__0RDC,[$Integral__]);
          return $Integral__2;});
$UHC.$Base.$Integral__NEW4568UNQ10883EVLDCT74__110__0RDC=
 new _F_(function($Integral__)
         {var $Integral__2=
           _e_(new _A_($UHC.$Base.$Integral__CLS74__14__0,[$Integral__]));
          var $__13=
           {_tag_:0,_1:$UHC.$Base.$Enum__DCT74__118__0,_2:$UHC.$Base.$Real__DCT74__100__0,_3:$UHC.$Base.$primDivInt,_4:$UHC.$Base.$primDivModInt,_5:$UHC.$Base.$primModInt,_6:$UHC.$Base.$primQuotInt,_7:$UHC.$Base.$primQuotRemInt,_8:$UHC.$Base.$primRemInt,_9:$UHC.$Base.$Integral__DCT74__110__0DFLUHC_2eBase_2etoInt,_10:$UHC.$Base.$primIntToInteger};
          return $__13;});
$UHC.$Base.$Integral__UNQ10882DCT74__110__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Integral__NEW4566UNQ10882DCT74__110__0RDC,[$UHC.$Base.$Integral__UNQ10882DCT74__110__0RDC]);}),[]);
$UHC.$Base.$__76__17806__2__2NEW4573UNQ4659=
 new _F_(function($Integral__)
         {var $Real__=
           _e_($Integral__);
          return $Real__._2;});
$UHC.$Base.$Integral__CLS74__14__0DFLUHC_2eBase_2erem=
 new _F_(function($Integral__,$n,$d)
         {var $__=
           new _A_($UHC.$Base.$quotRem,[$Integral__,$n,$d]);
          var $q=
           new _A_($UHC.$Base.$qNEW4578UNQ4751,[$__]);
          var $r=
           new _A_($UHC.$Base.$rNEW4581UNQ4752,[$__]);
          return $r;});
$UHC.$Base.$qNEW4578UNQ4751=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Base.$rNEW4581UNQ4752=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Base.$Integral__CLS74__14__0DFLUHC_2eBase_2ediv=
 new _F_(function($Integral__,$n,$d)
         {var $__=
           new _A_($UHC.$Base.$divMod,[$Integral__,$n,$d]);
          var $q=
           new _A_($UHC.$Base.$qNEW4586UNQ4686,[$__]);
          var $r=
           new _A_($UHC.$Base.$rNEW4589UNQ4687,[$__]);
          return $q;});
$UHC.$Base.$qNEW4586UNQ4686=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Base.$rNEW4589UNQ4687=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Base.$Integral__CLS74__14__0DFLUHC_2eBase_2emod=
 new _F_(function($Integral__,$n,$d)
         {var $__=
           new _A_($UHC.$Base.$divMod,[$Integral__,$n,$d]);
          var $r=
           new _A_($UHC.$Base.$rNEW4597UNQ4722,[$__]);
          var $q=
           new _A_($UHC.$Base.$qNEW4600UNQ4721,[$__]);
          return $r;});
$UHC.$Base.$rNEW4597UNQ4722=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Base.$qNEW4600UNQ4721=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Base.$__76__18135__2__0NEW4603UNQ4661=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$UHC.$Base.$__76__18058__2__0NEW4606UNQ4666=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$UHC.$Base.$Integral__CLS74__14__0DFLUHC_2eBase_2edivMod=
 new _F_(function($__,$__2,$Integral__,$n,$d)
         {var $qr=
           new _A_($UHC.$Base.$quotRem,[$Integral__,$n,$d]);
          var $r=
           new _A_($UHC.$Base.$rNEW4611UNQ4705,[$qr]);
          var $q=
           new _A_($UHC.$Base.$qNEW4614UNQ4704,[$qr]);
          var $__9=
           new _A_($UHC.$Base.$signum,[$__,$d]);
          var $__10=
           new _A_($UHC.$Base.$negate,[$__,$__9]);
          var $__11=
           new _A_($UHC.$Base.$signum,[$__,$r]);
          var $__12=
           new _A_($UHC.$Base.$_3d_3d,[$__2,$__11,$__10]);
          var $__13=
           _e_($__12);
          var $__swJSW801__0;
          switch($__13._tag_)
           {case 0:
             $__swJSW801__0=
              $qr;
             break;
            case 1:
             var $__14=
              new _A_($UHC.$Base.$_2b,[$__,$r,$d]);
             var $__15=
              new _A_($UHC.$Base.$packedStringToInteger,["1"]);
             var $__16=
              new _A_($UHC.$Base.$fromInteger,[$__,$__15]);
             var $__17=
              new _A_($UHC.$Base.$_2d,[$__,$q,$__16]);
             var $__18=
              [$__17,$__14];
             $__swJSW801__0=
              $__18;
             break;}
          return $__swJSW801__0;});
$UHC.$Base.$rNEW4611UNQ4705=
 new _F_(function($qr)
         {var $qr2=
           _e_($qr);
          return $qr2[1];});
$UHC.$Base.$qNEW4614UNQ4704=
 new _F_(function($qr)
         {var $qr2=
           _e_($qr);
          return $qr2[0];});
$UHC.$Base.$Integral__CLS74__14__0DFLUHC_2eBase_2equot=
 new _F_(function($Integral__,$n,$d)
         {var $__=
           new _A_($UHC.$Base.$quotRem,[$Integral__,$n,$d]);
          var $q=
           new _A_($UHC.$Base.$qNEW4629UNQ4736,[$__]);
          var $r=
           new _A_($UHC.$Base.$rNEW4632UNQ4737,[$__]);
          return $q;});
$UHC.$Base.$qNEW4629UNQ4736=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$UHC.$Base.$rNEW4632UNQ4737=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$UHC.$Base.$Integral__DCT74__143__0DFLUHC_2eBase_2etoInteger=
 new _F_(function($x)
         {return $x;});
$UHC.$Base.$Integral__NEW4638UNQ10868DCT74__143__0RDC=
 new _F_(function($Integral__)
         {var $Integral__2=
           new _A_($UHC.$Base.$Integral__NEW4640UNQ10869EVLDCT74__143__0RDC,[$Integral__]);
          return $Integral__2;});
$UHC.$Base.$Integral__NEW4640UNQ10869EVLDCT74__143__0RDC=
 new _F_(function($Integral__)
         {var $Integral__2=
           _e_(new _A_($UHC.$Base.$Integral__CLS74__14__0,[$Integral__]));
          var $__13=
           {_tag_:0,_1:$UHC.$Base.$Enum__DCT74__151__0,_2:$UHC.$Base.$Real__DCT74__142__0,_3:$UHC.$Base.$primDivInteger,_4:$UHC.$Base.$primDivModInteger,_5:$UHC.$Base.$primModInteger,_6:$UHC.$Base.$primQuotInteger,_7:$UHC.$Base.$primQuotRemInteger,_8:$UHC.$Base.$primRemInteger,_9:$UHC.$Base.$primIntegerToInt,_10:$UHC.$Base.$Integral__DCT74__143__0DFLUHC_2eBase_2etoInteger};
          return $__13;});
$UHC.$Base.$Integral__UNQ10868DCT74__143__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Integral__NEW4638UNQ10868DCT74__143__0RDC,[$UHC.$Base.$Integral__UNQ10868DCT74__143__0RDC]);}),[]);
$UHC.$Base.$Enum__DCT74__151__0DFLUHC_2eBase_2eenumFromThenTo=
 new _F_(function($n,$n2,$m)
         {var $p=
           new _A_($UHC.$Base.$pNEW4672UNQ11353,[$n,$n2,$m]);
          var $__=
           new _A_($UHC.$Base.$numericEnumFromThen,[$UHC.$Base.$Num__DCT74__134__0,$n,$n2]);
          return new _A_($UHC.$Base.$takeWhile,[$p,$__]);});
$UHC.$Base.$pNEW4672UNQ11353=
 new _F_(function($n,$n2,$m)
         {var $__=
           new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__132__0,$n2,$n]);
          var $__5=
           _e_($__);
          var $__swJSW807__0;
          switch($__5._tag_)
           {case 0:
             var $__6=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW808__0;
             switch($__6._tag_)
              {case 0:
                var $__7=
                 new _A_($UHC.$Base.$packedStringToString,["FAIL 75_308_0"]);
                var $__8=
                 new _A_($UHC.$Base.$error,[$__7]);
                $__swJSW808__0=
                 $__8;
                break;
               case 1:
                $__swJSW808__0=
                 new _A_($UHC.$Base.$__78__9200__0,[$m]);
                break;}
             $__swJSW807__0=
              $__swJSW808__0;
             break;
            case 1:
             $__swJSW807__0=
              new _A_($UHC.$Base.$__78__9205__0,[$m]);
             break;}
          return $__swJSW807__0;});
$UHC.$Base.$__78__9200__0=
 new _F_(function($m,$_24x__75__310__0)
         {return new _A_($UHC.$Base.$_3e_3d,[$UHC.$Base.$Ord__DCT74__132__0,$_24x__75__310__0,$m]);});
$UHC.$Base.$__78__9205__0=
 new _F_(function($m,$_24x__75__309__0)
         {return new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__132__0,$_24x__75__309__0,$m]);});
$UHC.$Base.$Enum__DCT74__151__0DFLUHC_2eBase_2esucc=
 new _F_(function($x)
         {var $__=
           new _A_($UHC.$Base.$primIntToInteger,[1]);
          return new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__134__0,$x,$__]);});
$UHC.$Base.$Enum__DCT74__151__0DFLUHC_2eBase_2eenumFromThen=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$numericEnumFromThen,[$UHC.$Base.$Num__DCT74__134__0]);}),[]);
$UHC.$Base.$Enum__DCT74__151__0DFLUHC_2eBase_2epred=
 new _F_(function($x)
         {var $__=
           new _A_($UHC.$Base.$primIntToInteger,[1]);
          return new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__134__0,$x,$__]);});
$UHC.$Base.$Enum__DCT74__151__0DFLUHC_2eBase_2eenumFrom=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$numericEnumFrom,[$UHC.$Base.$Num__DCT74__134__0]);}),[]);
$UHC.$Base.$Enum__DCT74__151__0DFLUHC_2eBase_2eenumFromTo=
 new _F_(function($n,$m)
         {var $__=
           new _A_($UHC.$Base.$numericEnumFrom,[$UHC.$Base.$Num__DCT74__134__0,$n]);
          var $__4=
           new _A_($UHC.$Base.$__78__9240__0,[$m]);
          return new _A_($UHC.$Base.$takeWhile,[$__4,$__]);});
$UHC.$Base.$__78__9240__0=
 new _F_(function($m,$_24x__75__306__0)
         {return new _A_($UHC.$Base.$_3c_3d,[$UHC.$Base.$Ord__DCT74__132__0,$_24x__75__306__0,$m]);});
$UHC.$Base.$Enum__NEW4693UNQ11318DCT74__151__0RDC=
 new _F_(function($Enum__,$Enum__2,$Enum__3)
         {var $Enum__4=
           new _A_($UHC.$Base.$Enum__NEW4697UNQ11328EVLDCT74__151__0RDC,[$Enum__,$Enum__2,$Enum__3]);
          return $Enum__4;});
$UHC.$Base.$Enum__NEW4697UNQ11328EVLDCT74__151__0RDC=
 new _F_(function($Enum__,$Enum__2,$Enum__3)
         {var $Enum__4=
           _e_(new _A_($UHC.$Base.$Enum__CLS74__38__0,[$Enum__]));
          var $__13=
           {_tag_:0,_1:$Enum__2,_2:$Enum__3,_3:$UHC.$Base.$Enum__DCT74__151__0DFLUHC_2eBase_2eenumFromThenTo,_4:$UHC.$Base.$Enum__DCT74__151__0DFLUHC_2eBase_2eenumFromTo,_5:$UHC.$Base.$primIntegerToInt,_6:$UHC.$Base.$Enum__DCT74__151__0DFLUHC_2eBase_2epred,_7:$UHC.$Base.$Enum__DCT74__151__0DFLUHC_2eBase_2esucc,_8:$UHC.$Base.$primIntToInteger};
          return $__13;});
$UHC.$Base.$Enum__UNQ11318DCT74__151__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Enum__NEW4693UNQ11318DCT74__151__0RDC,[$UHC.$Base.$Enum__UNQ11318DCT74__151__0RDC,$UHC.$Base.$Enum__DCT74__151__0DFLUHC_2eBase_2eenumFrom,$UHC.$Base.$Enum__DCT74__151__0DFLUHC_2eBase_2eenumFromThen]);}),[]);
$UHC.$Base.$Enum__CLS74__38__0DFLUHC_2eBase_2eenumFrom=
 new _F_(function($Enum__,$x)
         {var $__=
           new _A_($UHC.$Base.$fromEnum,[$Enum__,$x]);
          var $__4=
           new _A_($UHC.$Base.$enumFrom,[$UHC.$Base.$Enum__DCT74__118__0,$__]);
          var $__5=
           new _A_($UHC.$Base.$toEnum,[$Enum__]);
          return new _A_($UHC.$Base.$map,[$__5,$__4]);});
$UHC.$Base.$Enum__CLS74__38__0DFLUHC_2eBase_2eenumFromThen=
 new _F_(function($Enum__,$x,$y)
         {var $__=
           new _A_($UHC.$Base.$fromEnum,[$Enum__,$y]);
          var $__5=
           new _A_($UHC.$Base.$fromEnum,[$Enum__,$x]);
          var $__6=
           new _A_($UHC.$Base.$enumFromThen,[$UHC.$Base.$Enum__DCT74__118__0,$__5,$__]);
          var $__7=
           new _A_($UHC.$Base.$toEnum,[$Enum__]);
          return new _A_($UHC.$Base.$map,[$__7,$__6]);});
$UHC.$Base.$Enum__CLS74__38__0DFLUHC_2eBase_2eenumFromThenTo=
 new _F_(function($Enum__,$x,$y,$z)
         {var $__=
           new _A_($UHC.$Base.$fromEnum,[$Enum__,$z]);
          var $__6=
           new _A_($UHC.$Base.$fromEnum,[$Enum__,$y]);
          var $__7=
           new _A_($UHC.$Base.$fromEnum,[$Enum__,$x]);
          var $__8=
           new _A_($UHC.$Base.$enumFromThenTo,[$UHC.$Base.$Enum__DCT74__118__0,$__7,$__6,$__]);
          var $__9=
           new _A_($UHC.$Base.$toEnum,[$Enum__]);
          return new _A_($UHC.$Base.$map,[$__9,$__8]);});
$UHC.$Base.$Enum__CLS74__38__0DFLUHC_2eBase_2eenumFromTo=
 new _F_(function($Enum__,$x,$y)
         {var $__=
           new _A_($UHC.$Base.$fromEnum,[$Enum__,$y]);
          var $__5=
           new _A_($UHC.$Base.$fromEnum,[$Enum__,$x]);
          var $__6=
           new _A_($UHC.$Base.$enumFromTo,[$UHC.$Base.$Enum__DCT74__118__0,$__5,$__]);
          var $__7=
           new _A_($UHC.$Base.$toEnum,[$Enum__]);
          return new _A_($UHC.$Base.$map,[$__7,$__6]);});
$UHC.$Base.$Enum__NEW4520UNQ10418DCT74__118__0RDC=
 new _F_(function($Enum__,$Enum__2,$Enum__3,$Enum__4,$Enum__5,$Enum__6,$Enum__7)
         {var $Enum__8=
           new _A_($UHC.$Base.$Enum__NEW4528UNQ10438EVLDCT74__118__0RDC,[$Enum__,$Enum__2,$Enum__3,$Enum__4,$Enum__5,$Enum__6,$Enum__7]);
          return $Enum__8;});
$UHC.$Base.$Enum__NEW4528UNQ10438EVLDCT74__118__0RDC=
 new _F_(function($Enum__,$Enum__2,$Enum__3,$Enum__4,$Enum__5,$Enum__6,$Enum__7)
         {var $Enum__8=
           _e_(new _A_($UHC.$Base.$Enum__CLS74__38__0,[$Enum__5]));
          var $__17=
           {_tag_:0,_1:$Enum__7,_2:$Enum__,_3:$Enum__3,_4:$Enum__6,_5:$UHC.$Base.$id,_6:$Enum__2,_7:$Enum__4,_8:$UHC.$Base.$id};
          return $__17;});
$UHC.$Base.$Enum__DCT74__118__0DFLUHC_2eBase_2eenumFromThen=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$boundedEnumFromThen,[$UHC.$Base.$Ord__DCT74__91__0,$UHC.$Base.$Bounded__DCT74__97__0,$UHC.$Base.$Enum__UNQ10418DCT74__118__0RDC]);}),[]);
$UHC.$Base.$Enum__UNQ10418DCT74__118__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Enum__NEW4520UNQ10418DCT74__118__0RDC,[$UHC.$Base.$Enum__DCT74__118__0DFLUHC_2eBase_2eenumFromThen,$UHC.$Base.$Enum__DCT74__118__0DFLUHC_2eBase_2epred,$UHC.$Base.$Enum__DCT74__118__0DFLUHC_2eBase_2eenumFromThenTo,$UHC.$Base.$Enum__DCT74__118__0DFLUHC_2eBase_2esucc,$UHC.$Base.$Enum__UNQ10418DCT74__118__0RDC,$UHC.$Base.$Enum__DCT74__118__0DFLUHC_2eBase_2eenumFromTo,$UHC.$Base.$Enum__DCT74__118__0DFLUHC_2eBase_2eenumFrom]);}),[]);
$UHC.$Base.$Enum__DCT74__118__0DFLUHC_2eBase_2epred=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$boundedPred,[$UHC.$Base.$Num__DCT74__101__0,$UHC.$Base.$Bounded__DCT74__97__0,$UHC.$Base.$Enum__UNQ10418DCT74__118__0RDC]);}),[]);
$UHC.$Base.$Enum__DCT74__118__0DFLUHC_2eBase_2eenumFromThenTo=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$boundedEnumFromThenTo,[$UHC.$Base.$Ord__DCT74__91__0,$UHC.$Base.$Num__DCT74__101__0,$UHC.$Base.$Bounded__DCT74__97__0,$UHC.$Base.$Enum__UNQ10418DCT74__118__0RDC]);}),[]);
$UHC.$Base.$Enum__DCT74__118__0DFLUHC_2eBase_2esucc=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$boundedSucc,[$UHC.$Base.$Num__DCT74__101__0,$UHC.$Base.$Bounded__DCT74__97__0,$UHC.$Base.$Enum__UNQ10418DCT74__118__0RDC]);}),[]);
$UHC.$Base.$Enum__DCT74__118__0DFLUHC_2eBase_2eenumFromTo=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$boundedEnumFromTo,[$UHC.$Base.$Ord__DCT74__91__0,$UHC.$Base.$Bounded__DCT74__97__0,$UHC.$Base.$Enum__UNQ10418DCT74__118__0RDC]);}),[]);
$UHC.$Base.$Enum__DCT74__118__0DFLUHC_2eBase_2eenumFrom=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$boundedEnumFrom,[$UHC.$Base.$Ord__DCT74__91__0,$UHC.$Base.$Bounded__DCT74__97__0,$UHC.$Base.$Enum__UNQ10418DCT74__118__0RDC]);}),[]);
$UHC.$Base.$Num__NEW4544UNQ10666DCT74__101__0RDC=
 new _F_(function($Num__,$Num__2,$Num__3)
         {var $Num__4=
           new _A_($UHC.$Base.$Num__NEW4548UNQ10671EVLDCT74__101__0RDC,[$Num__,$Num__2,$Num__3]);
          return $Num__4;});
$UHC.$Base.$Num__NEW4548UNQ10671EVLDCT74__101__0RDC=
 new _F_(function($Num__,$Num__2,$Num__3)
         {var $Num__4=
           _e_(new _A_($UHC.$Base.$Num__CLS74__11__0,[$Num__]));
          var $__14=
           {_tag_:0,_1:$UHC.$Base.$primMulInt,_2:$UHC.$Base.$primAddInt,_3:$UHC.$Base.$primSubInt,_4:$UHC.$Base.$Eq__DCT74__88__0,_5:$Num__3,_6:$UHC.$Base.$id,_7:$UHC.$Base.$primIntegerToInt,_8:$UHC.$Base.$primNegInt,_9:$Num__2};
          return $__14;});
$UHC.$Base.$Num__UNQ10666DCT74__101__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Num__NEW4544UNQ10666DCT74__101__0RDC,[$UHC.$Base.$Num__UNQ10666DCT74__101__0RDC,$UHC.$Base.$Num__DCT74__101__0DFLUHC_2eBase_2esignum,$UHC.$Base.$Num__DCT74__101__0DFLUHC_2eBase_2eabs]);}),[]);
$UHC.$Base.$Num__DCT74__101__0DFLUHC_2eBase_2esignum=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$signumReal,[$UHC.$Base.$Ord__DCT74__91__0,$UHC.$Base.$Num__UNQ10666DCT74__101__0RDC]);}),[]);
$UHC.$Base.$Num__DCT74__101__0DFLUHC_2eBase_2eabs=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$absReal,[$UHC.$Base.$Ord__DCT74__91__0,$UHC.$Base.$Num__UNQ10666DCT74__101__0RDC]);}),[]);
$UHC.$Base.$Real__DCT74__142__0DFLUHC_2eBase_2etoRational=
 new _F_(function($x)
         {var $__=
           new _A_($UHC.$Base.$primIntToInteger,[1]);
          return new _A_($UHC.$Base.$_25,[$UHC.$Base.$Integral__DCT74__143__0,$x,$__]);});
$UHC.$Base.$Real__NEW4647UNQ10911DCT74__142__0RDC=
 new _F_(function($Real__)
         {var $Real__2=
           new _A_($UHC.$Base.$Real__NEW4649UNQ10914EVLDCT74__142__0RDC,[$Real__]);
          return $Real__2;});
$UHC.$Base.$Real__NEW4649UNQ10914EVLDCT74__142__0RDC=
 new _F_(function($Real__)
         {var $Real__2=
           _e_(new _A_($UHC.$Base.$Real__CLS74__13__0,[$Real__]));
          var $__6=
           {_tag_:0,_1:$UHC.$Base.$Num__DCT74__134__0,_2:$UHC.$Base.$Ord__DCT74__132__0,_3:$UHC.$Base.$Real__DCT74__142__0DFLUHC_2eBase_2etoRational};
          return $__6;});
$UHC.$Base.$Real__UNQ10911DCT74__142__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Real__NEW4647UNQ10911DCT74__142__0RDC,[$UHC.$Base.$Real__UNQ10911DCT74__142__0RDC]);}),[]);
$UHC.$Base.$__76__46633__2__0NEW4653UNQ10912=
 new _F_(function($Real__)
         {var $Num__=
           _e_($Real__);
          return $Num__._1;});
$UHC.$Base.$__76__46633__2__0UNQ10912=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$__76__46633__2__0NEW4653UNQ10912,[$UHC.$Base.$Real__UNQ10911DCT74__142__0RDC]);}),[]);
$UHC.$Base.$Real__DCT74__100__0DFLUHC_2eBase_2etoRational=
 new _F_(function($x)
         {var $__=
           new _A_($UHC.$Base.$primIntToInteger,[1]);
          var $__3=
           new _A_($UHC.$Base.$toInteger,[$UHC.$Base.$Integral__DCT74__110__0,$x]);
          return new _A_($UHC.$Base.$_25,[$UHC.$Base.$Integral__DCT74__143__0,$__3,$__]);});
$UHC.$Base.$Real__NEW4707UNQ10936DCT74__100__0RDC=
 new _F_(function($Real__)
         {var $Real__2=
           new _A_($UHC.$Base.$Real__NEW4709UNQ10940EVLDCT74__100__0RDC,[$Real__]);
          return $Real__2;});
$UHC.$Base.$Real__NEW4709UNQ10940EVLDCT74__100__0RDC=
 new _F_(function($Real__)
         {var $Real__2=
           _e_(new _A_($UHC.$Base.$Real__CLS74__13__0,[$Real__]));
          var $__6=
           {_tag_:0,_1:$UHC.$Base.$Num__DCT74__101__0,_2:$UHC.$Base.$Ord__DCT74__91__0,_3:$UHC.$Base.$Real__DCT74__100__0DFLUHC_2eBase_2etoRational};
          return $__6;});
$UHC.$Base.$Real__UNQ10936DCT74__100__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Real__NEW4707UNQ10936DCT74__100__0RDC,[$UHC.$Base.$Real__UNQ10936DCT74__100__0RDC]);}),[]);
$UHC.$Base.$Num__DCT74__134__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Num__UNQ10649DCT74__134__0RDC;}),[]);
$UHC.$Base.$Integral__DCT74__110__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Integral__UNQ10882DCT74__110__0RDC;}),[]);
$UHC.$Base.$Integral__DCT74__143__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Integral__UNQ10868DCT74__143__0RDC;}),[]);
$UHC.$Base.$Enum__DCT74__151__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Enum__UNQ11318DCT74__151__0RDC;}),[]);
$UHC.$Base.$Enum__DCT74__118__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Enum__UNQ10418DCT74__118__0RDC;}),[]);
$UHC.$Base.$Num__DCT74__101__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Num__UNQ10666DCT74__101__0RDC;}),[]);
$UHC.$Base.$Real__DCT74__142__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Real__UNQ10911DCT74__142__0RDC;}),[]);
$UHC.$Base.$Real__DCT74__100__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Real__UNQ10936DCT74__100__0RDC;}),[]);
$UHC.$Base.$Num__CLS74__11__0=
 new _F_(function($Num__)
         {var $__=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__3=
           new _A_($UHC.$Base.$fromInteger,[$Num__,$__]);
          var $Num__CLS74__11__0DFLUHC_2eBase_2enegate=
           new _A_($UHC.$Base.$_2d,[$Num__,$__3]);
          var $Num__CLS74__11__0DFLUHC_2eBase_2efromInt=
           new _A_($UHC.$Base.$fromIntegral,[$UHC.$Base.$Integral__DCT74__110__0,$Num__]);
          var $__4=
           new _A_($UHC.$Base.$Num__CLS74__11__0DFLUHC_2eBase_2e_2d,[$Num__]);
          var $Num__5=
           {_tag_:0,_1:$UHC.$Base.$undefined,_2:$UHC.$Base.$undefined,_3:$__4,_4:$UHC.$Base.$undefined,_5:$UHC.$Base.$undefined,_6:$Num__CLS74__11__0DFLUHC_2eBase_2efromInt,_7:$UHC.$Base.$undefined,_8:$Num__CLS74__11__0DFLUHC_2eBase_2enegate,_9:$UHC.$Base.$undefined};
          return $Num__5;});
$UHC.$Base.$Integral__CLS74__14__0=
 new _F_(function($Integral__)
         {var $__=
           new _A_($UHC.$Base.$__76__17806__2__2NEW4573UNQ4659,[$Integral__]);
          var $__3=
           new _A_($UHC.$Base.$toInteger,[$Integral__]);
          var $__4=
           new _A_($UHC.$Base.$toInt,[$UHC.$Base.$Integral__DCT74__143__0]);
          var $Integral__CLS74__14__0DFLUHC_2eBase_2etoInt=
           new _A_($UHC.$Base.$_2e,[$__4,$__3]);
          var $__5=
           new _A_($UHC.$Base.$__76__18135__2__0NEW4603UNQ4661,[$__]);
          var $__6=
           new _A_($UHC.$Base.$__76__18058__2__0NEW4606UNQ4666,[$__5]);
          var $__7=
           new _A_($UHC.$Base.$Integral__CLS74__14__0DFLUHC_2eBase_2erem,[$Integral__]);
          var $__8=
           new _A_($UHC.$Base.$Integral__CLS74__14__0DFLUHC_2eBase_2equot,[$Integral__]);
          var $__9=
           new _A_($UHC.$Base.$Integral__CLS74__14__0DFLUHC_2eBase_2emod,[$Integral__]);
          var $__10=
           new _A_($UHC.$Base.$Integral__CLS74__14__0DFLUHC_2eBase_2edivMod,[$__5,$__6,$Integral__]);
          var $__11=
           new _A_($UHC.$Base.$Integral__CLS74__14__0DFLUHC_2eBase_2ediv,[$Integral__]);
          var $Integral__12=
           {_tag_:0,_1:$UHC.$Base.$undefined,_2:$UHC.$Base.$undefined,_3:$__11,_4:$__10,_5:$__9,_6:$__8,_7:$UHC.$Base.$undefined,_8:$__7,_9:$Integral__CLS74__14__0DFLUHC_2eBase_2etoInt,_10:$UHC.$Base.$undefined};
          return $Integral__12;});
$UHC.$Base.$Enum__CLS74__38__0=
 new _F_(function($Enum__)
         {var $__=
           new _A_($UHC.$Base.$fromEnum,[$Enum__]);
          var $__3=
           new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,1]);
          var $__4=
           new _A_($UHC.$Base.$_2e,[$__3,$__]);
          var $__5=
           new _A_($UHC.$Base.$toEnum,[$Enum__]);
          var $Enum__CLS74__38__0DFLUHC_2eBase_2esucc=
           new _A_($UHC.$Base.$_2e,[$__5,$__4]);
          var $__6=
           new _A_($UHC.$Base.$fromEnum,[$Enum__]);
          var $__7=
           new _A_($UHC.$Base.$subtract,[$UHC.$Base.$Num__DCT74__101__0,1]);
          var $__8=
           new _A_($UHC.$Base.$_2e,[$__7,$__6]);
          var $__9=
           new _A_($UHC.$Base.$toEnum,[$Enum__]);
          var $Enum__CLS74__38__0DFLUHC_2eBase_2epred=
           new _A_($UHC.$Base.$_2e,[$__9,$__8]);
          var $__10=
           new _A_($UHC.$Base.$Enum__CLS74__38__0DFLUHC_2eBase_2eenumFromTo,[$Enum__]);
          var $__11=
           new _A_($UHC.$Base.$Enum__CLS74__38__0DFLUHC_2eBase_2eenumFromThenTo,[$Enum__]);
          var $__12=
           new _A_($UHC.$Base.$Enum__CLS74__38__0DFLUHC_2eBase_2eenumFromThen,[$Enum__]);
          var $__13=
           new _A_($UHC.$Base.$Enum__CLS74__38__0DFLUHC_2eBase_2eenumFrom,[$Enum__]);
          var $Enum__14=
           {_tag_:0,_1:$__13,_2:$__12,_3:$__11,_4:$__10,_5:$UHC.$Base.$undefined,_6:$Enum__CLS74__38__0DFLUHC_2eBase_2epred,_7:$Enum__CLS74__38__0DFLUHC_2eBase_2esucc,_8:$UHC.$Base.$undefined};
          return $Enum__14;});
$UHC.$Base.$primNegDouble=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primNegInt($__2);});
$UHC.$Base.$primAddDouble=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primAddInt($__3,$__4);});
$UHC.$Base.$Num__NEW5577UNQ10622EVLDCT74__177__0RDC=
 new _F_(function($Num__,$Num__2,$Num__3)
         {var $Num__4=
           _e_(new _A_($UHC.$Base.$Num__CLS74__11__0,[$Num__3]));
          var $__14=
           {_tag_:0,_1:$UHC.$Base.$primMulDouble,_2:$UHC.$Base.$primAddDouble,_3:$UHC.$Base.$primSubDouble,_4:$UHC.$Base.$Eq__DCT74__162__0,_5:$Num__,_6:$UHC.$Base.$primIntToDouble,_7:$UHC.$Base.$primIntegerToDouble,_8:$UHC.$Base.$primNegDouble,_9:$Num__2};
          return $__14;});
$UHC.$Base.$Num__NEW5573UNQ10617DCT74__177__0RDC=
 new _F_(function($Num__,$Num__2,$Num__3)
         {var $Num__4=
           new _A_($UHC.$Base.$Num__NEW5577UNQ10622EVLDCT74__177__0RDC,[$Num__,$Num__2,$Num__3]);
          return $Num__4;});
$UHC.$Base.$_3e_3d=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._4;});
$UHC.$Base.$negate=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._8;});
$UHC.$Base.$fromInteger=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._7;});
$UHC.$Base.$packedStringToInteger=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primPackedStringToInteger($__2);});
$UHC.$Base.$absReal=
 new _F_(function($__,$__2,$x)
         {var $__4=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__5=
           new _A_($UHC.$Base.$fromInteger,[$__2,$__4]);
          var $__6=
           new _A_($UHC.$Base.$_3e_3d,[$__,$x,$__5]);
          var $__7=
           _e_($__6);
          var $__swJSW819__0;
          switch($__7._tag_)
           {case 0:
             var $__8=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW820__0;
             switch($__8._tag_)
              {case 0:
                var $__9=
                 new _A_($UHC.$Base.$packedStringToString,["FAIL 75_118_0"]);
                var $__10=
                 new _A_($UHC.$Base.$error,[$__9]);
                $__swJSW820__0=
                 $__10;
                break;
               case 1:
                var $__11=
                 new _A_($UHC.$Base.$negate,[$__2,$x]);
                $__swJSW820__0=
                 $__11;
                break;}
             $__swJSW819__0=
              $__swJSW820__0;
             break;
            case 1:
             $__swJSW819__0=
              $x;
             break;}
          return $__swJSW819__0;});
$UHC.$Base.$Num__DCT74__177__0DFLUHC_2eBase_2eabs=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$absReal,[$UHC.$Base.$Ord__DCT74__166__0,$UHC.$Base.$Num__UNQ10617DCT74__177__0RDC]);}),[]);
$UHC.$Base.$Num__UNQ10617DCT74__177__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Num__NEW5573UNQ10617DCT74__177__0RDC,[$UHC.$Base.$Num__DCT74__177__0DFLUHC_2eBase_2eabs,$UHC.$Base.$Num__DCT74__177__0DFLUHC_2eBase_2esignum,$UHC.$Base.$Num__UNQ10617DCT74__177__0RDC]);}),[]);
$UHC.$Base.$Num__DCT74__177__0DFLUHC_2eBase_2esignum=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$signumReal,[$UHC.$Base.$Ord__DCT74__166__0,$UHC.$Base.$Num__UNQ10617DCT74__177__0RDC]);}),[]);
$UHC.$Base.$Num__DCT74__177__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Num__UNQ10617DCT74__177__0RDC;}),[]);
$UHC.$Base.$primCmpDouble=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primCmpInt($__3,$__4);});
$UHC.$Base.$primEqDouble=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return primEqInt($__3,$__4);});
$UHC.$Base.$Eq__NEW1741UNQ10092EVLDCT74__162__0RDC=
 new _F_(function($Eq__)
         {var $Eq__2=
           _e_(new _A_($UHC.$Base.$Eq__CLS74__4__0,[$Eq__]));
          var $__5=
           {_tag_:0,_1:$Eq__2._1,_2:$UHC.$Base.$primEqDouble};
          return $__5;});
$UHC.$Base.$Eq__NEW1739UNQ10091DCT74__162__0RDC=
 new _F_(function($Eq__)
         {var $Eq__2=
           new _A_($UHC.$Base.$Eq__NEW1741UNQ10092EVLDCT74__162__0RDC,[$Eq__]);
          return $Eq__2;});
$UHC.$Base.$Eq__UNQ10091DCT74__162__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq__NEW1739UNQ10091DCT74__162__0RDC,[$UHC.$Base.$Eq__UNQ10091DCT74__162__0RDC]);}),[]);
$UHC.$Base.$Eq__DCT74__162__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Eq__UNQ10091DCT74__162__0RDC;}),[]);
$UHC.$Base.$__76__19336__2__0NEW2115UNQ5002=
 new _F_(function($Ord__)
         {var $Eq__=
           _e_($Ord__);
          return $Eq__._5;});
$UHC.$Base.$Ord__CLS74__5__0DFLUHC_2eBase_2emin=
 new _F_(function($Ord__,$x,$y)
         {var $__=
           new _A_($UHC.$Base.$_3c_3d,[$Ord__,$x,$y]);
          var $__5=
           _e_($__);
          var $__swJSW823__0;
          switch($__5._tag_)
           {case 0:
             var $__6=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW824__0;
             switch($__6._tag_)
              {case 0:
                var $__7=
                 new _A_($UHC.$Base.$packedStringToString,["FAIL 75_19_0"]);
                var $__8=
                 new _A_($UHC.$Base.$error,[$__7]);
                $__swJSW824__0=
                 $__8;
                break;
               case 1:
                $__swJSW824__0=
                 $y;
                break;}
             $__swJSW823__0=
              $__swJSW824__0;
             break;
            case 1:
             $__swJSW823__0=
              $x;
             break;}
          return $__swJSW823__0;});
$UHC.$Base.$Ord__CLS74__5__0DFLUHC_2eBase_2emax=
 new _F_(function($Ord__,$x,$y)
         {var $__=
           new _A_($UHC.$Base.$_3c_3d,[$Ord__,$x,$y]);
          var $__5=
           _e_($__);
          var $__swJSW825__0;
          switch($__5._tag_)
           {case 0:
             var $__6=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW826__0;
             switch($__6._tag_)
              {case 0:
                var $__7=
                 new _A_($UHC.$Base.$packedStringToString,["FAIL 75_18_0"]);
                var $__8=
                 new _A_($UHC.$Base.$error,[$__7]);
                $__swJSW826__0=
                 $__8;
                break;
               case 1:
                $__swJSW826__0=
                 $x;
                break;}
             $__swJSW825__0=
              $__swJSW826__0;
             break;
            case 1:
             $__swJSW825__0=
              $y;
             break;}
          return $__swJSW825__0;});
$UHC.$Base.$Ord__CLS74__5__0DFLUHC_2eBase_2e_3c=
 new _F_(function($Ord__,$x,$y)
         {var $__=
           new _A_($UHC.$Base.$compare,[$Ord__,$x,$y]);
          return new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$__74__80__0,$__,$UHC.$Base.$LT__]);});
$UHC.$Base.$Ord__CLS74__5__0DFLUHC_2eBase_2e_3e=
 new _F_(function($Ord__,$x,$y)
         {var $__=
           new _A_($UHC.$Base.$compare,[$Ord__,$x,$y]);
          return new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$__74__80__0,$__,$UHC.$Base.$GT__]);});
$UHC.$Base.$_3c_3d=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._2;});
$UHC.$Base.$otherwise=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$True__;}),[]);
$UHC.$Base.$Ord__CLS74__5__0DFLUHC_2eBase_2ecompare=
 new _F_(function($__,$Ord__,$x,$y)
         {var $__5=
           new _A_($UHC.$Base.$_3d_3d,[$__,$x,$y]);
          var $__6=
           _e_($__5);
          var $__swJSW828__0;
          switch($__6._tag_)
           {case 0:
             var $__7=
              new _A_($UHC.$Base.$_3c_3d,[$Ord__,$x,$y]);
             var $__8=
              _e_($__7);
             var $__swJSW829__0;
             switch($__8._tag_)
              {case 0:
                var $__9=
                 _e_($UHC.$Base.$otherwise);
                var $__swJSW830__0;
                switch($__9._tag_)
                 {case 0:
                   var $__10=
                    new _A_($UHC.$Base.$packedStringToString,["FAIL 75_13_0"]);
                   var $__11=
                    new _A_($UHC.$Base.$error,[$__10]);
                   $__swJSW830__0=
                    $__11;
                   break;
                  case 1:
                   $__swJSW830__0=
                    $UHC.$Base.$GT__;
                   break;}
                $__swJSW829__0=
                 $__swJSW830__0;
                break;
               case 1:
                $__swJSW829__0=
                 $UHC.$Base.$LT__;
                break;}
             $__swJSW828__0=
              $__swJSW829__0;
             break;
            case 1:
             $__swJSW828__0=
              $UHC.$Base.$EQ__;
             break;}
          return $__swJSW828__0;});
$UHC.$Base.$Ord__CLS74__5__0DFLUHC_2eBase_2e_3c_3d=
 new _F_(function($Ord__,$x,$y)
         {var $__=
           new _A_($UHC.$Base.$compare,[$Ord__,$x,$y]);
          return new _A_($UHC.$Base.$_2f_3d,[$UHC.$Base.$__74__80__0,$__,$UHC.$Base.$GT__]);});
$UHC.$Base.$compare=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._6;});
$UHC.$Base.$_2f_3d=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$UHC.$Base.$Eq__CLS74__4__0DFLUHC_2eBase_2e_3d_3d=
 new _F_(function($Eq__,$x,$y)
         {var $__=
           new _A_($UHC.$Base.$_2f_3d,[$Eq__,$x,$y]);
          return new _A_($UHC.$Base.$not,[$__]);});
$UHC.$Base.$_3d_3d=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._2;});
$UHC.$Base.$not=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW834__0;
          switch($__._tag_)
           {case 0:
             $__swJSW834__0=
              $UHC.$Base.$True__;
             break;
            case 1:
             $__swJSW834__0=
              $UHC.$Base.$False__;
             break;}
          return $__swJSW834__0;});
$UHC.$Base.$Eq__CLS74__4__0DFLUHC_2eBase_2e_2f_3d=
 new _F_(function($Eq__,$x,$y)
         {var $__=
           new _A_($UHC.$Base.$_3d_3d,[$Eq__,$x,$y]);
          return new _A_($UHC.$Base.$not,[$__]);});
$UHC.$Base.$Eq__CLS74__4__0=
 new _F_(function($Eq__)
         {var $__=
           new _A_($UHC.$Base.$Eq__CLS74__4__0DFLUHC_2eBase_2e_3d_3d,[$Eq__]);
          var $__3=
           new _A_($UHC.$Base.$Eq__CLS74__4__0DFLUHC_2eBase_2e_2f_3d,[$Eq__]);
          var $Eq__4=
           {_tag_:0,_1:$__3,_2:$__};
          return $Eq__4;});
$UHC.$Base.$__74__80__0NEW1861UNQ9905EVLRDC=
 new _F_(function($__,$__2)
         {var $Eq__=
           _e_(new _A_($UHC.$Base.$Eq__CLS74__4__0,[$__]));
          var $__6=
           {_tag_:0,_1:$Eq__._1,_2:$__2};
          return $__6;});
$UHC.$Base.$__74__80__0NEW1858UNQ9894RDC=
 new _F_(function($__,$__2)
         {var $__3=
           new _A_($UHC.$Base.$__74__80__0NEW1861UNQ9905EVLRDC,[$__,$__2]);
          return $__3;});
$UHC.$Base.$GT__=
 new _A_(new _F_(function()
                 {return {_tag_:1};}),[]);
$UHC.$Base.$EQ__=
 new _A_(new _F_(function()
                 {return {_tag_:0};}),[]);
$UHC.$Base.$LT__=
 new _A_(new _F_(function()
                 {return {_tag_:2};}),[]);
$UHC.$Base.$__Rep0OrderingDFLUHC_2eBase_2eto0GENRepresentable0=
 new _F_(function($proj__1)
         {var $proj__2=
           _e_($proj__1);
          var $__swJSW836__0;
          switch($proj__2._tag_)
           {case 0:
             var $proj__4=
              _e_($proj__2.unL1);
             $__swJSW836__0=
              $UHC.$Base.$LT__;
             break;
            case 1:
             var $proj__56=
              _e_($proj__2.unR1);
             var $__swJSW838__0;
             switch($proj__56._tag_)
              {case 0:
                var $proj__7=
                 _e_($proj__56.unL1);
                $__swJSW838__0=
                 $UHC.$Base.$EQ__;
                break;
               case 1:
                var $proj__9=
                 _e_($proj__56.unR1);
                $__swJSW838__0=
                 $UHC.$Base.$GT__;
                break;}
             $__swJSW836__0=
              $__swJSW838__0;
             break;}
          return $__swJSW836__0;});
$UHC.$Base.$R1__=
 new _F_(function($x1)
         {return {_tag_:1,unR1:$x1};});
$UHC.$Base.$M1__=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$id;}),[]);
$UHC.$Base.$L1__=
 new _F_(function($x1)
         {return {_tag_:0,unL1:$x1};});
$UHC.$Base.$U1__=
 new _A_(new _F_(function()
                 {return {_tag_:0};}),[]);
$UHC.$Base.$__Rep0OrderingDFLUHC_2eBase_2efrom0GENRepresentable0=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          var $__swJSW841__0;
          switch($x2._tag_)
           {case 0:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__4=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__5=
              new _A_($UHC.$Base.$R1__,[$__4]);
             var $__6=
              new _A_($UHC.$Base.$M1__,[$__5]);
             $__swJSW841__0=
              $__6;
             break;
            case 1:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__8=
              new _A_($UHC.$Base.$R1__,[$__]);
             var $__9=
              new _A_($UHC.$Base.$R1__,[$__8]);
             var $__10=
              new _A_($UHC.$Base.$M1__,[$__9]);
             $__swJSW841__0=
              $__10;
             break;
            case 2:
             var $__=
              new _A_($UHC.$Base.$M1__,[$UHC.$Base.$U1__]);
             var $__12=
              new _A_($UHC.$Base.$L1__,[$__]);
             var $__13=
              new _A_($UHC.$Base.$M1__,[$__12]);
             $__swJSW841__0=
              $__13;
             break;}
          return $__swJSW841__0;});
$UHC.$Base.$Representable0__CLS74__369__0=
 new _F_(function($Representable0__)
         {var $Representable0__2=
           {_tag_:0,_1:$UHC.$Base.$undefined,_2:$UHC.$Base.$undefined};
          return $Representable0__2;});
$UHC.$Base.$__Rep0OrderingNEW1319UNQ2261EVLSDCGENRepresentable0=
 new _F_(function($__)
         {var $Representable0__=
           _e_(new _A_($UHC.$Base.$Representable0__CLS74__369__0,[$__]));
          var $__5=
           {_tag_:0,_1:$UHC.$Base.$__Rep0OrderingDFLUHC_2eBase_2efrom0GENRepresentable0,_2:$UHC.$Base.$__Rep0OrderingDFLUHC_2eBase_2eto0GENRepresentable0};
          return $__5;});
$UHC.$Base.$__Rep0OrderingNEW1317UNQ2260SDCGENRepresentable0=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$__Rep0OrderingNEW1319UNQ2261EVLSDCGENRepresentable0,[$__]);
          return $__2;});
$UHC.$Base.$__Rep0OrderingUNQ2260SDCGENRepresentable0=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$__Rep0OrderingNEW1317UNQ2260SDCGENRepresentable0,[$UHC.$Base.$__Rep0OrderingUNQ2260SDCGENRepresentable0]);}),[]);
$UHC.$Base.$__Rep0OrderingGENRepresentable0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$__Rep0OrderingUNQ2260SDCGENRepresentable0;}),[]);
$UHC.$Base.$from0=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$UHC.$Base.$geqdefault=
 new _F_(function($__,$__2,$rep,$x,$y)
         {var $__6=
           new _A_($UHC.$Base.$from0,[$__,$y]);
          var $__7=
           new _A_($UHC.$Base.$from0,[$__,$x]);
          return new _A_($UHC.$Base.$geq_27,[$__2,$__7,$__6]);});
$UHC.$Base.$__76__42802__2__5UNQ9895=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$UHC.$Base.$__76__42802__2__3UNQ9902,$UHC.$Base.$__76__42802__2__3UNQ9902]);}),[]);
$UHC.$Base.$True__=
 new _A_(new _F_(function()
                 {return {_tag_:1};}),[]);
$UHC.$Base.$Eq_27__DCT74__389__0DFLUHC_2eBase_2egeq_27=
 new _F_(function($__,$__2)
         {return $UHC.$Base.$True__;});
$UHC.$Base.$Eq_27__NEW555UNQ10149EVLDCT74__389__0RDC=
 new _F_(function($Eq_27__)
         {var $Eq_27__2=
           _e_(new _A_($UHC.$Base.$Eq_27__CLS74__388__0,[$Eq_27__]));
          var $__4=
           {_tag_:0,_1:$UHC.$Base.$Eq_27__DCT74__389__0DFLUHC_2eBase_2egeq_27};
          return $__4;});
$UHC.$Base.$Eq_27__NEW553UNQ10148DCT74__389__0RDC=
 new _F_(function($Eq_27__)
         {var $Eq_27__2=
           new _A_($UHC.$Base.$Eq_27__NEW555UNQ10149EVLDCT74__389__0RDC,[$Eq_27__]);
          return $Eq_27__2;});
$UHC.$Base.$Eq_27__UNQ10148DCT74__389__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__NEW553UNQ10148DCT74__389__0RDC,[$UHC.$Base.$Eq_27__UNQ10148DCT74__389__0RDC]);}),[]);
$UHC.$Base.$Eq_27__DCT74__389__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Eq_27__UNQ10148DCT74__389__0RDC;}),[]);
$UHC.$Base.$__76__42802__2__3UNQ9902=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$UHC.$Base.$Eq_27__DCT74__389__0]);}),[]);
$UHC.$Base.$False__=
 new _A_(new _F_(function()
                 {return {_tag_:0};}),[]);
$UHC.$Base.$Eq_27__DCT74__392__0DFLUHC_2eBase_2egeq_27=
 new _F_(function($__,$__2,$x1,$x2)
         {var $x15=
           _e_($x1);
          var $__swJSW845__0;
          switch($x15._tag_)
           {case 0:
             var $x27=
              _e_($x2);
             var $__swJSW846__0;
             switch($x27._tag_)
              {case 0:
                var $__9=
                 new _A_($UHC.$Base.$geq_27,[$__,$x15.unL1,$x27.unL1]);
                $__swJSW846__0=
                 $__9;
                break;
               case 1:
                $__swJSW846__0=
                 $UHC.$Base.$False__;
                break;}
             $__swJSW845__0=
              $__swJSW846__0;
             break;
            case 1:
             var $x212=
              _e_($x2);
             var $__swJSW847__0;
             switch($x212._tag_)
              {case 0:
                $__swJSW847__0=
                 $UHC.$Base.$False__;
                break;
               case 1:
                var $__15=
                 new _A_($UHC.$Base.$geq_27,[$__2,$x15.unR1,$x212.unR1]);
                $__swJSW847__0=
                 $__15;
                break;}
             $__swJSW845__0=
              $__swJSW847__0;
             break;}
          return $__swJSW845__0;});
$UHC.$Base.$Eq_27__NEW1846UNQ10159EVLDCT74__392__0RDC=
 new _F_(function($__,$Eq_27__,$__3)
         {var $Eq_27__4=
           _e_(new _A_($UHC.$Base.$Eq_27__CLS74__388__0,[$Eq_27__]));
          var $__6=
           new _A_($UHC.$Base.$Eq_27__DCT74__392__0DFLUHC_2eBase_2egeq_27,[$__,$__3]);
          var $__7=
           {_tag_:0,_1:$__6};
          return $__7;});
$UHC.$Base.$Eq_27__NEW1842UNQ10156DCT74__392__0RDC=
 new _F_(function($__,$Eq_27__,$__3)
         {var $Eq_27__4=
           new _A_($UHC.$Base.$Eq_27__NEW1846UNQ10159EVLDCT74__392__0RDC,[$__,$Eq_27__,$__3]);
          return $Eq_27__4;});
$UHC.$Base.$Eq_27__DCT74__392__0=
 new _F_(function($__,$__2)
         {var $Eq_27__=
           _i_();
          _i_set_($Eq_27__,new _A_($UHC.$Base.$Eq_27__NEW1842UNQ10156DCT74__392__0RDC,[$__,$Eq_27__,$__2]));
          return $Eq_27__;});
$UHC.$Base.$__76__42802__2__2UNQ9901=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__392__0,[$UHC.$Base.$__76__42802__2__3UNQ9902,$UHC.$Base.$__76__42802__2__5UNQ9895]);}),[]);
$UHC.$Base.$geq_27=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$UHC.$Base.$Eq_27__DCT74__391__0DFLUHC_2eBase_2egeq_27=
 new _F_(function($__,$__2,$__3)
         {return new _A_($UHC.$Base.$geq_27,[$__,$__2,$__3]);});
$UHC.$Base.$Eq_27__CLS74__388__0=
 new _F_(function($Eq_27__)
         {var $Eq_27__2=
           {_tag_:0,_1:$UHC.$Base.$undefined};
          return $Eq_27__2;});
$UHC.$Base.$Eq_27__NEW1830UNQ10137EVLDCT74__391__0RDC=
 new _F_(function($Eq_27__,$__)
         {var $Eq_27__3=
           _e_(new _A_($UHC.$Base.$Eq_27__CLS74__388__0,[$Eq_27__]));
          var $__5=
           new _A_($UHC.$Base.$Eq_27__DCT74__391__0DFLUHC_2eBase_2egeq_27,[$__]);
          var $__6=
           {_tag_:0,_1:$__5};
          return $__6;});
$UHC.$Base.$Eq_27__NEW1827UNQ10135DCT74__391__0RDC=
 new _F_(function($Eq_27__,$__)
         {var $Eq_27__3=
           new _A_($UHC.$Base.$Eq_27__NEW1830UNQ10137EVLDCT74__391__0RDC,[$Eq_27__,$__]);
          return $Eq_27__3;});
$UHC.$Base.$Eq_27__DCT74__391__0=
 new _F_(function($__)
         {var $Eq_27__=
           _i_();
          _i_set_($Eq_27__,new _A_($UHC.$Base.$Eq_27__NEW1827UNQ10135DCT74__391__0RDC,[$Eq_27__,$__]));
          return $Eq_27__;});
$UHC.$Base.$__76__42810__0__4__0UNQ9897=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Eq_27__DCT74__391__0,[$UHC.$Base.$__76__42802__2__2UNQ9901]);}),[]);
$UHC.$Base.$__74__80__0DFLUHC_2eBase_2e_3d_3d=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$geqdefault,[$UHC.$Base.$__Rep0OrderingGENRepresentable0,$UHC.$Base.$__76__42810__0__4__0UNQ9897,$UHC.$Base.$undefined]);}),[]);
$UHC.$Base.$__74__80__0UNQ9894RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$__74__80__0NEW1858UNQ9894RDC,[$UHC.$Base.$__74__80__0UNQ9894RDC,$UHC.$Base.$__74__80__0DFLUHC_2eBase_2e_3d_3d]);}),[]);
$UHC.$Base.$__74__80__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$__74__80__0UNQ9894RDC;}),[]);
$UHC.$Base.$Ord__CLS74__5__0DFLUHC_2eBase_2e_3e_3d=
 new _F_(function($Ord__,$x,$y)
         {var $__=
           new _A_($UHC.$Base.$compare,[$Ord__,$x,$y]);
          return new _A_($UHC.$Base.$_2f_3d,[$UHC.$Base.$__74__80__0,$__,$UHC.$Base.$LT__]);});
$UHC.$Base.$Ord__CLS74__5__0=
 new _F_(function($Ord__)
         {var $__=
           new _A_($UHC.$Base.$__76__19336__2__0NEW2115UNQ5002,[$Ord__]);
          var $__3=
           new _A_($UHC.$Base.$Ord__CLS74__5__0DFLUHC_2eBase_2emin,[$Ord__]);
          var $__4=
           new _A_($UHC.$Base.$Ord__CLS74__5__0DFLUHC_2eBase_2emax,[$Ord__]);
          var $__5=
           new _A_($UHC.$Base.$Ord__CLS74__5__0DFLUHC_2eBase_2ecompare,[$__,$Ord__]);
          var $__6=
           new _A_($UHC.$Base.$Ord__CLS74__5__0DFLUHC_2eBase_2e_3e_3d,[$Ord__]);
          var $__7=
           new _A_($UHC.$Base.$Ord__CLS74__5__0DFLUHC_2eBase_2e_3e,[$Ord__]);
          var $__8=
           new _A_($UHC.$Base.$Ord__CLS74__5__0DFLUHC_2eBase_2e_3c_3d,[$Ord__]);
          var $__9=
           new _A_($UHC.$Base.$Ord__CLS74__5__0DFLUHC_2eBase_2e_3c,[$Ord__]);
          var $Ord__10=
           {_tag_:0,_1:$__9,_2:$__8,_3:$__7,_4:$__6,_5:$UHC.$Base.$undefined,_6:$__5,_7:$__4,_8:$__3};
          return $Ord__10;});
$UHC.$Base.$Ord__NEW2152UNQ10782EVLDCT74__166__0RDC=
 new _F_(function($Ord__)
         {var $Ord__2=
           _e_(new _A_($UHC.$Base.$Ord__CLS74__5__0,[$Ord__]));
          var $__11=
           {_tag_:0,_1:$Ord__2._1,_2:$Ord__2._2,_3:$Ord__2._3,_4:$Ord__2._4,_5:$UHC.$Base.$Eq__DCT74__162__0,_6:$UHC.$Base.$primCmpDouble,_7:$Ord__2._7,_8:$Ord__2._8};
          return $__11;});
$UHC.$Base.$Ord__NEW2150UNQ10781DCT74__166__0RDC=
 new _F_(function($Ord__)
         {var $Ord__2=
           new _A_($UHC.$Base.$Ord__NEW2152UNQ10782EVLDCT74__166__0RDC,[$Ord__]);
          return $Ord__2;});
$UHC.$Base.$Ord__UNQ10781DCT74__166__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Ord__NEW2150UNQ10781DCT74__166__0RDC,[$UHC.$Base.$Ord__UNQ10781DCT74__166__0RDC]);}),[]);
$UHC.$Base.$Ord__DCT74__166__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Ord__UNQ10781DCT74__166__0RDC;}),[]);
$Asteroids.$fresh=
 new _F_(function($r)
         {var $__=
           new _A_($UHC.$Base.$_3e,[$UHC.$Base.$Ord__DCT74__166__0,$r,$Asteroids.$chance]);
          var $__3=
           _e_($__);
          var $__swJSW852__0;
          switch($__3._tag_)
           {case 0:
             var $__4=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW853__0;
             switch($__4._tag_)
              {case 0:
                var $__5=
                 new _A_($UHC.$Base.$packedStringToString,["FAIL 52_14_0"]);
                var $__6=
                 new _A_($UHC.$Base.$error,[$__5]);
                $__swJSW853__0=
                 $__6;
                break;
               case 1:
                var $__7=
                 new _A_($UHC.$Base.$fromIntegral,[$UHC.$Base.$Integral__DCT74__110__0,$UHC.$Base.$Num__DCT74__177__0,$Asteroids.$width]);
                var $__8=
                 new _A_($UHC.$Base.$_2a,[$UHC.$Base.$Num__DCT74__177__0,$__7,$r]);
                var $__9=
                 new _A_($UHC.$Base.$_2f,[$UHC.$Base.$Fractional__DCT74__197__0,$__8,$Asteroids.$chance]);
                var $__10=
                 new _A_($UHC.$Base.$floor,[$UHC.$Base.$RealFrac__DCT74__227__0,$UHC.$Base.$Integral__DCT74__110__0,$__9]);
                var $__11=
                 new _A_($Asteroids.$track,[$__10]);
                var $__12=
                 new _A_($UHC.$Base.$_3a,[$__11,$UHC.$Base.$_5b_5d]);
                $__swJSW853__0=
                 $__12;
                break;}
             $__swJSW852__0=
              $__swJSW853__0;
             break;
            case 1:
             $__swJSW852__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW852__0;});
$Asteroids.$__55__169=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$map,[$Asteroids.$fresh,$Asteroids.$randoms]);}),[]);
$Asteroids.$randomRocks=
 new _A_(new _F_(function()
                 {return new _A_($Asteroids.$flatten,[$UHC.$Base.$_5b_5d,$Asteroids.$__55__169]);}),[]);
$UHC.$IOBase.$__234__1635__0=
 new _F_(function($var)
         {return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$var]);});
$UHC.$Base.$id=
 new _F_(function($x)
         {return $x;});
$UHC.$ST.$ST__=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$id;}),[]);
$UHC.$Base.$_24=
 new _F_(function($f)
         {return $f;});
$UHC.$STRef.$STRef__=
 new _F_(function($x1)
         {return {_tag_:0,_1:$x1};});
$UHC.$MutVar.$newMutVar=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__2);
          return primNewMutVar($__,$__3);});
$UHC.$STRef.$__132__42__0=
 new _F_(function($init,$s1)
         {var $__=
           new _A_($UHC.$MutVar.$newMutVar,[$init,$s1]);
          var $__4=
           _e_($__);
          var $__7=
           new _A_($UHC.$STRef.$STRef__,[$__4[1]]);
          var $__8=
           [$__4[0],$__7];
          return $__8;});
$UHC.$STRef.$newSTRef=
 new _F_(function($init)
         {var $__=
           new _A_($UHC.$STRef.$__132__42__0,[$init]);
          return new _A_($UHC.$Base.$_24,[$UHC.$ST.$ST__,$__]);});
$UHC.$IOBase.$stToIO=
 new _F_(function($__)
         {return $__;});
$UHC.$IOBase.$newIORef=
 new _F_(function($v)
         {var $__=
           new _A_($UHC.$STRef.$newSTRef,[$v]);
          var $__3=
           new _A_($UHC.$IOBase.$stToIO,[$__]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__3,$UHC.$IOBase.$__234__1635__0]);});
$Graphics.$UI.$WXCore.$Types.$varCreate=
 new _A_(new _F_(function()
                 {return $UHC.$IOBase.$newIORef;}),[]);
$Asteroids.$asteroids=
 new _A_(new _F_(function()
                 {var $__=
                   new _A_($Graphics.$UI.$WXCore.$Types.$varCreate,[$Asteroids.$randomRocks]);
                  return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$Asteroids.$_24okUNQ149]);}),[]);
$Language.$UHC.$JS.$HTML5.$Window.$window=
 new _F_(function($__)
         {var $__2=
           _e_(window);
          return [$__,$__2];});
$Language.$UHC.$JS.$Marshal.$stringToJSString=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primStringToPackedString($__2);});
$Language.$UHC.$JS.$Marshal.$ToJS__CLS81__0__0=
 new _F_(function($ToJS__)
         {var $ToJS__2=
           {_tag_:0,_1:$UHC.$Base.$undefined};
          return $ToJS__2;});
$Language.$UHC.$JS.$Marshal.$ToJS__NEW17UNQ206EVLDCT81__13__0RDC=
 new _F_(function($ToJS__)
         {var $ToJS__2=
           _e_(new _A_($Language.$UHC.$JS.$Marshal.$ToJS__CLS81__0__0,[$ToJS__]));
          var $__4=
           {_tag_:0,_1:$Language.$UHC.$JS.$Marshal.$stringToJSString};
          return $__4;});
$Language.$UHC.$JS.$Marshal.$ToJS__NEW15UNQ205DCT81__13__0RDC=
 new _F_(function($ToJS__)
         {var $ToJS__2=
           new _A_($Language.$UHC.$JS.$Marshal.$ToJS__NEW17UNQ206EVLDCT81__13__0RDC,[$ToJS__]);
          return $ToJS__2;});
$Language.$UHC.$JS.$Marshal.$ToJS__UNQ205DCT81__13__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Language.$UHC.$JS.$Marshal.$ToJS__NEW15UNQ205DCT81__13__0RDC,[$Language.$UHC.$JS.$Marshal.$ToJS__UNQ205DCT81__13__0RDC]);}),[]);
$Language.$UHC.$JS.$Marshal.$ToJS__DCT81__13__0=
 new _A_(new _F_(function()
                 {return $Language.$UHC.$JS.$Marshal.$ToJS__UNQ205DCT81__13__0RDC;}),[]);
$Language.$UHC.$JS.$Marshal.$toJS=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$Language.$UHC.$JS.$Primitives.$__primSetAttr=
 new _F_(function($__,$__2,$__3,$__4)
         {var $__5=
           _e_($__);
          var $__6=
           _e_($__2);
          var $__7=
           _e_($__3);
          var $__8=
           _e_(primSetAttr($__5,$__6,$__7));
          return [$__4,$__8];});
$Language.$UHC.$JS.$Prelude.$setAttr=
 new _F_(function($s)
         {var $__=
           new _A_($Language.$UHC.$JS.$Marshal.$toJS,[$Language.$UHC.$JS.$Marshal.$ToJS__DCT81__13__0,$s]);
          return new _A_($Language.$UHC.$JS.$Primitives.$__primSetAttr,[$__]);});
$Language.$UHC.$JS.$Prelude.$setAttr__=
 new _F_(function($s,$a,$p)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
          var $__5=
           new _A_($Language.$UHC.$JS.$Prelude.$setAttr,[$s,$a,$p]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__5,$__]);});
$Graphics.$UI.$WXCore.$_24okUNQ16=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToString,["onload"]);
          var $__5=
           new _A_($Language.$UHC.$JS.$Prelude.$setAttr__,[$__4,$_24x2,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__5,$__]);});
$Language.$UHC.$JS.$Prelude.$wrapFunc=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_(function()
               {var res=
                 _e_(new _A_($__3,[[]]));
                _e_(res[0]);
                return _e_(res[1]);});
          return [$__2,$__4];});
$Graphics.$UI.$WXCore.$_24okUNQ9=
 new _F_(function($f,$_24x)
         {var $__=
           new _A_($Language.$UHC.$JS.$Prelude.$wrapFunc,[$f]);
          var $__4=
           new _A_($Graphics.$UI.$WXCore.$_24okUNQ16,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__,$__4]);});
$Graphics.$UI.$WXCore.$appOnInit=
 new _F_(function($f)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$_24okUNQ9,[$f]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Language.$UHC.$JS.$HTML5.$Window.$window,$__]);});
$Graphics.$UI.$WXCore.$run=
 new _F_(function($init)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
          var $__3=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$init,$__]);
          return new _A_($Graphics.$UI.$WXCore.$appOnInit,[$__3]);});
$UHC.$Base.$_3e_3e=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$UHC.$Base.$return=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._4;});
$UHC.$Base.$primbindIO=
 new _F_(function($__,$f,$w)
         {var $__4=
           new _A_($__,[$w]);
          var $__5=
           _e_($__4);
          var $w_278=
           _e_($__5[0]);
          var $__9=
           new _A_($f,[$__5[1]]);
          return new _A_($__9,[$w_278]);});
$UHC.$Base.$_3e_3e_3d=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._2;});
$UHC.$Base.$__78__7648__0=
 new _F_(function($q,$__)
         {return $q;});
$UHC.$Base.$Monad__CLS74__45__0DFLUHC_2eBase_2e_3e_3e=
 new _F_(function($Monad__,$p,$q)
         {var $__=
           new _A_($UHC.$Base.$__78__7648__0,[$q]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$Monad__,$p,$__]);});
$UHC.$Base.$Monad__CLS74__45__0=
 new _F_(function($Monad__)
         {var $__=
           new _A_($UHC.$Base.$Monad__CLS74__45__0DFLUHC_2eBase_2e_3e_3e,[$Monad__]);
          var $Monad__3=
           {_tag_:0,_1:$__,_2:$UHC.$Base.$undefined,_3:$UHC.$Base.$error,_4:$UHC.$Base.$undefined};
          return $Monad__3;});
$UHC.$Base.$primretIO=
 new _F_(function($x,$w)
         {return [$w,$x];});
$UHC.$Base.$_5b_5d=
 new _A_(new _F_(function()
                 {return {_tag_:1};}),[]);
$UHC.$Base.$_3a=
 new _F_(function($x1,$x2)
         {return {_tag_:0,_1:$x1,_2:$x2};});
$UHC.$Base.$packedStringNull=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primPackedStringNull($__2);});
$UHC.$Base.$packedStringTail=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primPackedStringTail($__2);});
$UHC.$Base.$packedStringHead=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return primPackedStringHead($__2);});
$UHC.$Base.$primThrowException=
 new _F_(function($__)
         {return primThrowException($__);});
$UHC.$Base.$throw=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$primThrowException;}),[]);
$UHC.$Base.$ErrorCall__=
 new _F_(function($x1)
         {return {_tag_:6,_1:$x1};});
$UHC.$Base.$error=
 new _F_(function($s)
         {var $__=
           new _A_($UHC.$Base.$ErrorCall__,[$s]);
          return new _A_($UHC.$Base.$throw,[$__]);});
$UHC.$Base.$__78__1373=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["Prelude.undefined"]);}),[]);
$UHC.$Base.$undefined=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$error,[$UHC.$Base.$__78__1373]);}),[]);
$UHC.$Base.$packedStringToString=
 new _F_(function($p)
         {var $__=
           new _A_($UHC.$Base.$packedStringNull,[$p]);
          var $__3=
           _e_($__);
          var $__swJSW861__0;
          switch($__3._tag_)
           {case 0:
             var $__4=
              new _A_($UHC.$Base.$packedStringTail,[$p]);
             var $__5=
              new _A_($UHC.$Base.$packedStringToString,[$__4]);
             var $__6=
              new _A_($UHC.$Base.$packedStringHead,[$p]);
             var $__7=
              new _A_($UHC.$Base.$_3a,[$__6,$__5]);
             $__swJSW861__0=
              $__7;
             break;
            case 1:
             $__swJSW861__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW861__0;});
$UHC.$Base.$Monad__NEW3761UNQ10224EVLDCT74__339__0RDC=
 new _F_(function($Monad__)
         {var $Monad__2=
           _e_(new _A_($UHC.$Base.$Monad__CLS74__45__0,[$Monad__]));
          var $__7=
           {_tag_:0,_1:$Monad__2._1,_2:$UHC.$Base.$primbindIO,_3:$Monad__2._3,_4:$UHC.$Base.$primretIO};
          return $__7;});
$UHC.$Base.$Monad__NEW3759UNQ10223DCT74__339__0RDC=
 new _F_(function($Monad__)
         {var $Monad__2=
           new _A_($UHC.$Base.$Monad__NEW3761UNQ10224EVLDCT74__339__0RDC,[$Monad__]);
          return $Monad__2;});
$UHC.$Base.$Monad__UNQ10223DCT74__339__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$Monad__NEW3759UNQ10223DCT74__339__0RDC,[$UHC.$Base.$Monad__UNQ10223DCT74__339__0RDC]);}),[]);
$UHC.$Base.$Monad__DCT74__339__0=
 new _A_(new _F_(function()
                 {return $UHC.$Base.$Monad__UNQ10223DCT74__339__0RDC;}),[]);
$Graphics.$UI.$WXCore.$Types.$unitIO=
 new _F_(function($io)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$io,$__]);});
$Graphics.$UI.$WX.$start=
 new _F_(function($io)
         {var $__=
           new _A_($Graphics.$UI.$WXCore.$Types.$unitIO,[$io]);
          return new _A_($Graphics.$UI.$WXCore.$run,[$__]);});
$Asteroids.$main=
 new _A_(new _F_(function()
                 {return new _A_($Graphics.$UI.$WX.$start,[$Asteroids.$asteroids]);}),[]);
var $main=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Run.$ehcRunMain,[$Asteroids.$main]);}),[]);
_e_(new _A_($main,[[]]));
