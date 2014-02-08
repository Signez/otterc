
object LazyPrimes {
  def main(): Unit = {
    // no vars here!
    println(new LazyPrimesMain().run());
  }
}

class LazyPrimesMain {
  var i: Int;

  def from(n: Int): LazyIntList = {
    return new LazyIntList().LazyIntList(n, new FunctionReturningNats().FunctionReturningNats(n+1));
  }
  
  def testTrue(msg: String): Bool = {
    i = i + 1;
    println(msg);
    return true;
  }
  
  def takeAndPrintSome(l : LazyIntList) : String = {
    println(l.take(4).evaluateAsString());
    println(l.take(23).evaluateAsString());
    println(l.take(9).evaluateAsString());
    return "";
  }

  def run() : String = {
    var nats: LazyIntList;
    var squares: LazyIntList;
    var primes: LazyIntList;
    var notBy4: LazyIntList;
    var notBy4and6: LazyIntList;
    
    nats = this.from(0);
    println(nats.take(6).evaluateAsString());
    
    squares = nats.map(new FunctionSquare());
    println(this.takeAndPrintSome(squares));
    
    // test if lazy or evaluation is correct
    i = 0;
    println(this.testTrue("msg1") || this.testTrue("msg2"));
    if (i == 1) {
      println("or test ok");
    } else {
      println("or test failed!!!!!");
    }
    
    // test filtering
    notBy4 = nats.filter(new FunctionNotDivisibleBy().FunctionNotDivisibleBy(4));
    println(this.takeAndPrintSome(notBy4));
    notBy4and6 = notBy4.filter(new FunctionNotDivisibleBy().FunctionNotDivisibleBy(6));
    println(this.takeAndPrintSome(notBy4and6));
    
    // test primes
    primes = this.sieve(this.from(2));
    println(this.takeAndPrintSome(primes));
    
    return "";
  }
  
  // cons(s.head, sieve(s.tail filter { _ % s.head != 0 }))
  def sieve(s: LazyIntList) : LazyIntList = {
    return new LazyIntList().LazyIntList(s.getHead(), new Function42().Function42(s));
  }
}


/*abstract*/ class FunctionReturningLazyIntList {
  /*abstract*/ def apply(): LazyIntList = { return new LazyIntList(); }
}


// sieve(s.tail filter { _ % s.head != 0 })
class Function42 extends FunctionReturningLazyIntList {
  var l: LazyIntList;
  def Function42(s: LazyIntList) : Function42 = {
    l = s;
    return this;
  }
  def apply(): LazyIntList = { 
    return new LazyPrimesMain().sieve(l.getTail().filter(new FunctionNotDivisibleBy().FunctionNotDivisibleBy(l.getHead())));
  }
}

/*abstract*/ class FunctionIntToBool {
  /*abstract*/ def apply(n : Int) : Bool = { return false; }
}

// _ % given != 0
class FunctionNotDivisibleBy extends FunctionIntToBool {
  var m: Int;
  def FunctionNotDivisibleBy(given: Int): FunctionNotDivisibleBy = {
    m = given;
    return this;
  }
  def apply(n: Int) : Bool = {
    println("   testing if " + n + " mod " + m + " == 0");
    return !((n / m) * m == n);
  }
}

class FunctionReturningNats extends FunctionReturningLazyIntList {
  var n: Int;
  def FunctionReturningNats(from: Int) : FunctionReturningNats = {
    n = from;
    return this;
  }
  def apply(): LazyIntList = { 
    return new LazyIntList().LazyIntList(n, new FunctionReturningNats().FunctionReturningNats(n+1)); 
  }
}

/*abstract*/ class FunctionIntToInt {
  def apply(arg: Int) : Int = { return 0; }
}

class FunctionSquare extends FunctionIntToInt {
  def apply(arg: Int) : Int = {
    println("   calculating " + arg + "^2");
    return arg*arg;
  }
}

class LazyIntList {
  // head
  var h: Int;
  // tail
  var t: LazyIntList;
  // if tail has already been evaluated
  var te: Bool;
  // function for tail
  var tf: FunctionReturningLazyIntList;
  
  def LazyIntList(head: Int, tailFunction: FunctionReturningLazyIntList): LazyIntList = {
    h = head;
    te = false;
    tf = tailFunction;
    return this;
  }
  
  def isEmpty() : Bool = {
    return false;
  }
  
  def getHead(): Int = { 
    return h; 
  }

  def getTail(): LazyIntList = { 
    if (!te) {
      t = tf.apply();
      te = true;
    }
    return t;
  }
  
  def take(n: Int): LazyIntList = {
    return new FunctionTake().FunctionTake(n, this).apply();
  }
  
  def map(f: FunctionIntToInt) : LazyIntList = {
    return new FunctionMap().FunctionMap(this, f).apply();
  }
  
  def filter(f: FunctionIntToBool) : LazyIntList = {
    var ret : LazyIntList;
    // here we can't be very lazy, because the stream's head must be known
    // throw away elements until we get the first unfiltered
    var rest: LazyIntList;
    rest = this;
    while (!rest.isEmpty() && !f.apply(rest.getHead())) {
      rest = rest.getTail();
    }
    if (rest.isEmpty()) {
      ret = new EmptyLazyIntList();
    } else {
      ret = new LazyIntList().LazyIntList(rest.getHead(), new FunctionTakeTailThenFilter().FunctionTakeTailThenFilter(rest, f));
    }
    return ret;
  }
  
  def evaluateAsString(): String = { 
    return this.getHead() + " :: " + this.getTail().evaluateAsString(); 
  }
}

class FunctionTake extends FunctionReturningLazyIntList {
  var n: Int;
  var l: LazyIntList;
  def FunctionTake(howMany: Int, list: LazyIntList) : FunctionTake = {
    n = howMany;
    l = list;
    return this;
  }
  def apply(): LazyIntList = { 
    var ret: LazyIntList;
    if (n == 0) {
      ret = new EmptyLazyIntList();
    } else {
      if (n == 1) {
        // avoid evaluation of l.getTail()
        ret = new LazyIntList().LazyIntList(l.getHead(), new FunctionTake().FunctionTake(0, new EmptyLazyIntList()));
      } else {
        ret = new LazyIntList().LazyIntList(l.getHead(), new FunctionTake().FunctionTake(n-1, l.getTail()));
      }
    }
    return ret;
  }
}

class FunctionMap extends FunctionReturningLazyIntList {
  var l: LazyIntList;
  var f: FunctionIntToInt;
  def FunctionMap(list: LazyIntList, func: FunctionIntToInt) : FunctionMap = {
    l = list;
    f = func;
    return this;
  }
  def apply(): LazyIntList = {
    var ret: LazyIntList;
    if (l.isEmpty()) {
      ret = new EmptyLazyIntList();
    } else {
      ret = new LazyIntList().LazyIntList(f.apply(l.getHead()), new FunctionMap().FunctionMap(l.getTail(), f));
    }
    return ret;
  }
}

class FunctionTakeTailThenFilter extends FunctionReturningLazyIntList {
  var l: LazyIntList;
  var f: FunctionIntToBool;
  def FunctionTakeTailThenFilter(list: LazyIntList, func: FunctionIntToBool) : FunctionTakeTailThenFilter = {
    l = list;
    f = func;
    return this;
  }
  def apply(): LazyIntList = {
    return l.getTail().filter(f);
  }
}

class EmptyLazyIntList extends LazyIntList {
  /*override*/ def isEmpty() : Bool = {
    return true;
  }
  /*override*/ def evaluateAsString(): String = { return "Nil"; }
}






