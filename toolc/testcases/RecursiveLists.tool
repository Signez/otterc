

object RecursiveLists {
  def main(): Unit = {
    // no vars here!
    println(new RecursiveListsMain().main2().toString());
  }
}

class RecursiveListsMain extends Objekt {
  def main2(): Void = {
    var l : IntList;
    void = this.init();
    
    l = this.Cons(11, this.Cons(12, this.Cons(13, this.Cons(14, Nil))));    
    println(l.toString());
    
    l = l.take(2);
    println(l.toString());
    
    // throw an home-made exception
    println("--- Attention: Now we will throw home-made exceptions! ---");
    l = l.take(3);
    
    return void;
  }
}

// replacement for Unit
class Void {
  def toString(): String = {return "";}
}

class Objekt {
  var void: Void;
  var Nil: EmptyIntList;
  
  def init() : Void = {
    void = new Void();
    Nil = new EmptyIntList(); 
    // note that we do not call init on Nil, because it Nil does not need another Nil and a void
    return void;
  }
  
  def throw(e : Exception): Void = {
    println("");
    println("\\   /!\ Exception /!\   //");
    println(e.toString());
    return void;
  }
  
  def error(msg: String): Void = {
    var e: Exception;
    e = new Exception();
    return this.throw(e.setMessage(msg));
  }
  
  def toString(): String = {
    return "Objekt";
  }
  
  // case-class like constructors
    
  def Cons(h: Int, t: IntList): IntList = {
    var ret: NonEmptyIntList;
    ret = new NonEmptyIntList();
    void = ret.init();
    return ret.setHead(h).setTail(t);    
  }
}


class Exception {
  var msg : String;
  def setMessage(message: String) : Exception = {
    msg = message;
    return this;
  } 
  def toString() : String = {
    return "Exception(" + msg + ")";
  }
}

class IntList extends Objekt { 
  /*abstract*/ def getHead(): Int = { return 0;}
  /*abstract*/ def getTail(): IntList = { return Nil; }
  /*abstract*/ def setHead(head: Int): IntList = { return this; }
  /*abstract*/ def setTail(tail: IntList): IntList = { return this; }
  
  def take(n: Int): IntList = {
    var ret: IntList;
    if (n == 0) {
      ret = Nil;
    } else {
      ret = this.Cons(this.getHead(), this.getTail().take(n-1));
    }
    return ret;
  }
}

class EmptyIntList extends IntList {
  // must not use its Nil, because not initialized!
  // must not read its void before it is written
  /*override*/ def getHead(): Int = {
    void = this.error("Head of empty list");
    return 0;
  }
  /*override*/ def getTail(): IntList = {
    void = this.error("Tail of empty list");
    return this;
  }
  /*override*/ def setHead(head: Int): IntList = { 
    void = this.error("setHead of empty list");
    return this; 
  }
  /*override*/ def setTail(tail: IntList): IntList = { 
    void = this.error("setTail of empty list");
    return this;  
  }
  /*override*/ def toString(): String = { return "Nil"; }
}

class NonEmptyIntList extends IntList {
  var h: Int;
  var t: IntList;
  /*override*/ def getHead(): Int = { return h; }
  /*override*/ def getTail(): IntList = { return t; }
  /*override*/ def setHead(head: Int): IntList = { 
    h = head;
    return this; 
  }
  /*override*/ def setTail(tail: IntList): IntList = { 
    t = tail;
    return this;  
  }
  /*override*/ def toString(): String = {
    return "Cons(" + h + ", " + t.toString() + ")";
  }
}
