/*
  This program implements List of Object_s in tool. I wanted to
  
*/
object Lists {
  def main() : Unit = 
      {
        println(new ListExample().go());
        
      }
}

class ListExample {

  def go() : String =
  {
    var list : List;
    var list2 : List;
    var merge : List;
    var split1 : List;
    var split2 : List;
    var tmp : Int;

    var intList : List;
    var intList2 : List;
    
    
    list = new List()
    .init1( this.str("List 1 Elem 1") )
    .addHead( this.str("List 1 Elem 2") )
    .addHead( this.str("List 1 Elem 3") )
    .addHead( this.str("List 1 Elem 4") )
    .addHead( this.str("List 1 Elem 5") )
    .addHead( this.str("List 1 Elem 6") )
    .addHead( this.str("List 1 Elem 7") );
    
    list2 = new List()
    .init1( this.str("List 2 Elem 1") )
    .addHead( this.str("List 2 Elem 2") )
    .addHead( this.str("List 2 Elem 3") )
    .addHead( this.str("List 2 Elem 4") )
    .addHead( this.str("List 2 Elem 5") )
    .addHead( this.str("List 2 Elem 6") );
    
    

    println("List 1 : ");
    tmp = this.goThrough(list);
    println("List 2 : ");
    tmp = this.goThrough(list2);
    println("mergeRight");
    tmp = this.goThrough(list.mergeRight(list2));
    println("mergeLeft");    
    tmp = this.goThrough(list.mergeLeft(list2));

    println("");
    println("Split examples");
    println("");
    
    merge = list.mergeRight(list2);
    println("Merged List :");
    tmp = this.goThrough(merge);

    split1 = merge.splitLeft();
    split2 = merge.splitRight();
    
    println("Splitted List 1:");
    tmp = this.goThrough(split1);
    println("Splitted List 2:");
    tmp = this.goThrough(split2);

    
    println("");
    println("Sort examples");
    println("");

    intList = new List().init1(this.i(1))
    .addTail(this.i(2))
    .addTail(this.i(3))
    .addTail(this.i(4))
    .addTail(this.i(6)); 


    

    intList = new List().init1(this.i(1))
    .addHead(this.i(3))
    .addHead(this.i(2))
    .addHead(this.i(5))
    .addHead(this.i(6)); 

    println("list : ");
    tmp = this.goThrough(intList);
    /*intList = intList.copy().insertInSorted(this.i(0));
    println("");
    tmp = this.goThrough(intList);
    intList = intList.copy().insertInSorted(this.i(2));
    println("");
    tmp = this.goThrough(intList);
    intList = intList.copy().insertInSorted(this.i(10));
    println("");
    tmp = this.goThrough(intList);
    intList = intList.copy().insertInSorted(this.i(9));
    println("");
    tmp = this.goThrough(intList);*/
    
    println("sorted list : ");
    tmp = this.goThrough(intList.sort());


    intList2 = intList.copy()
    .addHead(this.i(100))
    .addTail(this.i(0))
    .addTail(this.i(0-3))
    .addTail(this.i(0-3))
    .addHead(this.i(2));
    
    println("list 2 . ");
    tmp = this.goThrough(intList2);
    println("sorted list 2 : ");
    tmp = this.goThrough(intList2.sort());
    
    
    
    
    return "Done.";

  }

  def str(string : String) : String_ = {
    return new String_().init(string);
  }

  def i(in : Int) : Int_ = 
  {
    return new Int_().init(in);
  }
    

  def goThrough(list : List) : Int = 
  {
    var tmp : Int;

    println(list.getElem().toString());
    
    if(list.isLastElem())
      tmp = 0;
    else
      tmp = this.goThrough(list.getTail());
    return tmp;
  }
    

}

//example implementation : we redifine string to extend Object_

class String_ extends Object_ {
  var str : String;

  def init(string : String) : String_ = {
    str = string;
    return this;
  }

  def toString() : String = {
    return str;
  }
    
}

//example for sorting
class Int_ extends Object_ {
  var val : Int;

  def init(i : Int) : Int_ = {
    val = i;
    return this;
  }

  def toString() : String = {
    return "" + val;
  }

  def getComparableValue() : Int = 
  {
    return val;
  }
    
}
    


//Base type. Every object that we want to use lists with should extend Object_
class Object_ {
  def toString() : String =
  {
    return "OBJECT";
  }

  def getComparableValue() : Int = //usefull for sorting
  {
    return 0;
  }
    
}

class List {
  var elem : Object_;
  var tail : List;
  var isLastElem : Bool;

  var otherPart : List; // used by splitRight
  

  /*def construct() : List
    {
    isLastElem = true;
    }*/
  
  def init1(obj : Object_) : List = 
  {
    elem = obj;
    isLastElem = true;
    return this;
  }

    def init(obj : Object_, list: List) : List =
    {
      elem = obj;
      tail = list;
      isLastElem = false;
      return this;
    }

    def getElem() : Object_ = 
    {
      return elem;
    }

    def getTail() : List = 
    {
      return tail;
    }

    def isLastElem() : Bool = 
    {
      return isLastElem;
    }

    def setLastElem(val: Bool) : Int = 
    {
      isLastElem = val;
      return 0;
    }

    def setTail(t:List) : Int = 
    {
      tail = t;
      return 0;
    }
    

    def addHead(obj : Object_) : List = 
    {
      return new List().init(obj, this);
    }

    def addTail(obj : Object_) : List = 
    {
      return this.addIfLastElem(obj,this);
    
    }
    
    def addIfLastElem(obj : Object_, list: List) : List =
    {
      var tmp : Int;

      if(list.isLastElem()) {
        tmp = list.setLastElem(false);
        tmp = list.setTail(new List().init1(obj));
      } else 
        tail = this.addIfLastElem(obj, list.getTail());
      // not very usefull to put the result in tail,
      // but it saves us a tmp variable
    
      return list;
    }
    

    def copy() : List =
    {
      return this.copy0(this);
    }

    def copy0(current: List) : List =
    {
    var retVal : List;
    if(current.isLastElem())
      retVal = new List().init1(current.getElem());
    else {
      retVal = this.copy0(current.getTail());
      retVal = retVal.addHead(current.getElem());
    }
    
    return retVal;
  }

  def count() : Int =
  {
    return this.count0(this);
  }

  def count0(list : List) : Int =
  {
    var retVal : Int;
    if(list.isLastElem())
      retVal = 1;
    else
      retVal = this.count0(list.getTail()) + 1;

    return retVal;
  }
  
  def mergeRight(list: List) : List =
  {
    var last : List;
    var copy : List;
    
    var tmp : Int;
    
    copy = this.copy();
    last = copy;
    
    while(!(last.isLastElem())) last = last.getTail();
    
    tmp = last.setLastElem(false);
    tmp = last.setTail(list);
    return copy;
  }

    def mergeLeft(list: List) : List =
  {
    return list.mergeRight(this);
  }

  //first step of splitting
  def splitLeft() : List =
  {
    var count : Int;
    var copy : List;
    var current : List;
    
    if(isLastElem) // if only one element, we cannot split ==> return this
      current = this;
    else {

      count = this.count()/2 -1;

      copy = this.copy();
      current = copy;

      while(0 < count){
        count = count -1;
        current = current.getTail();
      }

      count = current.setLastElem(true); //count = tmp
      otherPart = current.getTail();
      current = copy;

    }
    
    return current;
  }

  //second part of split. doesent make sense if called individually. must be called after splitLeft
  def splitRight() : List =
  {
    var retVal : List;

    if(isLastElem)  // if only one element, we cannot split ==> return this
      retVal = this;
    else
      retVal = otherPart;

    return retVal;
  }

  def insertInSorted(obj : Object_) : List = 
  {
    var belowEveryOne : Bool;
    var current : List;
    var prev : List;
    var retVal : List;
    var tmp : Int;
    
    current = this;
    prev = this;
    retVal = this;
    
    belowEveryOne = true;
    
    while(current.getElem().getComparableValue() < obj.getComparableValue()
          && !current.isLastElem()){
      
          //println("Comparing " + obj.getComparableValue() + " with " + current.getElem().getComparableValue());
          
      belowEveryOne = false;    
      prev = current;
      current = current.getTail();

    }
    
    if(current.isLastElem() && !(obj.getComparableValue() < current.getElem().getComparableValue()))
      current = current.addTail(obj);
    else {
      if(belowEveryOne)
        retVal = this.addHead(obj);
      else  
        tmp = prev.setTail(current.addHead(obj));
    }
      
    return retVal;
  }

  def insertListInSorted(list : List) : List =
  {
    return this.insertListInSorted0(list, this);
  }

  def insertListInSorted0(list1 : List, list2 : List) : List =
  {
    var retVal : List;
    if(list1.isLastElem())
      retVal = list2.insertInSorted(list1.getElem());
    else
      retVal = list2.insertListInSorted0(list1.getTail(), list2.insertInSorted(list1.getElem()));

    return retVal;
  }
      
    

  def sort() : List =
  {
    return this.sort0(this);
  }

  def sort0(list : List) : List =
  {
    var retVal : List;
    var leftPart : List;
    var rightPart : List;    
    
    if(list.isLastElem())
      retVal = list.copy();
    else {
      leftPart = this.sort0(list.splitLeft());
      rightPart = this.sort0(list.splitRight());
      retVal = leftPart.insertListInSorted(rightPart);
    }
    return retVal;
  }
  
}

/*
//defined for functions that return void
class void {}
*/
  


