/*
  This little program prints a calculation and then computes it.
  The point was to see how our little language could handle Objects
  inside other Objects, as well as method overriding.
*/
object Operations {
  def main() : Unit = 
      {
        println(new OperationsObj().go());
        
      }
}

class OperationsObj {
  def go():String = 
  {
    var h : HelperClass;
    var tmp : Int;
    var op1 : Operation;
    var op2 : Operation;
    var op3 : Operation;
    var op4 : Operation;
    
    
    h = new HelperClass();

    op1 = h.times(
            h.divide(
              h.val(8),
              h.d(12,3)
              ),
            h.times(
              h.d(4,2),
              h.m(6,3)
              )
            );          
    tmp = this.compute(op1);

    op2 = h.times(
            h.times(
              h.p(3,4),
              h.val(2)
            ),
            h.t(2,3)
          );
    tmp = this.compute(op2);
    
        
    op3 = h.minus(
            h.p(3,4),
            h.m(7,2)
          );
    tmp = this.compute(op3);
    
    op4 = h.minus(
            h.minus(
              h.p(3,4),
              h.val(7)
              ),
            h.val(2)
          );
    tmp = this.compute(op4);


    //now the cool ones ;)
    tmp = this.compute(
            h.times(
              op1,
              op3
            )
          );

    tmp = this.compute(
            h.times(
              op1,
              h.times(
                h.minus(
                  op2,
                  op1
                ),
                h.plus(
                  h.divide(
                    op1,
                    op4
                  ),
                  h.times(
                    op3,
                    h.val(5)
                  )
                )
              )
            )
          );
          
    
    return "Done.";
  }

  def compute(op: Operation): Int = 
  {
    var retVal : Int;
    
    println("Computing " + op.toString());
    retVal = op.getResult();

    println("The result is: "+  retVal);

    return retVal;
  }
    
}

class HelperClass {

  // Helper methods
  def val(i: Int) : Value = 
  {
    return new Value().init(i);
  }

  def plus(r : Operation, l: Operation) : BiOperation =
  {
    return new Plus().init(r,l);
  }

  def minus(r : Operation, l: Operation) : BiOperation =
  {
    return new Minus().init(r,l);
  }

  def times(r : Operation, l: Operation) : BiOperation =
  {
    return new Times().init(r,l);
  }

  def divide(r : Operation, l: Operation) : BiOperation =
  {
    return new Divide().init(r,l);
  }
  

  def p(r : Int, l: Int) : BiOperation =
  {
    return this.plus(this.val(r),this.val(l));
  }

  def m(r : Int, l: Int) : BiOperation =
  {
    return this.minus(this.val(r),this.val(l));
  }

  def t(r : Int, l: Int) : BiOperation =
  {
    return this.times(this.val(r),this.val(l));
  }

  def d(r : Int, l: Int) : BiOperation =
  {
    return this.divide(this.val(r),this.val(l));
  }    

}


  
class Operation {
  def getResult(): Int = 
  {
    return 0;
  }

  def toString() : String = 
  {
    return "";
  }

  def priority(): Int = 
  {
    return 0;
  }
    
}

class Value extends Operation {
  var value: Int;

  def init(val: Int): Value = {
    value = val;
    return this;  
  }

  def getResult() : Int = {
    return value;
  }

  def toString() : String = 
  {
    return value +""; 
  }


  def priority(): Int = 
  {
    return 100;
  }
  
}

class BiOperation extends Operation {
  var rightOp : Operation;
  var leftOp : Operation;
  
  def init(r : Operation, l : Operation) : BiOperation = 
  {
    rightOp = r;
    leftOp = l;
    return this;
  }

  def toString() : String = 
  {
    var str1 : String;
    var str2 : String;

    str1 = rightOp.toString();
    str2 = leftOp.toString();

    if( rightOp.priority() < this.priority() ||  rightOp.priority() == this.priority() )
      str1 = "(" + str1 + ")";
      
    if( leftOp.priority() < this.priority() || leftOp.priority() == this.priority() )
      str2 = "(" + str2 + ")";
      
    return str1 + this.getSymbol() + str2;
    
  }
  
  def getSymbol() : String = 
  {
    return ".";
  }
  
  
}  


class Plus extends BiOperation {
  
  def getResult() : Int = 
  {
    return rightOp.getResult() + leftOp.getResult();
  }
    
  def getSymbol() : String = 
  {
    return "+";
  }

  def priority(): Int = 
  {
    return 10;
  }

  
}

class Minus extends BiOperation {

  def getResult() : Int = 
  {
    return rightOp.getResult() - leftOp.getResult();
  }  

  def getSymbol() : String = 
  {
    return "-";
  }

  def priority(): Int = 
  {
    return 10;
  }
  
}

class Times extends BiOperation {

  def getResult() : Int = 
  {
    return rightOp.getResult() * leftOp.getResult();
  }
    
  def getSymbol() : String = 
  {
    return "*";
  }

  def priority(): Int = 
  {
    return 20;
  }
  
}

class Divide extends BiOperation {

  def getResult() : Int = 
  {
    return rightOp.getResult() / leftOp.getResult();
  }
    
  def getSymbol() : String = 
  {
    return "/";
  }

  def priority(): Int = 
  {
    return 20;
  }
  
}
    

  