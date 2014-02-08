object IntegerSquareRoot {
    def main() : Unit = { 
      println(new IntegerSqrt().init());
    }
} 

class IntegerSqrt {  
                     
  var a: Int;
  var b: Int;
  var c: Int;
  var c2: Int;  
  var lcg: PseudoRandomNumberGenerator;   
  var i: Int;   
  var temp: Int;
  var temp2: Int;

  
  def init(): String = {  
    lcg = new PseudoRandomNumberGenerator().init(); 
    i = 0;
    while (i < 5) {  
      temp2 = this.abs(lcg.rand()); 
      println("x = " + temp2 +" => isqrt(x) = " + this.isqrt(temp2));
      //println("     isqrt(x)^2 = "+ this.isqrt(temp2) * this.isqrt(temp2) + " <= x < (isqrt(x)+1)^2 = " + (this.isqrt(temp2)+1) * (this.isqrt(temp2) + 1));
      i = i + 1;      
    }
    return "------------------------";
  }
  
  // Computes the integer square root: isqrt(x) = floor(sqrt(x))
  def isqrt(x: Int): Int = {
    a = x;
    b = 0 ;
    c = ( a + b ) / 2;
    c2 = c*c;
    while( (b < a-1) && !(c2 == x) ) {
      if( c2 < x ) {
        b = c;
      }
      else {
        a = c;          
      }
      c = ( a + b ) / 2;
      c2 = c*c;
    }  
    return c;
  } 
  
  def abs(x: Int): Int = {
    if (x < 0) {
      temp = 0 - x;
    } else {
      temp = x;
    } 
    return temp;
  }
  

  
} 



class PseudoRandomNumberGenerator {
  var a: Int;
  var b: Int;
  var c: Int;
  var seed: Int;  
  
  // Computes modulus: x % y
  def mod(x: Int, y: Int): Int = {
    a = 0;
    b = x;
    while ( a < b ) {
      c = (a + b) / 2;
      if ( (y < (x - c*y)) ||  (y == (x - c*y))) {
        a = c + 1;          
      }
      else {
        b = c;  
      }
    }
    return x - a*y;
  } 
  
  // Generates a pseudorandom number using a Linear Congruential Generator
  def rand(): Int = {
    seed = this.mod(seed * 16807 + 12345, 2147483647);
    return seed;
  }
  
  // set the initial seed for the lcg
  def init(): PseudoRandomNumberGenerator = {
    seed = 1;
    return this;
  }
  
}

