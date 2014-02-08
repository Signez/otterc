object Euler {
  def main(): Unit = {
    println(new Eu().approximation().getNumber());
  }
}

class Eu {
  var limit: Int;
  var i: Int;
  var approx: FalseDouble;

  def approximation(): FalseDouble = {

    var interm: Int;
    var interm2: FalseDouble;
    var inverse: FalseDouble;
    var one: FalseDouble;
    interm = 0;
    limit = 13;
    i = 0;
    approx = new FalseDouble().create(0, 0, 0);
    one = new FalseDouble().create(1, 0, 0);
    inverse = new FalseDouble();

    while(i < limit) {
      interm = this.factorial(i);
      if(interm == 0) {
        println("abort! abort!");
        i = limit;
      }
      else {
          interm2 = new FalseDouble().create(interm, 0, 0);
          inverse = one.divideIntegers(interm2);
          approx = approx.add(inverse);
          i = i + 1;
      }
    }
    
    return approx;
  }

  def factorial(integer: Int): Int = {
    var ret: Int;
    if(1 < integer) {
      ret = integer*this.factorial(integer-1);
    }
    else {
      ret = 1;
    }
    return ret;
  }
}

class FalseDouble {
  var bt: Int; //before token
  var at: Int; //after token
  var atLength: Int;
  var number: String;
  var zeros: Int; //number of 0s before at

  def create(a: Int, b: Int, c: Int) : FalseDouble = {
    var temp: Int;
    temp = 0;
    bt = a;
    at = b;
    atLength = this.computeLength(at);
    zeros = c;
    number = bt + ".";
    while (temp < zeros-1) {
      number = number + "0";
      temp = temp + 1;
    }
    number = number + at;
    return this;
  }
  def computeLength(i: Int): Int = {
    var value: Int;
    var result: Int;
    result = 0;
    value = i;
    if (i == 0) {
      result = 0;
    } else {
      result = 0;
      while(0 < value) {
        value = value/10;
        result = result + 1;
      }
    }

    return result;
  }

  def add(d2: FalseDouble) : FalseDouble = {
    var d2Bt: Int;
    var d2At: Int;
    var atRealLength: Int;
    var d2AtRealLength: Int;

    var resultAt: Int;
    var resultBt: Int;
    var resultAtLength: Int;
    var resultZeros: Int;
    var result: FalseDouble;

    var diff: Int;
    var greatestLength: Int;
    var carry: Int;
    var temp: Int;

    d2At = d2.getAt();
    d2Bt = d2.getBt();
    atRealLength = atLength + this.getZeros();
    d2AtRealLength = d2.getAtLength() + d2.getZeros();

    if (atRealLength < d2AtRealLength) {
      diff = d2AtRealLength - atRealLength;
      temp = at*this.positivePower(10, diff-1);
      resultAt = d2At + temp;
      greatestLength = d2AtRealLength;
    } else if (d2AtRealLength < atRealLength) {
      diff = atRealLength - d2AtRealLength;
      temp = d2At* this.positivePower(10, diff-1);
      resultAt = at + temp;
      greatestLength = atRealLength;
    } else {
      diff = 0;
      resultAt = at + d2At;
      greatestLength = atRealLength;
    }

    resultAtLength = this.computeLength(resultAt);
    resultZeros = greatestLength - resultAtLength;

    if (greatestLength < resultAtLength) {
      carry = resultAt / this.positivePower(10, resultAtLength -1);
      resultAt = resultAt - carry*this.positivePower(10, resultAtLength -1);
      resultAtLength = resultAtLength - 1;
    } else {
      carry = 0;
    }

    resultBt = bt + d2Bt + carry;

    result = new FalseDouble().create(resultBt, resultAt, resultZeros); //TODO: Check

    return result;
  }

  def divideIntegers(d2: FalseDouble): FalseDouble = {
    var d2Bt: Int;

    var resultAt: Int;
    var resultBt: Int;
    var result: FalseDouble;
    var resultZeros: Int;

    var counter: Int;
    var intermVal: Int;
    var limit: Int;

    d2Bt = d2.getBt();
    resultAt = 0;
    resultBt = 0;
    result = new FalseDouble().create(0, 0, 0);
    resultZeros = 0;

    counter = 0;
    intermVal = 0;
    limit = 0;

    if (bt == d2Bt) {
      resultBt = 1;
      resultAt = 0;
    } else {
      if(d2Bt < bt) {
        resultBt = bt / d2Bt;
        intermVal = bt - resultBt*d2Bt;
      } else {
        resultBt = 0;
      }
      intermVal = bt*10;

      resultAt = 0;
      counter = 0;
      limit = 8;
      resultZeros = 0;

      while (counter < limit) {

        if(d2Bt < intermVal){
          if(resultAt == 0) {
            resultZeros = resultZeros + 1;
          }
          resultAt = resultAt*10 + intermVal / d2Bt;
          intermVal = intermVal - (intermVal/d2Bt)*d2Bt;
        } else {
          if(resultAt == 0) {
            resultZeros = resultZeros + 1;
          }
          resultAt = resultAt*10;
        }

        if(200000000 < intermVal) {
          println("overflow!");
          counter = limit;
        } else {
          intermVal = intermVal * 10;
          counter = counter + 1;
        }
      }
    }

    result = new FalseDouble().create(resultBt, resultAt, resultZeros);
    
    return result;
  }
  
  def positivePower(x: Int, y: Int): Int ={
     var counter: Int;
     var result: Int;
     result = x;
     counter = y;
     if (y < 0) {
       y = 0 - y;
     }
     if(y == 0) {
       result = 1;
     } else {
       while (1 < counter) {
         result = result * x;
         counter = counter - 1;
       }
     }
    return result;
  }

  def getBt() : Int = {
    return bt;
  }

  def getAt(): Int = {
    return at;
  }

  def getAtLength(): Int = {
    return atLength;
  }

  def getNumber(): String = {
    return number;
  }

  def getZeros(): Int = {
    return zeros;
  }
}
