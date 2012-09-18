object Pi {
    def main() : Unit = {
        if(new Computer().computePi()) { println("Ok"); } else { println("error"); }
    }
}

class Computer {
    def computePi() : Bool = {
        var j : Int;
        var value : Frac;
        var inter : Real;

        println("First method");
        println("************");

        value = new Frac().init(0,1);
        j = 0;
        while(j < 3) {
            println(value.toString() + " ~= " + new Real().init(0,10).evalFrac(value).toString());
            value = value.plus(this.getTerm(j));
            j = j + 1;
        }
        println(value.toString() + " ~= " + new Real().init(0,10).evalFrac(value).toString());

        println("");
        println("Second method");
        println("*************");

        value = new Frac().init(0,1);
        inter = new Real().init(0,10).evalFrac(value);
        j = 0;
        while(j < 7) {
            println(inter.toString());
            value = this.getTerm(j);
            inter = inter.plus(new Real().init(0,10).evalFrac(value));
        
            j = j + 1;
        }
        println(inter.toString());

        return true;
    }

    def getTerm(i : Int) : Frac = {
        var par : Frac;
        var first : Frac;
        var second : Frac;
        var third : Frac;
        var fourth : Frac;

        first  = new Frac().init(4, 8 * i + 1);
        second = new Frac().init(2, 8 * i + 4);
        third  = new Frac().init(1, 8 * i + 5);
        fourth = new Frac().init(1, 8 * i + 6);
        par = first.minus(second).minus(third).minus(fourth);

        return par.times(new Frac().init(1, this.sixteenPow(i)));
    }

    def sixteenPow(n : Int) : Int = {
        var res : Int;
        var i : Int;

        res = 1;
        i = 0;

        while(i < n) {
            i = i + 1;
            res = res * 16;
        }

        return res;
    }
}

class Frac {
    var numerator : Int;
    var denominator : Int;
    var sign : Bool; // true means positive.
    var util : Util;

    def init(n : Int, d : Int) : Frac = {
        util = new Util();

        numerator = util.abs(n);
        denominator = util.abs(d);
        sign = (n < 0 && d < 0 || (0 < n || n == 0) && (0 < d || d == 0)); 
        return this.simplify();
    }

    def getNumerator() : Int = {
        return numerator;
    }

    def getDenominator() : Int = {
        return denominator;
    }

    def setPos(positive : Bool) : Frac = {
        sign = positive;
        return this;
    }

    def isPos() : Bool = {
        return sign;
    }

    def simplify() : Frac = {
        var gcd_ : Int;

        if(!(numerator == 0) && !(denominator == 0)) {
            gcd_ = util.gcd(numerator, denominator);

            if(!(gcd_ == 1)) {
                numerator = numerator / gcd_;
                denominator = denominator / gcd_;
            }
        }

        return this;
    }

    def plus(other : Frac) : Frac = {
        var lcm : Int;
        var lfac : Int;
        var rfac : Int;

        lcm = util.lcm(denominator, other.getDenominator());
        lfac = lcm / denominator;

        if(!sign) {
            lfac = 0 - lfac;
        }

        rfac = lcm / other.getDenominator();

        if(!other.isPos()) {
            rfac = 0 - rfac;
        }

        return (new Frac()).init((lfac * numerator) + (rfac * other.getNumerator()), lcm);
    }

    def minus(other : Frac) : Frac = {
        return this.plus(other.negative());
    }

    def times(other : Frac) : Frac = {
        return (new Frac()).init(numerator * other.getNumerator(), denominator * other.getDenominator()).simplify().setPos(this.isPos() && other.isPos() || !this.isPos() && !other.isPos());
    }

    def divided(other : Frac) : Frac = {
        return this.times(other.inverse());
    }

    def inverse() : Frac = {
        return (new Frac()).init(denominator, numerator);
    }

    def negative() : Frac = {
        return (new Frac()).init(numerator, denominator).setPos(false);
    }

    def toString() : String = {
        var result : String;
        if(sign) {
            result = "";
        } else {
            result = "-";
        }
        return result + numerator + "/" + denominator;
    }
}

// represents real numbers as a number plus an array containing the digits
class Real {
    var integerPart : Int;
    var digits : Int[];
    var util : Util;

    def init(intPart : Int, digitsCount: Int): Real = {
        var i : Int;

        util = new Util();
        integerPart = intPart;
        digits = new Int[digitsCount];
        i = 0;
        while(i < digitsCount) {
            digits[i] = 0;
            i = i + 1;
        }
        return this;
    }

    def getDigits() : Int[] = {
        return digits;
    }

    def getIntegerPart() : Int = {
        return integerPart;
    }

    def setIntegerPart(p : Int) : Real = {
        integerPart = p;
        return this;
    }

    def evalFrac(frac : Frac) : Real = {
        var leftover : Int;
        var i : Int;
        var den : Int;

        den = frac.getDenominator();
        integerPart = frac.getNumerator() / den;
        if(!frac.isPos()) {
            integerPart = 0 - integerPart;
        }
        leftover = util.mod(frac.getNumerator(), den);

        i = 0;
        while(i < digits.length) {
            leftover = 10 * leftover;
            digits[i] = leftover / den;
            leftover = util.mod(leftover, den);
            i = i + 1;
        }
        return this;
    }

    // note that this only works for positive reals
    def plus(other : Real) : Real = {
        var len : Int;
        var od : Int[];
        var resDig : Int[];
        var carry : Int;
        var i : Int;
        var sum : Int;
        var result : Real;

        od = other.getDigits();
        // taking the max length ensures that it will crash if they don't match :P
        if(digits.length < od.length) {
            len = od.length;
        } else {
            len = digits.length;
        }

        result = new Real().init(0, len);
        resDig = result.getDigits();

        carry = 0;
        i = len - 1;

        while(!(i < 0)) {
            sum = digits[i] + od[i] + carry;
            carry = sum / 10;
            //println(digits[i] + " + " + od[i] + " = " + sum + "(" + carry + ")");
            resDig[i] = util.mod(sum, 10);
            i = i - 1;
        } 

        return result.setIntegerPart(integerPart + other.getIntegerPart() + carry);
    }

    def toString() : String = {
        var ret : String;
        var i : Int;

        ret = "" + integerPart + ".";
        i = 0;
        while(i < digits.length) {
            ret = ret + digits[i];
            i = i + 1;
        }
        return ret;
    }
}

// Some useful stuff
class Util {
    def abs(v : Int) : Int = {
        var res : Int;

        if(!(v < 0)) {
            res = v;
        } else {
            res = 0 - v;
        }
        return res;
    }

    def gcd(m_ : Int, n_ : Int) : Int = {
        var t : Int;
        var r : Int;
        var result : Int;
        var m : Int;
        var n : Int;

        m = this.abs(m_);
        n = this.abs(n_);

        if (m < n) {
            t = m;
            m = n;
            n = t;
        }

        r = this.mod(m,n); // m % n;

        if (r == 0) {
            result = n;
        } else {
            result = this.gcd(n, r);
        }
        return result;
    }

    def lcm(m : Int, n : Int) : Int = {
        return (n*m) / this.gcd(n,m);
    }

    def mod(m : Int, n : Int) : Int = {
        return m - (n * (m / n));
    }
}
