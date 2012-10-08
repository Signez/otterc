// http://projecteuler.net/problem=1
// answer is 233168

object Euler1 {
    def main() : Unit = {
        println(new Problem1().answer());
    }
}

class Problem1 {
    def sumMultiples(multiple : Int, upperBound : Int) : Int = {
        var sum : Int;
        var x : Int;

        sum = 0;
        x = multiple;

        while (x < upperBound) {
            sum = sum + x;
            x = x + multiple;
        }

        return sum;
    }

    def answer() : Int = {
        var result : Int;
        var UPPER_BOUND : Int;

        result = 0;
        UPPER_BOUND = 1000;

        result = result + this.sumMultiples(3, UPPER_BOUND);
        result = result + this.sumMultiples(5, UPPER_BOUND);
        result = result - this.sumMultiples(3*5, UPPER_BOUND);

        return result;
    }
}
