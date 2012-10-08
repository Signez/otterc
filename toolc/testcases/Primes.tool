object Primes {
	// This programs find each prime number from 0 to selected number (50 by default)
    def main() : Unit = {
        println(new PrimesComputing().computePrimes(50));     
    }
}

class PrimesComputing {
	// This method iterates numbers from 0 to max and tells us for each of them if they are prime.
    def computePrimes(max : Int) : String = {
    	var i : Int;
        var ret : String;
        i = 2;
        ret = " Primes : ";
    	println("");
    	println(" -------------------------------");
    	println(" Finding all primes from 0 to " + max + " ");
    	println(" -------------------------------");
    	println("");
        while(i < max + 1) {
            if (this.isPrime(i)) {
            	ret = ret + (i + " ");
        	}
        	i = i + 1;
        	
        }
        println(ret);
        println("");
        return " Tests finished";
    }
    // This method checks if n is prime. It first eliminates all number inferior to 1 and all number which could be divided by 2.
    // Then, it checks all integers number between 2 and the square root of n.
    def isPrime(n : Int) : Bool = {
    	var ret : Bool;
    	var j : Int;
    	ret = true;
    	j = 0;
    	if (!(n < 2)) {
    		if (n == 2) {
    			ret = true;
    		} else {
    			if (this.mod(n,2) == 0) {
    				ret = false;
    			} else {
    				j = 3;
    				while ((j * j < n) || (j * j == n)) {
						if (this.mod(n,j) == 0) {
							ret = false;
						}
						j = j + 2;
    				}
    			}
    		}
    	} else {
    		ret = false;
    	}
    	return ret;
    }
    // This method defines the "modulo" operator, meaning, it returns x mod(n) or x % n.
    def mod(x: Int, n:Int) : Int = {
    	return (x - ((x / n) * n));
    }
}
