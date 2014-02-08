object IsPrime {
    def main() : Unit = {
		println(new Util().isPrime(1875));
    }
}

class Util {
	def isPrime(i : Int) : Bool = {
		var temp : Int;
		var b : Bool;
		b = true;
		temp = 2;
		while(temp<i) {
			if(this.mod(i,temp) == 0) {
				b = false;
			}
			temp = temp + 1;
		}
		return b;
	}
	
	def mod(m : Int, n : Int) : Int = {
        return m - (n * (m / n));
    }

}