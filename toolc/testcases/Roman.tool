// Johan Berdat

object Roman {
	def main(): Unit = {
		println(new Converter().convert(1337));
	}
}

class Converter {
	def convert(n: Int): String = {
		var r: String;
		// Process x
		r = this.processDigit(this.mod(n, 10), "I", "V", "X");
		// Process x0
		n = n / 10;
		r = this.processDigit(this.mod(n, 10), "X", "L", "C") + r;
		// Process x00
		n = n / 10;
		r = this.processDigit(this.mod(n, 10), "C", "D", "M") + r;
		// Process x000
		n = n / 10;
		r = this.repeat("M", n) + r;
		return r;
	}
	
	def mod(x: Int, n: Int): Int = { return x - (x / n) * n; }
	
	def repeat(s: String, n: Int): String = {
		var r: String;
		r = "";
		while (!(n < 1)) { // We should add something to handle ">"...
			r = r + s;
			n = n - 1; // Since toolc-ref accepts this, I assume that we can modify arguments.
		}
		return r;
	}
	
	def processDigit(n: Int, low: String, mid: String, high: String): String = {
		var r: String;
		if (n < 4)
			r = this.repeat(low, n);
		else if (n == 4)
			r = low + mid;
		else if (n < 9)
			r = mid + this.repeat(low, n - 5);
		else
			r = low + high;
		return r;
	}
}