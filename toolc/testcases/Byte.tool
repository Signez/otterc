object Byte {
	def main(): Unit = {
		println(new B().init(35).shiftRight(1).toInt());
	}
}


class Util { 
    def mod(m : Int, n : Int) : Int = {
        return m - (n * (m / n));
    }
}

class B {
	var array : Int[];
	
	def init(n : Int) : B = {
		var i : Int;
		array = new Int[8];
		
		i = 8;
		while(0 < i) {
			array[i-1] = new Util().mod(n, 2);
			n = n/2;
			i = i - 1;
		}
		i = 0;
		while(i < 8) {
			println(array[i]);
			i = i+1;
		}
		println("***");
		return this;
	}
	
	def shiftLeft(n: Int) : B = {
		var i : Int;
		
		i = 0;
		
		while(i < 8 - n) {
			array[i] = array[i+1];
			i = i + 1;
		}
		
		while(i < 8) {
			array[i] = 0;
			i = i+1;
		}
		
		while(i < 8) {
			println(array[i]);
			i = i+1;
		}
		
		return this;
	}
	
	def shiftRight(n: Int) : B = {
		var i : Int;
		
		i = 8;
		
		while((0 + n) < i) {
			array[i-1] = array[i-2];
			i = i - 1;
		}
		
		while(0 < i) {
			array[i-1] = 0;
			i = i-1;
		}
		
		while(i < 8) {
			println(array[i]);
			i = i+1;
		}
		
		return this;
	}
	
	def toInt() : Int = {
		var i: Int;
		var result: Int;
		var power: Int;
		
		i = 8;
		result = 0;
		power = 1;
		
		while(0 < i) {
			result = result + array[i-1]*power;
			power = power*2;
			i = i - 1;
		}
		
		return result;		
	}
}

	