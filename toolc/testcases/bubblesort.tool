object bubblesort {
	def main(): Unit = {
		println(new BS().init().Start().print());
	}
}

class BS {
	var array: Int[];
	
	def init(): BS = {
		array = new Int[10];
		array[0] = 15;
		array[1] = 12;
		array[2] = 1;
		array[3] = 3;
		array[4] = 9;
		array[5] = 14;
		array[6] = 7;
		array[7] = 8;
		array[8] = 20;
		array[9] = 5;
		
		return this;
	}
	
	def Start(): BS = {
		var n: Int;
		var newn: Int;
		var i: Int;
		var tmp: Int;
		
		n = 10;
		while(0 < n) {
			newn = 0;
			i = 1;
			while(i < n) {
				if(array[i] < array[i-1]) {
					tmp = array[i];
					array[i] = array[i-1];
					array[i-1] = tmp;
					newn = i;
				}
				i = i+1;
			}
			n = newn;
		}
		
		return this;
	}
	
	def print(): String = {
		return array[0] + "|" + array[1] + "|" + array[2] + "|" + array[3] + "|" + array[4] + "|" + array[5] + "|" + array[6] + "|" + array[7] + "|" + array[8] + "|" + array[9];
	}
	
}