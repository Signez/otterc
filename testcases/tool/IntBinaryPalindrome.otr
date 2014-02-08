object IntBinaryPalindrome {
    def main() : Unit = {
		println(new Util().intToBinaryTab(112));
    }
}

class Util {

	def isPalindrome(tab : Int[], start : Int, end : Int) : Bool = {
		var b : Bool;
		b = false;
		if(end<start) {
			b = true;
		}
		else {
			if(start == end) {
				b = true;
			}
			if(tab[start] == tab[end]) {
				b = this.isPalindrome(tab, start+1, end-1);
			}
		}
		
		return b;
	}
	def intToBinaryTab(i: Int) : Bool = {
		var tab : Int[];
		var temp : Int;
		var l : Int;
		var index : Int;
		var modulo : Int;
		temp = i;
		l = 0;
		while(0<temp) {
			l = l+1;
			temp = temp/2;
		}
		tab = new Int[l];
		index = 0;
		while(index<l) {
			modulo = this.mod(i,2);
			if(modulo == 0) {
				tab[index] = 0;
			}
			else {
				tab[index] = 1;
			}
			index = index + 1;
			i = i/2;
		}
		return this.isPalindrome(tab, 0, index-1);
	}
	def mod(m : Int, n : Int) : Int = {
        return m - (n * (m / n));
    }
}
