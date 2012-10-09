object Pascal {
	def main() : Unit = {
		// Print the 10th line of the Pascal's triangle
		println(new PS().showTriangle(10));
	}
}

class PS {
	def computeTriangle(num : Int) : Int[] = {
		var tmp : Int[];
		var i : Int;
		var before : Int[];
		tmp = new Int[num];
		tmp[0] = 1;
		if (1 < num) {
			before = new Int[num - 1];
			i = 1;
			tmp[num - 1] = 1;
			while (i < num - 1) { 
				before = this.computeTriangle(num-1);
				tmp[i] = before[i - 1] + before[i]; 
				i = i + 1;
			}
		}
		return tmp;
	}

	def showTriangle(num : Int) : String = {
		return this.toString(this.computeTriangle(num));
	}

	def toString(tab : Int[]) : String = {
		var str : String;
		var i : Int;
		i = 0;
		str = "";
		while (i < tab.length) {
			str = str + tab[i] + " ";
			i = i + 1;
		}
		return str;
	}
}
