// Johan Berdat

/*
	I don't know why, but the given compiler (toolc-ref) has two errors:
		- It doesn't seem to handle UTF8 properly (same error with Maze.tool)
		- It generate invalid bytecode, since JVM crash at startup ("Register 3 contains wrong type at MatrixRain.tool:11)
*/

object MatrixRain {
	def main(): Unit = {
		println(new Generator().init().print(20, 20));
	}
}

class Generator {
	var fib_a: Int;
	var fib_b: Int;
	
	def init(): Generator = {
		fib_a = 0;
		fib_b = 1;
		return this;
	}
	
	def stepFib(): Int = {
		var tmp: Int;
		tmp = fib_a + fib_b;
		fib_a = fib_b;
		fib_b = tmp;
		return tmp;
	}
	
	def getSymbol(): String = {
		var x: Int;
		var r: String;
		x = this.stepFib();
		x = x - (x / 104) * 104; // Compiler should not optimize this :)
		x = x / 4;
		// Anyway, a smart compiler should optimize this...
		r = " ";
		if (x == 0 ) r = "A";
		if (x == 1 ) r = "B";
		if (x == 2 ) r = "C";
		if (x == 3 ) r = "D";
		if (x == 4 ) r = "E";
		if (x == 5 ) r = "F";
		if (x == 6 ) r = "G";
		if (x == 7 ) r = "H";
		if (x == 8 ) r = "I";
		if (x == 9 ) r = "J";
		if (x == 10) r = "K";
		if (x == 11) r = "L";
		if (x == 12) r = "M";
		if (x == 13) r = "N";
		if (x == 14) r = "O";
		if (x == 15) r = "P";
		if (x == 16) r = "Q";
		if (x == 17) r = "R";
		if (x == 18) r = "S";
		if (x == 19) r = "T";
		if (x == 20) r = "U";
		if (x == 21) r = "V";
		if (x == 22) r = "W";
		if (x == 23) r = "X";
		if (x == 24) r = "Y";
		if (x == 25) r = "Z";
		return r;
	}
	
	def print(width: Int, height: Int): String = {
		var x: Int;
		var y: Int;
		var lengths: Int[];
		var line: String;
		var char: String;
		lengths = new Int[width];
		while (x < width) {
			lengths[x] = this.stepFib();
			lengths[x] = lengths[x] - (lengths[x] / height) * height; // again, compiler shouldn't optimize
			x = x + 1;
		}
		x = 0;
		while (y < height) {
			line = "";
			while (x < width) {
				if (y < lengths[x])
					char = " ";
				else
					char = this.getSymbol();
				line = line + char;
				x = x + 1;
			}
			y = y + 1;
			println(line);
		}
		return "";
	}
}
