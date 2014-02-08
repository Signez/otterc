/**
 * This program will transform a simple number into a "Martian" number.
 * Martian number is defined like the following:
 *   - Multiples of eight thousands (8000) are written "$".
 *   - Multiples of one hundred and eight (108) are written "*".
 *   - Multiples of forty-two (42) are written "#" ;
 *   - Multiples of seven (7) are represented by several pipe characters "|" ;
 *   - Units are represented by points ".".
 *
 * This is obviously a very simple test case which use basic operations like string
 * concatenations, integer divisions, loops, and simple method calls.
 *
 * Author: Stanislas Signoud <stanislas.signoud@epfl.ch>
 * Date: 02.10.2012
 */

object Martianizer {
	def main () : Unit = {
		println(new UnitTester().runTests());
	}
}

class UnitTester {
	var martianizer : Martian;
	
	def testWith(original: Int, expecting: String) : String = {
		var result : String;

		martianizer = new Martian();	
		
		result = martianizer.martianize(original);
		
		println("[TEST] Number => " + original);
		println("    Expecting => " + expecting);
		println("       Result => " + result);
		
		return " -- ";
	} 
	
	
	// There were some great code about auto-checking the result,
	// counting failures and showing which results were wronG ;
	// as we can't check if two string are equals (even manually)
	// in the Tool programming language, this is not possible. :(
	def runTests() : String = {	
		
		println(this.testWith(65203, "$$$$$$$$***********||."));
		println(this.testWith(404, "***#|||||..."));
		println(this.testWith(2242, "********************#|||||....."));
		println(this.testWith(442, "****|..."));
		
		return "Done.";
	}
}

class Martian {
	def format(eightThousands: Int, oneHundredEights: Int, fortyTwos: Int, sevens: Int, units: Int): String = {
		var i: Int;
		var output: String;
		output = "";
		
		i = 0;
		while(i < eightThousands) {
			i = i + 1;
			output = output + "$";
		}
		
		i = 0;
		while(i < oneHundredEights) {
			i = i + 1;
			output = output + "*";
		}
		
		i = 0;
		while(i < fortyTwos) {
			i = i + 1;
			output = output + "#";
		}
		
		i = 0;
		while(i < sevens) {
			i = i + 1;
			output = output + "|";
		}
		
		i = 0;
		while(i < units) {
			i = i + 1;
			output = output + ".";
		}
		
		return output;
	}

	def martianize(original: Int) : String = {
		var the8000s: Int;
		var the108s: Int;
		var the42s: Int;
		var the7s: Int;
		var the1s: Int;
		
		the8000s = original / 8000;
		
		the108s = (original - the8000s * 8000) / 108;
		
		the42s = (original - the8000s * 8000 - the108s * 108) / 42;
		
		the7s = (original - the8000s * 8000 - the108s * 108 - the42s * 42) / 7;
		
		the1s = original - the8000s * 8000 - the108s * 108 - the42s * 42 - the7s * 7;
		
		return this.format(the8000s, the108s, the42s, the7s, the1s);
	}
}
