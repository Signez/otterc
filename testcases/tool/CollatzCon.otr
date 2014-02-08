/*
Author: Ismet Burak Kadron, Group 47
*/
object CollatzCon {
    def main() : Unit = {
		println(new CC().Loop(2,50));
    }
}
/*
	Tries collatz conjecture from 2 to given number and prints the sequence and the length of the sequence till it reaches 1.
	In the end, it prints average length of sequence.
*/
class CC {

	//Returns b in the equation num = b (mod m).
    def Mod(num : Int, modula : Int) : Int = {
		var num2 : Int;
		num2 = num / modula;
		return num - (num2 * modula);
    }

	//main function didn't let me declare any variables for the loop, so I made my own in this class.
	def Loop(start:Int, n:Int) : Int = {
		var i : Int;
		var tCount : Int;
		tCount = 0;
		if(0<start){
		i = start;
		}else{
			i = 2;
		}
		while(i<(n+1)){
			println(this.Hailstone(i));
			i = i+1;
		}
		return 0;
	}

	//Implementation of Collatz conjecture problem.
	def Hailstone(n : Int) : Int = {
		var count : Int;
		var temp : Int;
		var seqStr : String;
		
		seqStr = "";
		count = 1;
		temp = n;
		//A different way to say temp>1. I wanted to try and/or/not clauses in Tool language.
		while(!(temp<1 || temp==1)){
			seqStr = seqStr + temp + "-";
			if(this.Mod(temp,2) == 1){
				temp = 3*temp+1;
			}else{
				temp = temp/2;
			}
			count = count +1;
		}
		seqStr = seqStr + temp;
		println(seqStr);
		println("Length of the sequence of " + n + " is: " + count);
		return 0;
	}
}
