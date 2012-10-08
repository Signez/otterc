/*
Programned by group 47
*/
object ScalarProduct {
	def main() : Unit = {
		println(new Scalar0Calculator1().compute());
	} 
}
class Scalar0Calculator1 {
	var sizeOfVectors: Int;
	def compute():Int={
		var welcome: String ;
		var vector1 : Int [ ];
		var vector2 : Int[] ;
		var i: Int;
		
		sizeOfVectors = 2;
		vector1 = new Int [sizeOfVectors];
		vector2 = new Int[ vector1.length];
		
		welcome = "Welcome to the example of scalar product calculator !";
		println(welcome);
		println("size : ");
		println(this.findSize());
		
		i = 0;
		while(i < 2){
			vector1[i] = 3;
			i = i +1;
		}
		
		vector2[0] = 5;
		vector2[1] = 6;
		
		return (vector1[0] * vector2[0] + vector1[ 1]* vector2[1]);
	}
	def findSize() : Int = {
		var i: Int;
		i = 0;
		println("value of i : must be 0");
		println(i);
		return sizeOfVectors;
	}
}
