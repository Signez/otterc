//Vectory_Calculator by Jonny Quarta
object Vector_Calculator {
    def main(): Unit = {
        println(new Calculator().Start());
    }
}
 
/* This class contains vector calculations */
class Calculator {
    var vector1: Int[];
    var vector2: Int[];
	var vector3: Int[];
 
	def PrintVector(vect : Int[]): String = {
		var i: Int;
		var output: String;
		i = 0;
		output = "[";
		
		while(i < vect.length){
			output = output + vect[i] + ",";
			i = i + 1;
		}
		
		output = output + "]";
		
		return output;
	}
	
 	// Defines the vectors value
	def InitializeVectors() : Int = {
		vector1 = new Int[3];
		vector2 = new Int[3];
		vector3 = new Int[3];
	
		vector1[0] = 1;
		vector1[1] = 25;
		vector1[2] = 16;
		
		vector2[0] = 7;
		vector2[1] = 11;
		vector2[2] = 76;
		
		vector3[0] = 18;
		vector3[1] = 64;
		vector3[2] = 20;
		
		return 0;
	}
	
	
    // Invoke methods to initialize and calculate
    def Start() : String = { 
        var q: Int;
		q = this.InitializeVectors();
		println("Vector 1 : " + this.PrintVector(vector1));
		println("Vector 2 : " + this.PrintVector(vector2));
		println("Vector 3 : " + this.PrintVector(vector3));
		
		println("Sum of vectors 1 and 2 : " + this.PrintVector(this.SumVectors(vector1, vector2)));
		println("Scalar product of vectors 2 and 3 : " + this.ScalarProduct(vector2, vector3));
		println("Vector product of vectors 1 and 3 : " + this.PrintVector(this.VectorProduct(vector1, vector3)));
		
        return "";
    }
 
	def SumVectors(vect1: Int[], vect2: Int[]): Int[] = {
		var out: Int[];
		out = new Int[3];
		out[0] = vect1[0] + vect2[0];
		out[1] = vect1[1] + vect2[1];
		out[2] = vect1[2] + vect2[2];
		
		return out;
	}
 
	def ScalarProduct(vect1: Int[], vect2: Int[]): Int = {		
		return (vect1[0] * vect2[0]) + (vect1[1] * vect2[1]) + (vect1[2] * vect2[2]);
	}
	
	def VectorProduct(vect1: Int[], vect2: Int[]): Int[] = {
		var out: Int[];
		out = new Int[3];
		out[0] = (vect1[1]*vect2[2]) - (vect1[2]*vect2[1]);
		out[1] = (vect1[2]*vect2[0]) - (vect1[0]*vect2[2]);
		out[2] = (vect1[0]*vect2[1]) - (vect1[1]*vect2[0]);
		
		return out;
	}
}