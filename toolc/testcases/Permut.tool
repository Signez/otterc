object Permut {
    def main() : Unit = {
        println(new Calculator().computePermutation());
    }
}
 
class Calculator {
    def computePermutation() : String = {
		var i : Int;
		var j : Int;
		var alphabet : Int[];
		i = 0;
		alphabet = new Int[5];
		alphabet[0] = 1;
		alphabet[1] = 2;
		alphabet[2] = 3;
		alphabet[3] = 4;
		alphabet[4] = 5;
		println("****************************************************");
		println("Finding pairs of elements from the set: {"+alphabet[0]+","+alphabet[1]+","+alphabet[2]+","+alphabet[3]+","+alphabet[4]+"}");
		println("****************************************************");

		while(i < alphabet.length){
			j = i;
			while(j < alphabet.length){
				println("[" + alphabet[i] + "-"+ alphabet[j] +"]");
				j = j + 1;
			}
			i = i + 1;
		}
		return "Finish";
    }
}