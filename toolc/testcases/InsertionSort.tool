/*
Author: Ismet Burak Kadron, Group 47
*/
object InsertionSort {
    def main() : Unit = {
        println(new IS().Start(20, 89383));
    }
}
/*
	Initializes, sorts and prints data before and after sort execution.
	Uses Insertion sort as sort algorithm.
*/
class IS {
	var numList : Int[];
	var size : Int;
	var seed : Int;
	var numLim : Int;
	
    def Start(sz : Int, sd : Int) : Int = {
        var i : Int;
        numLim = this.Pow(2,16) - 1;
        
        i = this.Init(sz,sd);
        i = this.Print();
        i = this.Sort();
        i = this.Print();
        return 0;
    }
 
    //Sorts given list using Insertion Sort algorithm
    def Sort() : Int = {
        var i : Int;
        var j : Int;
		var temp : Int;

		i = 0;
		while (i<size){
			j = i;
			while (0<j){
				if(numList[j] < numList[j-1]){
					temp = numList[j-1];
					numList[j-1] = numList[j];
					numList[j] = temp;
					j = j - 1;
				}else{
					j = 0;
				}
			}
			i = i + 1;
		}
        return 0;
    }
    
    //Returns b in the equation num = b (mod m).
    def Mod(num : Int, modula : Int) : Int = {
		var num2 : Int;
		if (num < 0){
			num2 = 0-(num / modula);
			num = num + (num2 + 1) * modula;
		}
		num2 = num / modula;
		return num - (num2 * modula);
    }
    
    //Prints elements of the list on the output.
    def Print() : Int = {
        var i : Int;
        var str : String;
        
        i = 0 ;
        str = "";
        while (i < size) {
            str = str + numList[i] + " ";
            i = i + 1 ;
        }
        println(str);
        return 0;
    }
    
    //Returns the power of given number.
    def Pow(num : Int, pow : Int) : Int = {
		var temp : Int;
		var i : Int;
		
		temp = 1;
		i = 0;
		while(i<pow){
			temp = temp * num;
			i = i + 1;
		}
		return temp;
    }
    
    /*  A psuedo random generator.
		Generates random numbers according to the linear congruential generator.
		next parameter takes previous generated random number or seed.
		count parameter takes the iteration number.
		limit parameter has the bounds of generated number. (0<rand<limit-1) 
    */
    def Rand(limit : Int) : Int = {
		seed = this.Mod(16807 * seed, numLim);
		return this.Mod(this.Mod(16807 * seed, numLim), limit);
		
    }

    // A method for initialization of integer array. Uses Rand function for randomness.
    def Init(sz : Int, sd : Int) : Int = {
        var i : Int;
	    var next : Int;
	    
        size = sz;
        seed = sd;
        numList = new Int[sz];
	    
	    next = seed;
		i = 0;
		while (i<size){
			numList[i] = this.Rand(200);
			next = numList[i];
			i = i+1;
		}
        return 0;  
    }
	
}
