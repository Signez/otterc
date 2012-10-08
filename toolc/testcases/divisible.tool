object Divisible {
    def main() : Unit = {
        if ((new Div()).isDivisibleBy3(13)==true)  { println("Le nombre est divisible par 3!"); } else { println("Le nombre n'est pas divisible par 3!"); }  
       // if ((new Div()).isDivisibleBy5(390285)==true)  { println("Le nombre est divisible par 5!"); } else { println("Le nombre n'est pas divisible par 5!"); }  
    }
}

class Div {
    def isDivisibleBy3(num: Int) : Bool = {
	    var sum : Int; var n : Int; var m : Int;
	    n = num;
	    sum = 0;
	    
	    while(0<n) {
	      	m = this.mod(n,10);
	        sum = sum + m;
	        n = (n-m)/10;
	   	}
	    
	    return (this.mod(sum,3)==0);
}
    
    def isDivisibleBy5(num: Int): Bool = {
    	 return (this.mod(num,5)==0);
  	}
    
    def mod(n: Int, m: Int): Int = {
    	return (n-(n/m)*m);
    }
 }
