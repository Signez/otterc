object Matrix {
    def main() : Unit = {
        if (((new Mat().create2Unity()).mult((new Mat()).create())).display()==true) {}        
    }
}

class Mat {
	var values:Int[];
	
	def get(pos: Int) : Int = {
		return values[pos];
	}
	
    def  create() : Mat = {
    	values = new Int[9]; values[0]=1; values[1]=2; values[2]=3; values[3]=4; values[4]=5; values[5]=6; values[6]=7; values[7]=8; values[8]=9;
    	return this;
    }
    
    def  create2Unity() : Mat = {
    	values = new Int[9]; values[0]=2; values[1]=0; values[2]=0; values[3]=0; values[4]=2; values[5]=0; values[6]=0; values[7]=0; values[8]=2;
    	return this;
    }
    
    def display(): Bool = {
    	var j: Int; var temp: Int; var stemp: String; temp=0; j=0; 
    	stemp="";
    	while(j<9) {
    		if(values[j] <10) {
    			stemp=stemp+("  "+values[j]+" ");
    		} else {
    			stemp=stemp+(" "+values[j]+" ");
    		} j=j+1;
    		if(temp<(j-this.mod(j,3))/3) { println(stemp); stemp="";}
    		temp=(j-this.mod(j,3))/3;
    		
    	}
    	return false;
    }
    
    def mult(m2: Mat): Mat = {
    	var m1: Mat; var result: Int[];
    	var i:Int; var test:Bool; var k: Int;
    	var j:Int; j=0; m1=this; i=0; k=0;
    	result = new Int[9];
    	println("We are multiplying the matrix");
    	test=m1.display();
    	println("with the matrix: ");
    	test=m2.display(); println("");
    	println("Result: ");
    		while(i<9) {
    			k=(i-this.mod(i,3))/3;
    			result[(j*3)+k] = (m1.get(k*3)*m2.get(j*3))+(m1.get((k*3)+1)*m2.get(j*3+1)+(m1.get((k*3)+2)*m2.get(j*3+2)));
    			j=this.mod(j+1,3); i=i+1;
    		}
    	
    	values = result;
    		return this;
    }
    
        def mod(n: Int, m: Int): Int = {
    	return (n-(n/m)*m);
    }
    
}
