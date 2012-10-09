object TestingBubble {
    def main() : Unit = { 
        println(new Sort().init().bubbleSort().convertArrayToString());      
    }
}
 
class Sort {
	var numbers : Int[] ;
    def bubbleSort() : Sort = {
		var swap:Bool;
		var j:Int;
		var temp:Int;
		swap=true;
		while(swap)
			{
				swap=false;
				j=0;
				while(j<numbers.length-1)
					{
						if(numbers[j+1]<numbers[j])
							{
								temp=numbers[(j+1)];
								numbers[(j+1)]=numbers[j];
								numbers[j]=temp;
								temp=0;
								swap=true;
							}
						j=j+1;
					}
			}
		return this;
    }
	def convertArrayToString():String =
	{
		var output:String;
		var i:Int;
		output="";
		i=0;
		while(i<numbers.length)
			{
				output = output+numbers[i]+", ";
				i=i+1;
			}
		return output;
	}
    def init() : Sort = {	
	numbers = new Int[10];
	numbers[0] = 20 ;
	numbers[1] = 7  ; 
	numbers[2] = 12 ;
	numbers[3] = 18 ;
	numbers[4] = 2  ; 
	numbers[5] = 11 ;
	numbers[6] = 6  ; 
	numbers[7] = 9  ; 
	numbers[8] = 19 ; 
	numbers[9] = 5  ;
	return this;
    }
}