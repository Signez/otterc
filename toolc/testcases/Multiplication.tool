object Multiplication{
	def main() : Unit = {
		println(new Table().createTable(10));
	}
}

class Table{
	def createTable(nbr: Int) : String = {
		var i: Int;
		var j: Int;
		
		i=0;
		while(i<(nbr+1)){
			println("Table of "+ i + ":");
			j=0;
			while(j<11){
				println(i+" * "+j+" = "+i*j);
				j=j+1;
			}
			i=i+1;
			println("");
		}
		return "Done.";
	}
}