object Noel {
	def main() : Unit = {
		println(new Sapin().createTree(35));
	}
}

class Sapin {
	def createTree(lines: Int) : String = {
		var currentLine: Int;
		var blank: Int;
		var symbol: Int;
		var lineToPrint: String;
		var guirlande: Bool;
		
		currentLine = 1;		
		blank = 0;	
		symbol = 0;
		guirlande = true;
		while(!(currentLine == lines)){
			lineToPrint = "";
			blank = lines-currentLine;
			while(!(blank == 0)){
				lineToPrint = lineToPrint + " ";
				blank = blank - 1;
			}
			symbol = currentLine * 2;
			while(!(symbol == 0)){
				if(guirlande){
					lineToPrint = lineToPrint + "*";
				}else{
					lineToPrint = lineToPrint + "0";
				}
				guirlande = !(guirlande);
				symbol = symbol-1;
			}
			println(lineToPrint);
			currentLine = currentLine + 1;
		}			
			return "Creation of a tree with " + lines + " lines: SUCCESSFUL!";
	}
}