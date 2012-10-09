object Matrix {
	def main() : Unit = {
		if(new Multiplication().calc()) { println("Operations successful !"); } else { println("Error !"); }
	}
}

class Multiplication {
	var check : Bool;
	var matSize : Int;
	var matA1 : Int[];
	var matA2 : Int[];
	var matA3 : Int[];
	var matB1 : Int[];
	var matB2 : Int[];
	var matB3 : Int[];
	var matC1 : Int[];
	var matC2 : Int[];
	var matC3 : Int[];
	
	var row : Int[];
	var rowTemp : Int[];
	
	var i : Int;
	var count : Int;
	var result : Int;
	
	def calc() : Bool = {
		check = this.init();
		println("");
		check = this.wait(20);
		println("Computing Matrix A..."); println("");
		check = this.wait(50);
		matA1[0] = 387; matA2[0] = 326; matA3[0] = 525;
		matA1[1] = 432; matA2[1] = 587; matA3[1] = 372;
		matA1[2] = 745; matA2[2] = 378; matA3[2] = 284;
		check = this.simplePrint("Matrix A", matA1, matA2, matA3);
		check = this.wait(20);
		println("Computing Matrix B..."); println("");
		check = this.wait(50);
		matB1[0] = 321; matB2[0] = 452; matB3[0] = 434;
		matB1[1] = 683; matB2[1] = 292; matB3[1] = 214;
		matB1[2] = 131; matB2[2] = 361; matB3[2] = 987;
		check = this.simplePrint("Matrix B", matB1, matB2, matB3);
		check = this.wait(20);
		println("Multiplication A x B..."); println("");
		check = this.wait(50);
		matC1[0] = this.compute(this.createRow(0, matA1, matA2, matA3), matB1);
		matC1[1] = this.compute(this.createRow(1, matA1, matA2, matA3), matB1);
		matC1[2] = this.compute(this.createRow(2, matA1, matA2, matA3), matB1);
		matC2[0] = this.compute(this.createRow(0, matA1, matA2, matA3), matB2);
		matC2[1] = this.compute(this.createRow(1, matA1, matA2, matA3), matB2);
		matC2[2] = this.compute(this.createRow(2, matA1, matA2, matA3), matB2);
		matC3[0] = this.compute(this.createRow(0, matA1, matA2, matA3), matB3);
		matC3[1] = this.compute(this.createRow(1, matA1, matA2, matA3), matB3);
		matC3[2] = this.compute(this.createRow(2, matA1, matA2, matA3), matB3);
		check = this.printMult();
		check = this.wait(20);
		println("Computing determinant..."); println("");
		check = this.wait(50);
		println("Det[A] = " + this.computeDet(matA1, matA2, matA3)); println("");
		check = this.wait(50);
		println("Det[B] = " + this.computeDet(matB1, matB2, matB3)); println("");
		check = this.wait(50);
		println("Det[C] = " + this.computeDet(matC1, matC2, matC3)); println("");
		check = this.wait(20);
		return check; 
	}
	
	def init() : Bool = {
		matSize = 3;
		matA1 = new Int[matSize];
		matA2 = new Int[matSize];
		matA3 = new Int[matSize];
		matB1 = new Int[matSize];
		matB2 = new Int[matSize];
		matB3 = new Int[matSize];
		matC1 = new Int[matSize];
		matC2 = new Int[matSize];
		matC3 = new Int[matSize];
		row = new Int[matSize];
		rowTemp = new Int[matSize];
		return true;
	}	
	def wait(time : Int) : Bool = {
		i = 0;
		while (i < time) {
			count = 0;
			while (count < 1000000000) {
				count = count + 1;
			}
			i = i + 1;
		}
		return true;
	}
	def createRow(num : Int, colA : Int[], colB : Int[], colC : Int[]) : Int[] = {
		rowTemp[0] = colA[num];
		rowTemp[1] = colB[num];
		rowTemp[2] = colC[num];
		return rowTemp;
	}
	def compute(row : Int[], col : Int[]) : Int = {
		result = (row[0] * col[0]) + (row[1] * col[1]) + (row[2] * col[2]);
		return result;
	}	
	def computeDet(colA : Int[], colB : Int[], colC : Int[]) : Int = {
		result = colA[0]*colB[1]*colC[2] - colA[0]*colB[2]*colC[1] - colB[0]*colA[1]*colC[2] + colB[0]*colA[2]*colC[1] + colC[0]*colA[1]*colB[2] - colC[0]*colA[2]*colB[1];
		return result;
	}
	def simplePrint(name : String, colA : Int[], colB : Int[], colC : Int[]) : Bool = {
		println("     " + name);
		println("| " + colA[0] + "  " + colB[0] + "  " + colC[0] + " |");
		println("| " + colA[1] + "  " + colB[1] + "  " + colC[1] + " |");
		println("| " + colA[2] + "  " + colB[2] + "  " + colC[2] + " |");
		println("");
		return true;
	}	
	def printMult() : Bool = {
		println("     Matrix A              Matrix B                  Matrix C");
		println("| "+matA1[0]+"  "+matA2[0]+"  "+matA3[0]+" |     | "+matB1[0]+"  "+matB2[0]+"  "+matB3[0]+" |     | "+matC1[0]+"  "+matC2[0]+"  "+matC3[0] + " |");
		println("| "+matA1[1]+"  "+matA2[1]+"  "+matA3[1]+" |  X  | "+matB1[1]+"  "+matB2[1]+"  "+matB3[1]+" |  =  | "+matC1[1]+"  "+matC2[1]+"  "+matC3[1] + " |");
		println("| "+matA1[2]+"  "+matA2[2]+"  "+matA3[2]+" |     | "+matB1[2]+"  "+matB2[2]+"  "+matB3[2]+" |     | "+matC1[2]+"  "+matC2[2]+"  "+matC3[2] + " |");
		println("");
		return true;
	}
}
