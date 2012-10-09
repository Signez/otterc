object Encrypt {
	def main(): Unit = {
		println(new SimpleEncryption().init("Hello world",11, 8).print());
	}
}

class SimpleEncryption {
	var offset : Int;
	var txtIn : Int[];
	
	//var encrypted : String;

	def init(vTxt : String, size : Int, vOffset : Int) : SimpleEncryption = {
		offset = vOffset;
		txtIn = new Int[11];
		txtIn[0]=33;
		txtIn[1]=4;
		txtIn[2]=11;
		txtIn[3]=11;
		txtIn[4]=14;
		txtIn[5]=0-1;
		txtIn[6]=22;
		txtIn[7]=14;
		txtIn[8]=17;
		txtIn[9]=11;
		txtIn[10]=3;

		return this;
	}
	
	def getChar(i : Int) : String = {
		var s : String;
		s = "";
		if (i < 0 ) s = " ";
		if ( i == 0 ) s = "a";
		if ( i == 1 ) s = "b";
		if ( i == 2 ) s = "c";
		if ( i == 3 ) s = "d";
		if ( i == 4 ) s = "e";
		if ( i == 5 ) s = "f";
		if ( i == 6 ) s = "g";
		if ( i == 7 ) s = "h";
		if ( i == 8 ) s = "i";
		if ( i == 9 ) s = "j";
		if ( i == 10 ) s = "k";
		if ( i == 11 ) s = "l";
		if ( i == 12 ) s = "m";
		if ( i == 13 ) s = "n";
		if ( i == 14 ) s = "o";
		if ( i == 15 ) s = "p";
		if ( i == 16 ) s = "q";
		if ( i == 17 ) s = "r";
		if ( i == 18 ) s = "s";
		if ( i == 19 ) s = "t";
		if ( i == 20 ) s = "u";
		if ( i == 21 ) s = "v";
		if ( i == 22 ) s = "w";
		if ( i == 23 ) s = "x";
		if ( i == 24 ) s = "y";
		if ( i == 25 ) s = "z";
		if ( i == 26 ) s = "A";
		if ( i == 27 ) s = "B";
		if ( i == 28 ) s = "C";
		if ( i == 29 ) s = "D";
		if ( i == 30 ) s = "E";
		if ( i == 31 ) s = "F";
		if ( i == 32 ) s = "G";
		if ( i == 33 ) s = "H";
		if ( i == 34 ) s = "I";
		if ( i == 35 ) s = "J";
		if ( i == 36 ) s = "K";
		if ( i == 37 ) s = "L";
		if ( i == 38 ) s = "M";
		if ( i == 39 ) s = "N";
		if ( i == 40 ) s = "O";
		if ( i == 41 ) s = "P";
		if ( i == 42 ) s = "Q";
		if ( i == 43 ) s = "R";
		if ( i == 44 ) s = "S";
		if ( i == 45 ) s = "T";
		if ( i == 46 ) s = "U";
		if ( i == 47 ) s = "V";
		if ( i == 48 ) s = "W";
		if ( i == 49 ) s = "X";
		if ( i == 50 ) s = "Y";
		if ( i == 51 ) s = "Z";
		return s;
	}
	def getOriginalString(txt : Int[]) : String = {
		var c : Int;
		var out : String;
		c = 0;
		out="";
		while (c < txt.length){
			out=out+this.getChar(txt[c]);
			c=c+1;
		}
		return out;
	}
	def getEncryptedString(txt : Int[]) : String = {
		var c : Int;
		var out : String;
		var n : Int;
		c = 0;
		out="";
		n=0;
		while (c < txt.length){ 
			n=offset*txt[c];
			while (51<n){
				n=n-52;
			}
			out=out+this.getChar(n);
			c=c+1;
		}
		return out;
	}
	def print() : String = {
		var s : String;
		s = "Encryption:";
		println(s);
		s = "in: "+this.getOriginalString(txtIn);
		println(s);
		s = "out: "+this.getEncryptedString(txtIn);
		println(s);
		return " :) ";
		
	}
}