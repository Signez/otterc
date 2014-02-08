object Diamond {
	def main(): Unit = {
		println( new MyDiamond().init(13).drawDiamond());
	}
}
class MyDiamond {
	var max : Int;
	
	/* This is a dub commentary */
	def init (i : Int) : MyDiamond = {
		max = i;
		return this;
	}
	
	def drawDown(level : Int) : String = {
		var aux : Int;
		var myLine : String;
		var myLevel : Int;	
		aux = level;
		myLine = "";
		
		
		if (level < 1) {println("");}
		else 	{
			while (0<aux){
				myLine=myLine+"*";
				aux=aux-1;
				}
			println(myLine);
			myLevel=level - 1;
			myLine=this.drawDown(myLevel);
			}
		return "";
	}
	def drawUp(level : Int, maxLevel : Int) : String = {
		var aux : Int;
		var myLine : String;
		var myLevel : Int;
	
		aux = level;
		myLine = "";
		
		
		if (level < (maxLevel+2)) {
			while (0<aux){
				myLine=myLine+"*";
				aux=aux-1;
				}
			println(myLine);
			myLevel=level + 1;
			myLine=this.drawUp(myLevel, maxLevel);
			}
		return "";
	}
	def drawDiamond(): String = {
		var s : String;
		s=this.drawUp(0,max);
		s=this.drawDown(max);
		return s;

	}

}