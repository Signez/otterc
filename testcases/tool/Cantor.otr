object Cantor {
	def main(): Unit = {
		if(new Cant().init()){}
	}
}

class Cant{
	var horzChar : String;
	var mainLine : Int[];
	var mainSize : Int;

	/*
		This function remove an interval of charachters in the line (set this interval to space instead line)
	*/
	def removeInterval(line: Int[], from: Int, to: Int): Int[]={
		var i:Int;
		i=from-1;
		while(i<to){
			line[i]=0;
			i=i+1;
		}
		return line;
	}

	/*
		This function dissociate a line as Cantor set will do : it give back the two intervals where there will not be spaces
	*/
	def dissociate(lineStart: Int, lineEnd:Int): Int[]={
		var startRemove:Int;
		var endRemove:Int;
		/*The return is a,b,c and d, as we have : [a,b]U[c,d]*/		
		var answer:Int[];
		answer=new Int[4];

		startRemove=(lineStart+(lineEnd-lineStart)/3);
		endRemove=(lineEnd-(lineEnd-lineStart)/3);
		answer[0]=lineStart;
		answer[1]=startRemove;
		answer[2]=endRemove;
		answer[3]=lineEnd;

		return answer;
	}

	/*
		This function simply print a line of the Cantor set.
	*/
	def printLine(line :Int[], lineSize:Int):Bool={
		var i:Int;
		var res:String;
		i=0;
		res="";
		while(i<lineSize){
			if(line[i]==0){
				res=res+" ";
			}else{
				res=res+horzChar;
			}
			i=i+1;
		}
		println(res);
		return true;
	}

	/*
		As we can't have a String or char array, we have to make line as Int array.
		And we have to initalize each cell.
	*/
	def initLine(line:Int[], lineSize:Int): Bool={
		var i:Int;
		i=0;
		while(i<lineSize){
			line[i]=1;
			i=i+1;
		}
		return true;
	}

	/*
		This is the function which make all step from first to produce last line (but doesn't print the lines, because we won't see a fractal, but all steps of the calculation).
		The last argument is used to stop function at some point.
	*/
	def recursion(line:Int[], lineStart:Int, lineEnd:Int, print:Bool, stop:Int):Int[]={
		var A0:Int[];
		var A1:Int[];
		var tmp:Int[];
		var dummy:Bool;
		var i:Int;

		A0=new Int[lineEnd-lineStart];

		if(lineEnd-lineStart<2 || stop <1){
			A0=line;
		}else{
			A0=line;

			tmp=this.dissociate(lineStart, lineEnd);
			A1=this.removeInterval(A0, tmp[1]+1,tmp[2]);
			i=tmp[0];
			while(i<tmp[1]){
				mainLine[i]=A1[i];
				i=i+1;
			}
			A1=this.recursion(A1, tmp[0], tmp[1], false, stop-1);
			A1=this.recursion(A1, tmp[2], tmp[3], true, stop-1);
			A0=A1;
		}
		return A0;
	}
	
	def howManyLinesToPrint(lineSize:Int):Int={
		var i:Int;
		var tmp:Int[];

		i=1;
		tmp=this.dissociate(0,lineSize);
		while(!(tmp[1]-tmp[0]<2)){
			tmp=this.dissociate(tmp[0],tmp[1]);
			i=i+1;
		}
		return i;
	}

	def init(): Bool ={
		var dummy:Bool;
		var dummyTable:Int[];
		var nbLine:Int;
		var i:Int;
		i=0;
		nbLine=0;

		horzChar = "━";
		/*
			You just need to set your line size in the mainSize value
		*/
		mainSize=150;
		mainLine=new Int[mainSize];
		dummy=this.initLine(mainLine, mainSize);

		println("Here you can see a Cantor Set (for a first line size to "+mainSize+")");
		nbLine=this.howManyLinesToPrint(mainSize);
		println(nbLine+" lines");

		while(i<nbLine){
			dummy=this.initLine(mainLine, mainSize);
			dummyTable=this.recursion(mainLine, 0, mainSize, true, i);
			dummy=this.printLine(mainLine,mainSize);
			i=i+1;
		}
		return dummy;
	}
}
