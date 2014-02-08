object RabbitGen {
	def main(): Unit = {
		if(new RabbitRun().init()){}
	}
}

class RabbitRun{
	def init():Bool={
		var rabField:RabbitField;
		var dumBool:Bool;
		var dumRab:Rabbit;

		rabField=new RabbitField().initRabbitField(10,10);
		dumRab=rabField.addRabbit(0,0);
		dumRab=rabField.addRabbit(5,0);

		dumBool=rabField.print();
		return true;
	}
}

class Rabbit{
	/*
	(\___/)
	(='.'=)
	(")_(")
	*/
	/*
	0388821
	0457541
	0618061
	*/
	var posX:Int;
	var posY:Int;
	var height:Int;
	var width:Int;
	var design:Int[];

	def initRabbit(posX_:Int, posY_:Int):Rabbit={
		height=3;
		width=7;
		design=new Int[21];

		design[0]=0;
		design[1]=3;
		design[2]=8;
		design[3]=8;
		design[4]=8;
		design[5]=2;
		design[6]=1;
		design[7]=0;
		design[8]=4;
		design[9]=5;
		design[10]=7;
		design[11]=5;
		design[12]=4;
		design[13]=1;
		design[14]=0;
		design[15]=6;
		design[16]=1;
		design[17]=8;
		design[18]=0;
		design[19]=6;
		design[20]=1;

		posX=posX_;
		posY=posY_;
		return this;
	}

	def getWidth():Int={
		return width;
	}
	def getHeight():Int={
		return height;
	}
	def getDesign():Int[]={
		return design;
	}
}

class RabbitField extends Field{
	var myField:Field;

	var lBrice:String;
	var rBrice:String;
	var slash:String;
	var underScore:String;
	var rSlash:String;
	var equ:String;
	var simpleQuote:String;
	var doubleQuote:String;
	var dot:String;

	def initRabbitField(width_:Int,height_:Int):RabbitField={
		/*0*/ lBrice="(";
		/*1*/ rBrice=")";
		/*2*/ slash="/";
		/*3*/ rSlash="\";
		/*4*/ equ="=";
		/*5*/ simpleQuote="'";
		/*6*/ doubleQuote="~";
		/*7*/ dot=".";
		/*8*/ underScore="_";

		myField=new Field().initField(width_,height_);
		return this;
	}

	def print():Bool={
		var i:Int;
		var j:Int;
		var k:Int;

		var line:String;

		i=0;j=0;
		//dummyBool=myField.print();
		while(i<myField.getHeight()){
			j=0;
			line="";

			while(j<myField.getWidth()){
				k=myField.get(i,j);
				if(k==0){
					line=line+lBrice;
				}else if(k==1){
					line=line+rBrice;
				}else if(k==2){
					line=line+slash;
				}else if(k==3){
					line=line+rSlash;
				}else if(k==4){
					line=line+equ;
				}else if(k==5){
					line=line+simpleQuote;
				}else if(k==6){
					line=line+doubleQuote;
				}else if(k==7){
					line=line+dot;
				}else if(k==8){
					line=line+underScore;
				}else{
					line=line+" ";
				}
				j=j+1;
			}
			println(line);
			i=i+1;
		}
		return true;
	}

	def addRabbit(posX:Int,posY:Int):Rabbit={
		var rab:Rabbit;
		var dumBool:Bool;

		rab=new Rabbit().initRabbit(posX,posY);
		dumBool=myField.insertTable(posX,posY,rab.getWidth(),rab.getHeight(),rab.getDesign());

		return rab;
	}
}

class Field {
	var field:Table2D;
	var width:Int;
	var height:Int;

	def initField(width_:Int, height_:Int):Field={
		var i:Int;
		var j:Int;
		var dumBool:Bool;

		width=width_;
		height=height_;
		field=new Table2D().init(width,height);

		i=0;
		j=0;
		println("Created Field with sizes: "+width+" width and "+height+" height");
		while(i<height){
			j=0;
			while(j<width){
				dumBool=field.set(i,j,8);
				j=j+1;
			}
			i=i+1;
		}
		return this;
	}

	def print():Bool={
		var i:Int;
		var j:Int;
		var line:String;

		i=0;j=0;
		println("Printing Field with sizes: "+width+" width and "+height+" height");
		while(i<height){
			j=0;
			line="";

			while(j<width){
				line=line+field.get(i,j);
				j=j+1;
			}
			println(line);
			i=i+1;
		}
		return true;
	}

	def get(i:Int,j:Int):Int={
		return field.get(i,j);
	}
	def getWidth():Int={
		return width;
	}
	def getHeight():Int={
		return height;
	}
	def insertTable(posX:Int,posY:Int,subLineSize:Int,subNbLine:Int,values:Int[]):Bool={
		var res:Bool;
		res=field.insertTable(posX,posY,subLineSize,subNbLine,values);
		return res;
	}
}

class Table2D {
	var lineSize: Int;
	var nbLine: Int;
	var table: Int[];
	var i:Int;

	def init(lineSize_:Int, nbLine_:Int):Table2D={
		lineSize=lineSize_;
		nbLine=nbLine_;
		table = new Int[lineSize*nbLine-1];
		println("Created a new Int Table2D with sizes : "+lineSize+" lineSize"+" and "+nbLine+" nbLine");
		return this;
	}

	def insert(values:Int[], nbValues:Int):Bool={
		var res:Bool;

		res=true;
		if(nbValues==(lineSize*nbLine)){
			i=0;
			while(i<lineSize*nbLine-1){
				table[i]=values[i];
				i=i+1;
			}
		}else{
			println("You want to INSERT too/not enough many values : "+nbValues);
			res=false;
		}
		return res;
	}

	def get(line:Int, column:Int):Int={
		var res:Int;
		var index: Int;

		if(column<lineSize && line<nbLine){
			index=(lineSize-1)*line+column;
			res=table[index];
		}
		else{
			println("Hey you want to GET something too big ! "+line+" "+column);
			res=0;
		}
		return res;
	}

	def set(line:Int, column:Int, value:Int):Bool={
    var res:Bool;
    var index: Int;

		if(column<lineSize && line<nbLine){
      index=(lineSize-1)*line+column;
      table[index]=value;
			res=true;
    }
    else{
			println("Hey you want to SET something too big ! "+line+" "+column);
      res=false;
    }
    return res;
  }

	def insertTable(posX:Int,posY:Int,subLineSize:Int,subNbLine:Int,values:Int[]):Bool={
		var res:Bool;
		var i:Int;
		var j:Int;
		var dumBool:Bool;

		res=false;
		if(posX<lineSize-1 && !(posX<0) && posY<subNbLine-1 && !(posY<0)){
			i=0;
			while(i<subNbLine){
				j=0;
				while(j<subLineSize){
					dumBool=this.set(posX+i,posY+j,values[i*subNbLine+j]);
					j=j+1;
				}
				i=i+1;
			}
		}else{
			println("Hey you want to INSERT some strange SUBTABLE !");
		}
		return res;
	}
}
