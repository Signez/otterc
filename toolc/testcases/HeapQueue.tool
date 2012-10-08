object HeapQueue {
	def main() : Unit = {
		println(new HQ().demo());
	}
}

class Void{}//used to simule void methods

class HQ {
	var eltsMaxSize : Int;
	var elts : Int[];
	var eltsCount : Int;
	var void : Void;
	
	def init(initElts: Int[]): Void = {
		var iterator : Int;
		
		eltsMaxSize = 256;
		elts = new Int[eltsMaxSize];
		eltsCount = initElts.length;
		void = new Void();
		
		if(eltsMaxSize < initElts.length){
			println("Warnig: elements list too large. Last elements will be ommited");
			eltsCount = 256;
		}
		
		iterator = 0;
		while(iterator < eltsCount){
			elts[iterator] = initElts[iterator];
			iterator = iterator + 1;
		}
		iterator = 0;
		
		void = this.makeHeap();
		
		return void;
	}
	
	def makeHeap(): Void = {
		var siftIndex : Int;
		
		siftIndex = 0;
		while (siftIndex < eltsCount){
			void = this.siftUp(0, siftIndex);
			siftIndex = siftIndex+1;
		}
		return void;
	}
	
	def switchPos(indexA : Int, indexB : Int): Void = {
		var temp : Int;
		
		temp = elts[indexA];
		elts[indexA] = elts[indexB];
		elts[indexB] = temp;
		
		return void;
	}
	
	def siftUp(upBound : Int, lowBound : Int) : Void = {
		var parent : Int;
		var finished : Bool;
		
		parent = (lowBound-1)/2;
		finished = parent < upBound;
		while ( !finished){
			if(elts[parent]<elts[lowBound]){
				void = this.switchPos(parent, lowBound);
				lowBound = parent;
			}else{
				finished = true;
			}
			parent = (lowBound-1)/2;
			finished = parent < upBound || finished;
		}
		return void;
	}
	
	def siftDown(upBound : Int, lowBound : Int) : Void = {
		var child1 : Int;
		var child2 : Int;
		var finished : Bool;
		
		child1 = upBound * 2 + 1;
		child2 = upBound * 2 + 2;
		finished = lowBound < child1;
		while (!finished){
			if(elts[upBound] < elts[child1]){
				if(elts[child1] < elts[child2]){
					void = this.switchPos(upBound, child2);
					upBound = child2;
				}else{
					void = this.switchPos(upBound, child1);
					upBound = child1;
				}
			}else if (elts[upBound] < elts[child2]){
				void = this.switchPos(upBound, child2);
				upBound = child2;
			}else{
				finished = true;
			}
			
			child1 = upBound * 2 + 1;
			child2 = upBound * 2 + 2;
			finished = finished || lowBound < child1;
		}
		return void;
	}
	
	def addElt(element: Int) : Void = {
		if(eltsCount<eltsMaxSize){
			elts[eltsCount] = element;
			void = this.siftUp(0,eltsCount);
			eltsCount = eltsCount + 1;
		}else{
			println("Warning: Too many elements. Remove before adding new element.");
		}
		return void;
	}
	
	def remElt() : Int = {
		var removedElt : Int;
		
		if(0<eltsCount){
			removedElt = elts[0];
			eltsCount = eltsCount - 1;
			elts[0] = elts[eltsCount];
			void = this.siftDown(0, eltsCount-1);
		}else{
			println("Warning: Trying to remove element from empty list. removedElt will be 0.");
			removedElt = 0;
		}
		
		return removedElt;
	}
	
	def demo() : String = {
		var initialElts : Int[];
		var i: Int;
		var demoSize : Int;
		
		demoSize = 20;
		
		println("");
		println("Welcome to the HeapQueue demo. This class simulate elemets of different priorities by integers stored in a heap. The class can add new elements(integers) of different priority to the heap and remove the higest element form the heap.");
		println("");
		println("");
		println("*****************");
		println("* Demo launched *");
		println("*****************");
		
		initialElts = new Int[demoSize];
		i = 0;
		while (i < demoSize){
			initialElts[i] = this.modulo(i*(i+demoSize), demoSize*3/4-1);
			i = i + 1;
		}
		
		println("");
		println("Generated array:");
		println(this.arrayToString(initialElts, 0, demoSize));
		
		void = this.init(initialElts);
		
		println("");
		println("Heap list:");
		println(this.arrayToString(elts, 0, eltsCount));
		
		println("");
		println("Removing first element:");
		println("Removed element: " + this.remElt());
		println("New heap:");
		println(this.arrayToString(elts, 0, eltsCount));
		
		println("");
		println("Removing 4 next elements:");
		i = 0;
		while(i<4){
			println("Removed element " + (i+1) + ": " + this.remElt());
			i = i+1;
		}
		println("New heap:");
		println(this.arrayToString(elts, 0, eltsCount));
		
		println("");
		println("Adding 1 element of low priority: 0");
		void = this.addElt(0);
		println("New heap:");
		println(this.arrayToString(elts, 0, eltsCount));
		
		println("");
		println("Adding 1 element of medium priority: "+ elts[0]/2);
		void = this.addElt(elts[0]/2);
		println("New heap:");
		println(this.arrayToString(elts, 0, eltsCount));
		
		println("");
		println("Adding 1 element of high priority: "+ (elts[0]+1));
		void = this.addElt(elts[0]+1);
		println("New heap:");
		println(this.arrayToString(elts, 0, eltsCount));
		
		println("");
		println("Removing all elements in decreasing order");
		i=1;
		while(0<eltsCount){
			println("Removed element " + i + ": " + this.remElt());
			i = i+1;
		}
		println("Printing Heap(empty): "+this.arrayToString(elts, 0, eltsCount));
		
		return "";
	}
	
	def modulo(value : Int, module: Int) : Int = {
		return value-module*(value/module);
	}
	def arrayToString(array : Int[], from : Int, to : Int) : String = {
		var msg : String;
		
		msg = "[";
		if(array.length<to){
			println("Warning: trying to print more than array size. Less elements will be printed.");
			to = array.length;
		}
		while (from<to-1){
			msg = msg + array[from] + ", ";
			from = from+1;
		}
		if(0<to){
			msg = msg + array[to-1];
		}
		msg = msg + "]";
		return msg;
	}
}