object IntBuffer {
    def main() : Unit = {
        println(new TestBuffer().testBuffer());        
    }
}


class TestBuffer {
	def testBuffer() : String = {
		var buff : Buffer;
		buff = new Buffer().init(3);
		
		println("");
		println(" --------------- ");
		println(" Testing Buffers ");
		println(" --------------- ");
		println("");
		
		println(" Initializing a buffer with one element ");
		buff = buff.put(50);
		println(" " + buff.toString());
		println("");
		
		println(" Removing only element ");
		println(" " + buff.remove());
		println(" " + buff.toString());
		println("");
		
		println(" Removing from empty buffer ");
		println(" " + buff.remove());
		println(" " + buff.toString());
		println("");
		
		println(" Adding 10, 20 and 30 to buffer ");
		buff = buff.put(10).put(20).put(30);
		println(" " + buff.toString());
		println("");
		
		println(" Adding too much elements (buffer's limit exceeded) ");
		buff = buff.put(50);
		println(" " + buff.toString());
		println("");
		return " Tests finished";
	}
}
class Buffer {
	var size : Int;
	var buffer : Int[];
	var nbOfElem : Int;
	var inPointer : Int;
	var outPointer : Int;
	var DEFAULT_SIZE : Int;
	
	
	def initDef() : Buffer = {
		DEFAULT_SIZE = 20;
		return this;
	}
	
	def init(sizeGiven : Int) : Buffer = {
		var junk : Buffer;
		junk = this.initDef();
		if (sizeGiven < 1) {
			size = DEFAULT_SIZE;
		} else {
			size = sizeGiven;
		}
		buffer = new Int[size];
		nbOfElem = 0;
		inPointer = 0;
		outPointer = 0;
		return this;
	}
	
	def put(elem : Int) : Buffer = {
		if (this.isFull()) {
			println(" Error: buffer is full, unable to add the element.");
		} else {
			buffer[inPointer] = elem;
			inPointer = this.mod((inPointer + 1), size);
			nbOfElem = nbOfElem + 1;
		}
		return this;
	}
	
	def remove() : Int = {
		var ret : Int;
		if (this.isEmpty()) {
			println(" Error: buffer is empty, returning zero.");
			ret = 0;
		} else {
			ret = buffer[outPointer];
			outPointer = this.mod((outPointer + 1), size);
			nbOfElem = nbOfElem - 1;
		}
		return ret;
	}
	
	def isEmpty() : Bool = {return (nbOfElem == 0);}
	
	def isFull() : Bool = {return (nbOfElem == size);}
	
	def mod(x : Int, y : Int) : Int = {return (x - (y * (x / y)));}
	
	def toString() : String = {
		var tempPointer : Int;
		var ret : String;
		if (this.isEmpty()) {
			ret = "Buffer is empty.";
		} else {
			tempPointer = outPointer;
			ret = "Buffer[" + buffer[tempPointer];
			tempPointer = this.mod((tempPointer + 1), size);
			while(!(tempPointer == inPointer)) {
				ret = ret + ", " + buffer[tempPointer];
				tempPointer = this.mod((tempPointer + 1), size);
			}
			ret = ret + "]";
		}
		return ret + " -- [" + nbOfElem + "/" + size + "]";
	}
}