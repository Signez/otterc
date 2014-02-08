object IntArrayList {
	def main() : Unit = {
	    println((new TestArrayList()).test());
	  }
}

class TestArrayList {
	def test() : String = {
		var tmp : Bool;
		var list : ArrayList;
		
		list = new ArrayList();
		tmp = list.init();
		
		tmp = list.push(1);
	    tmp = list.push(2);
	    tmp = list.push(3);
	    
	    println("Starting poping list");
	    while( 0 < list.size() ) {
	    	println(list.get(0));
	    	tmp = list.remove(0);
	    }
	    println("List empty");
		
		return "";
	}
}

class ArrayList {
	var array : Int[];
	var size : Int;
	
	def init() : Bool = {
		array = new Int[1];
		size = 0;
		return true;
	}
	
	def push(value : Int) : Bool = {
		var newArray : Int[];
		var it : Int;
	
		if(size == (array.length)) {
			newArray = new Int[array.length*2];
			
			it = 0;
			while(it < (array.length)) {
				newArray[it] = array[it];
				it = it + 1;
			}
			
			array = newArray;
		}
		
		array[size] = value;
		size = size + 1;
		
		return true;
	}
	
	def pop() : Int = {
		var ret : Int;
		if(size < 1) {
			ret = (1-0);
		} else {
			ret = array[size-1];
			size = size - 1;
		}
		return ret;
	}
	
	def set(pos : Int, value : Int) : Bool = {
		var ret : Bool;
		if(pos < 0 || (size-1) < pos) {
			ret = false;
		} else {
			
			array[pos] = value;
		
			ret = true;
		}
		return ret;
	}
	
	def get(pos : Int) : Int = {
		var ret : Int;
		if(pos < 0 || (size-1) < pos) {
			ret = (0-1);
		} else {
			ret = array[pos];
		}
		return ret;
	}
	
	def remove(pos : Int) : Bool = {
		var ret : Bool;
		var it : Int;
		
		if(pos < 0 || (size-1) < pos) {
			ret = false;
		} else {
			it = pos;
			
			while(it < size-1) {
				array[it] = array[it+1];
				it = it + 1;
			}
			
			size = size - 1;
			ret = true;
		}
		
		return ret;
	}
	
	def size() : Int = {
		return size;
	}
	
	def isEmpty() : Bool = {
		return (0 < size);
	}
	
	def capacity() : Int = {
		return array.length;
	}
	
}