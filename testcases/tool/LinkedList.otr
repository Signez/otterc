object LinkedList {
	def main(): Unit = {
		println(new Test().test());
	}
}

class Test {	
	def test(): String = {
		var l1: List;
		var l2: List;
		l1 = new ListElement().init(1);
		l2 = new ListElement().init(2);
		l2 = l2.append(l1);
		l2 = l2.insertHead(3);
		l2 = l2.insertTail(4);
		return "Content : " + l2.print() + " - Size : " + l2.size() + " - Contains(3) ? " + this.contains(l2, 3) + " - Contains(5) ? " + this.contains(l2, 5);
	}
	
	def contains(l: List, i: Int): String = {
		var s: String;
		if(l.contains(i)) {
			s = "yes";
		}
		else {
			s = "no";
		}
		return s;
	}
}


/**
 *  This class represents an empty list
 */
class List {
	
	def size(): Int = {
		return 0;
	}
	
	def contains(i: Int): Bool = {
		return false;
	}
	
	def insertHead(i: Int): List = {
		return new ListElement().init(i);
	}

	def insertTail(i: Int): List = {
		return new ListElement().init(i).append(this);
	}

	def append(ys: List): List = {
		return ys;
	}

	def print(): String = {
		return "";
	}
}

/**
 *  This class represents an element of the list
 */
class ListElement extends List {
	var x: Int;
	var xs: List;
	
	def init(i_x: Int): List = {
		x = i_x;
		xs = new List();
		return this;
	}
	
	def size(): Int = {
		return 1 + xs.size();
	}

	def contains(i: Int): Bool = {
		return x == i || xs.contains(i);
	}
	
	def append(ys: List): List = {
		xs = xs.append(ys);
		return this;
	}

	def insertHead(i: Int): List = {
		return new ListElement().init(i).append(this);
	}
	
	def insertTail(i: Int): List = {
		xs = xs.insertTail(i);
		return this;
	}
	def print(): String = {
		return x + ", " + xs.print();
	}
}
