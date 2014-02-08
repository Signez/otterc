object HeapSort {
	def main() : Unit = {
		/* Sort the following Heap:
		             5
		        4        1
		    3      2  
		*/       
		println(new HP().Init().Sort().toString());
	}
}

class HP {
	var numbers : Int[];

	def Init() : HP = {
		numbers = new Int[5];
		numbers[0] = 1;
		numbers[1] = 3;
		numbers[2] = 4;
		numbers[3] = 5;
		numbers[4] = 2;
		println("Before : ");
		println(this.toString());
		println("After : ");
		return this;	
	}

	def Sort(): HP = {
		var i : Int;
		var tmp : Int;
		var j : Int;
		i = 0;
		while (i < numbers.length) {
			j = numbers.length;
			while (1 < j) {
				tmp = this.SortHeap(j - 1);
				j = j - 1;
			}
			i = i + 1;	
		}
		return this;
	}

	def toString(): String = {
		var i : Int;
		var str : String;
		i = 0;
		str = "";
		while (i < numbers.length) {
			str = str + numbers[i] + " ";
			i = i + 1;
		}
		return str;
	}

	def SortHeap(upperbound : Int) : Int = {
		var i : Int;
		var o : Int;
		var leftChild : Int;
		var rightChild : Int;
		var mChild : Int;
		var root : Int;
		var temp : Int;

		root = (upperbound - 1) / 2;
		o = root;
		while (0 < o || o == 0) {
			i = root;
			while (0 < i || 0 == i) {
				leftChild = (2*i)+1;
				rightChild = (2*i)+2;

				if (((leftChild < upperbound) || (leftChild == upperbound)) && ((rightChild < upperbound)) || (rightChild == upperbound)) {
					if ((numbers[leftChild] < numbers[rightChild]) || (numbers[leftChild] == numbers[rightChild])) {
						mChild = rightChild;
					}
					else {
						mChild = leftChild;
					}
				}
				else {
					if (upperbound < rightChild) {
						mChild = leftChild;
					}
					else {
						mChild = rightChild;
					}
				}
				
				if (numbers[i] < numbers[mChild]) {
					temp = numbers[i];
					numbers[i] = numbers[mChild];
					numbers[mChild] = temp;
				}
				i = i - 1;
			}
			o = o - 1;
		}
		temp = numbers[0];
		numbers[0] = numbers[upperbound];
		numbers[upperbound] = temp;
		return 0;
	}
}

