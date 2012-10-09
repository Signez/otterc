object BubbleSort {
  def main(): Unit = {
    println(new BS().apply());
  }
}

class BS {
  var list: Int[];
  var size: Int;
  var swapped: Bool;

  def initialize(): Int = {
    size = 10;
    list = new Int[size];
    list[0] = 12;
    list[1] = 4;
    list[2] = 3;
    list[3] = 1;
    list[4] = 9;
    list[5] = 99;
    list[6] = 10;
    list[7] = 2;
    list[8] = 20;
    list[9] = 32;
    return 0;
  }

  def apply(): String = {
    var o: Int;
    o = this.initialize();
    swapped = true;
    while (swapped == true) {
      o = this.sort(); 
    }
    return this.concat();
  }

  def sort(): Int = {
    var i: Int;
    var k: Int;
    i  = 0;
    k = 0;
    swapped = false;
    while (i < size-1) {
      if(list[i+1] < list[i]) {
        swapped = true;
        k = list[i];
        list[i] = list[i+1];
	list[i+1] = k;
      }
      i = i+1;
    }
    return 0;
  }

  def concat(): String = {
    var i: Int;
    var sorted: String;
    i = 0;
    sorted = "";
    while(i < size) {
      sorted = sorted+list[i]+",";
      i = i+1;
    }
    return sorted;
  }
}
