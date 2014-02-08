object Stack {
    def main() : Unit = {
        println(new StackC().init().push(2).push(3).push(4).pop().pop().push(5).pop().pop().print());
    }
}

class StackC {
    var data : Int[];
    var head : Int;
    var MAX_SIZE : Int;

    def push(element : Int) : StackC = {
        println("push "+element);
        data[head] = element;
        head = head + 1;
        return this;
    }

    def size() : Int = {
        return (head - 1);
    }

    def pop() : StackC = {
        var i:Int;
        i = data[head-1];
        println("pop "+data[head-1]);
        head = head -1;
        return this;
    }

    def init(): StackC = {
        MAX_SIZE = 300;
        data = new Int[MAX_SIZE];
        head = 0;
        return this;
    }

    def print(): Int = {
      var i:Int;
      i = 0;
      while (i < head) {
        println(" - "+data[i]);
      }
      return 1;
    }
}
