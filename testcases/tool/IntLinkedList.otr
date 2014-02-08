object IntLinkedListMain {
  def main() : Unit = {
    println(new IntLinkedList().test());
  }
}

class IntLinkedList {
  var isNotEmpty: Bool;
  var cur: Int;
  var next: IntLinkedList;

  def test(): Bool = {
    var last: IntLinkedList;
    last = this.setCur(0).setNext(1).setNext(2).setNext(42);
    return this.contains(42);
  }

  def contains(value: Int): Bool = {
    var ret: Bool;
    if (this.isEmpty()) {
      ret = false;
    } else {
      if (value == cur) {
        ret = true;
      } else {
        ret = next.contains(value);
      }
    }
    return ret;
  }

  def isEmpty(): Bool = {
    return !isNotEmpty;
  }

  def cur(): Int = {
    return cur;
  }

  def next(): IntLinkedList = {
    return next;
  }

  def setCur(value: Int): IntLinkedList = {
    cur = value;
    next = new IntLinkedList();
    isNotEmpty = true;
    return this;
  }

  def setNext(value: Int): IntLinkedList = {
    next = next.setCur(value);
    return next;
  }
}
