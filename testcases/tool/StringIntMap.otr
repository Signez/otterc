object StringIntMapMain {
  def main() : Unit = {
    println(new StringIntMap().test());
  }
}

class StringIntMap {
  var isNotEmpty: Bool;
  var key: String;
  var value: Int;
  var next: StringIntMap;

  def test(): Int = {
    var last: StringIntMap;
    last = this.set("foo", 1).set("bar", 2).set("duck", 42);
    println(last.get("duck", 1337));
    println(last.get("unavailable", 1337));
    return 0;
  }

  def get(key_: String, default: Int): Int = {
    var ret: Int;
    if (this.isEmpty()) {
      ret = default;
    } else if (key == key_) {
      ret = value;
    } else {
      ret = next.get(key_, default);
    }
    return ret;
  }

  def set(key: String, value: Int): StringIntMap = {
    return new StringIntMap().setNext(this).setCurrent(key, value);
  }


  def setCurrent(key_: String, value_: Int): StringIntMap = {
    key = key_;
    value = value_;
    isNotEmpty = true;
    return this;
  }

  def setNext(next_: StringIntMap): StringIntMap = {
    next = next_;
    return this;
  }

  def isEmpty(): Bool = {
    return !isNotEmpty;
  }
}
