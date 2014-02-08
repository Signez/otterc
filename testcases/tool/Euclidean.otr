/* Euclidean algorithm which takes 2 int and returns the GCD */
object Euclidean {
  def main(): Unit = {
    println(
      new EuclideanC().compute(42, 58) + ", " +
      new EuclideanC().compute(42, 59) + ", " +
      new EuclideanC().compute(2, 101) + ", " +
      new EuclideanC().compute(21, 49) + ", " +
      new EuclideanC().compute(87, 97) + ", " +
      new EuclideanC().compute(15, 98) + ", " +
      new EuclideanC().compute(2185, 2098) + ", " +
      new EuclideanC().compute(3000, 6000) +
      ""
    );
}
}

class EuclideanC {
  /* a = b ⨯ q + r */
  var a: Int;
  var b: Int;
  var q: Int;
  var r: Int;

  def compute(x: Int, y: Int): Int = {
    b = this.max(x, y);
    r = this.min(x, y);

    while (0 < r) {
      /* shift variables (going down) */
      a = b;
      b = r;

      q = a / b;
      r = a - (q*b);

      println(a +" = "+ b +" ⨯ "+ q +" + "+ r);
    }

    return b;
  }

  def min(a: Int, b: Int): Int = {
    var y: Int;

    if (a < b) {
      y = a;
    }
    else {
      y = b;
    }

    return y;
  }

  def max(a: Int, b: Int): Int = {
    var y: Int;

    if (a < b) {
      y = b;
    }
    else {
      y = a;
    }

    return y;
  }
}
