object Maze {
  def main() : Unit = {
    /* prints a maze of size 20x20 */
    println(new MazeArray().init(20).printMaze());
  }
}

class MazeArray {
  var size : Int;
  var prng : PseudoRandomNumberGenerator;
  var walls : Int[];
  var wallCount : Int;
  var vertOffset : Int;
  var wallIDs : Int[];
  var cells : Int[];
  var cellCount : Int;
  
  var pipeChar : String;
  var horzChar : String;
  var plusChar : String;
  var t1Char : String;
  var t2Char : String;
  var t3Char : String;
  var t4Char : String;
  var l1Char : String;
  var l2Char : String;
  var l3Char : String;
  var l4Char : String;
  var i1Char : String;
  var i2Char : String;
  var i3Char : String;
  var i4Char : String;
  var arrBdyChar : String;
  var arrTipChar : String;

  def init(sze : Int) : MazeArray = {
    var i : Int;
    var dummy : Bool;

    // Drawing characters. I use Unicode, but you may want to replace that by
    // something else if it breaks (note that scala.io.Source handles it just
    // fine). Use the alternatives otherwise.
    pipeChar = "┃";   // "|";
    horzChar = "━";   // "-";
    plusChar = "╋";   // "+";
    t1Char   = "┳";   // "+";
    t2Char   = "┫";   // "+";
    t3Char   = "┻";   // "+";
    t4Char   = "┣";   // "+";
    l1Char   = "┗";   // "+";
    l2Char   = "┏";   // "+";
    l3Char   = "┓";   // "+";
    l4Char   = "┛";   // "+";
    i1Char   = "╻";   // "+";
    i2Char   = "╸";   // "+";
    i3Char   = "╹";   // "+";
    i4Char   = "╺";   // "+";
    arrBdyChar = " "; // "=";
    arrTipChar = " "; // ">";

    size = sze;
    prng = new PseudoRandomNumberGenerator().init();

    wallCount = 2 * size * (size - 1);
    walls = new Int[wallCount];
    vertOffset = wallCount / 2;

    wallIDs = new Int[wallCount];

    cellCount = size * size;
    cells = new Int[cellCount];
    
    i = 0;
    while (i < wallCount) {
      walls[i] = 1;
      wallIDs[i] = i;
      i = i + 1;
    }

    i = 0;
    while (i < cellCount) {
      cells[i] = i;
      i = i + 1;
    }

    wallIDs = this.shuffleArray(wallIDs);

    i = 0;
    while(!this.allMerged()) {
      dummy = this.destroyIfNotConnected(wallIDs[i]);
      i = i + 1;
    }

    return this;
  }

  def cellRepresentative(cid : Int) : Int = {
    var ptr : Int;

    ptr = cid;

    while(!(cells[ptr] == ptr)) {
      ptr = cells[ptr];
    }

    cells[cid] = ptr; // speeds up further lookups.

    return ptr;
  }     

  def setRepresentative(cid : Int, repr : Int) : Bool = {
    cells[this.cellRepresentative(cid)] = this.cellRepresentative(repr);
    return true;
  }

  def allMerged() : Bool = {
    var i : Int;

    i = 1;
    while(i < cellCount && this.cellRepresentative(i) == this.cellRepresentative(0)) {
      i = i + 1;
    }

    return i == cellCount;
  }

  def cellID(row : Int, col : Int) : Int = { return (row * size) + col; }
  def cellRow(cid : Int) : Int = { return cid / size; }
  def cellCol(cid : Int) : Int = { return prng.mod(cid, size); }

  def wallID(horizontal : Bool, row : Int, col : Int) : Int = {
    var toRet : Int;

    if(horizontal) {
      toRet = row * size + col;
    } else {
      toRet = row * (size - 1) + col + vertOffset;
    }

    return toRet;
  }

  def wallRow(wid : Int) : Int = {
    var toRet : Int;
    if(wid < vertOffset) {
      toRet = wid / size;
    } else {
      toRet = (wid - vertOffset) / (size - 1);
    }
    return toRet;
  }

  def wallCol(wid : Int) : Int = {
    var toRet : Int;
    if(wid < vertOffset) {
      toRet = prng.mod(wid, size);
    } else {
      toRet = prng.mod(wid - vertOffset, size - 1);
    }
    return toRet;
  }

  def cellOneOfWall(wid : Int) : Int = { return this.cellID(this.wallRow(wid), this.wallCol(wid)); }

  def cellTwoOfWall(wid : Int) : Int = { 
    var toRet : Int;

    if(wid < vertOffset) {
      toRet = this.cellID(this.wallRow(wid) + 1, this.wallCol(wid));
    } else {
      toRet = this.cellID(this.wallRow(wid), this.wallCol(wid) + 1);
    }
    return toRet;
  }

  def destroyIfNotConnected(wid : Int) : Bool = {
    var c1 : Int;
    var c2 : Int;
    var dummy : Bool;

    c1 = this.cellOneOfWall(wid);
    c2 = this.cellTwoOfWall(wid);

    if(!(this.cellRepresentative(c1) == this.cellRepresentative(c2))) {
      walls[wid] = 0;
      dummy = this.setRepresentative(c1, c2);
    }

    return true;
  }

  def printMaze() : String = {
    var i : Int;
    var j : Int;
    var str : String;
    var c : String;
    var w1 : Bool;
    var w2 : Bool;
    var w3 : Bool;
    var w4 : Bool;

    i = 0;
    j = 0;
    str = "";
    c = "";
    w1 = false;
    w2 = false;
    w3 = false;
    w4 = false;

    // top row
    i = 0;
    str = "   " + l2Char;
    while (i < size) {
      str = str + horzChar + horzChar + horzChar;
      if(i == size - 1)
        str = str + l3Char;
      else if(0 < walls[this.wallID(false, 0, i)])
        str = str + t1Char;
      else
        str = str + horzChar;
      i = i + 1;
    }
    println(str);


    i = 0;
    str = "";
    c = "";

    while (i < size) {
      // the vertical walls
      if(i == 0)
        str = " " + arrBdyChar + arrBdyChar + arrBdyChar + arrTipChar + "  ";
      else
        str = "   " + pipeChar + "   ";
      j = 0;
      while (j < size - 1) {
        if (walls[this.wallID(false, i, j)] == 1)
          c = pipeChar;
        else
          c = " ";
        
        str = str + c;

        if(!(i == (size - 1) && j == (size - 2)))
            str = str + "   ";
        else
            str = str + "  " + arrBdyChar;
        j = j + 1;
      }

      if (!(i == (size - 1)))
        str = str + pipeChar;
      else
        str = str + arrBdyChar + arrBdyChar + arrTipChar;

      println(str);

      // the horizontal walls
      if(i < (size-1)) {
        str = "   ";
        if(walls[this.wallID(true, i, 0)] == 1) 
          str = str + t4Char;
        else
          str = str + pipeChar;

        j = 0;
        while (j < size) {
          if (walls[this.wallID(true, i, j)] == 1)
            c = horzChar + horzChar + horzChar;
          else
            c = "   ";

          str = str + c;

          if(j < size - 1) {
            w1 = (walls[this.wallID(false, i, j)] == 1);       // up
            w2 = (walls[this.wallID(true, i, j + 1)] == 1);    // right
            w3 = (walls[this.wallID(false, i + 1, j)] == 1);   // down
            w4 = (walls[this.wallID(true, i, j)] == 1);        // left

            if(w1 && w2 && w3 && w4) 
              str = str + plusChar;
            else if(w1 && w2 && w3 && !w4)
              str = str + t4Char;
            else if(w1 && w2 && !w3 && w4)
              str = str + t3Char;
            else if(w1 && !w2 && w3 && w4)
              str = str + t2Char;
            else if(!w1 && w2 && w3 && w4)
              str = str + t1Char;
            else if(w1 && !w2 && w3 && !w4)
              str = str + pipeChar;
            else if(!w1 && w2 && !w3 && w4)
              str = str + horzChar;
            else if(w1 && w2 && !w3 && !w4)
              str = str + l1Char;
            else if(!w1 && w2 && w3 && !w4)
              str = str + l2Char;
            else if(!w1 && !w2 && w3 && w4)
              str = str + l3Char;
            else if(w1 && !w2 && !w3 && w4)
              str = str + l4Char;
            else if(w1 && !w2 && !w3 && !w4)
              str = str + i3Char;
            else if(!w1 && w2 && !w3 && !w4)
              str = str + i4Char;
            else if(!w1 && !w2 && w3 && !w4)
              str = str + i1Char;
            else if(!w1 && !w2 && !w3 && w4)
              str = str + i2Char;
          } else if(walls[this.wallID(true, i, size - 1)] == 1)
            str = str + t2Char;
          else
            str = str + pipeChar;

          j = j + 1;
        }
        println(str);
      }

      i = i + 1;
    }

    // top row
    i = 0;
    str = "   " + l1Char;
    while (i < size) {
      str = str + horzChar + horzChar + horzChar;
      if(i == size - 1)
        str = str + l4Char;
      else if(0 < walls[this.wallID(false, size-1, i)])
        str = str + t3Char;
      else
        str = str + horzChar;
      i = i + 1;
    }
    println(str);

    return " ** Enjoy ! **";
  }

  // only works for arrays with positive numbers.
  def shuffleArray(arr : Int[]) : Int[] = {
    var newarr : Int[];
    var i : Int;
    var j : Int;

    newarr = new Int[arr.length];

    i = 0;
    while(i < newarr.length) {
      newarr[i] = 0 - 1;
      i = i + 1;
    }

    i = 0;
    while(i < arr.length) {
      j = prng.getInt(0, arr.length);
      while(!(newarr[j] == 0 - 1)) {
        j = j + 1;
        if (j == newarr.length)
          j = 0;
      }
      newarr[j] = arr[i];
      i = i + 1;
    }

    return newarr;
  }
}

class PseudoRandomNumberGenerator {
  var a : Int;
  var b : Int;

  def init() : PseudoRandomNumberGenerator = {
    a = 12345; // put whatever you like in here
    b = 67890; 
    return this;
  }

  def getInt(min : Int, max : Int) : Int = {
    var posInt : Int;

    posInt = this.nextInt();
    if(posInt < 0)
      posInt = 0 - posInt;

    return min + (this.mod(posInt, max - min));
  }

  def mod(i : Int, j : Int) : Int = { return i - (i / j * j); }

  def nextInt() : Int = {
    b = 36969 * ((b * 65536) / 65536) + (b / 65536);
    a = 18000 * ((a * 65536) / 65536) + (a / 65536);
    return (b * 65536) + a;
  }
}
