// Compiler Construction Course 2012
// Stephane Martin

/*
 Solves the N-Queens problem by exploring the
 solution space with a recursive algorithm. 
 The dimensions of the chessboard can be
 set in the run() method.
*/

object N_Queens {
  def main(): Unit = {
    if (new ProgramNQ().run()){}     
  }
}

class ProgramNQ {
  var size: Int;
  var succ: Bool;
  var nbrSolutions: Int;

  def run(): Bool = {   
    nbrSolutions = 0;


    // The dimension of the square chessboard: size x size. 
    // (do not use numbers too big if you want to be still alive 
    // when the program stops.

    size = 8;

    nbrSolutions = this.search();
    println("Total number of solutions: " + nbrSolutions);
    return true;
  }

  def search(): Int = {
    var solution: Int[];

    solution = this.initialSolution();
    succ = this.tryNextColumn(solution, 0);
    return nbrSolutions;
  }

  // Creates an empty array of ints of size `size'.
  // Solutions are encoded as arrays of ints,
  // such that `solution[i]' is the row of the
  // queen set on column i.
  // Empty columns are set at -1.
  def initialSolution(): Int[] = {
    var col: Int;
    var board: Int[];

    board = new Int[size];
    col = 0;
    while (col < size) {
      board[col] = 0-1;   
      col = col + 1;
    }
    return board;
  }

  def tryNextColumn(current: Int[], column: Int): Bool = {
    var row: Int;

    if (column == size) { // We have filled every column
      nbrSolutions = nbrSolutions + 1;
      println("Solution #" + nbrSolutions + ":");
      succ = this.printBoard(current);
      //succ = this.printSolution(current);
    } else { // We try every row for the next column
      row = 0;
      while (row < size) {
        if (this.possible(current, row, column)) {
          current[column] = row;
          succ = this.tryNextColumn(current, column+1);
        }
        row = row + 1;
      }
    }
    return true;
  }

  def possible(current: Int[], row: Int, column: Int): Bool = {
    return this.checkAgaintsRows(current, row, column) &&
           this.checkAgaintsDiag(current, row, column);
  }

  // Whether a queen set on some row of some column would be
  // threatened by a queen already set on the same row
  def checkAgaintsRows(current: Int[], row: Int, column: Int): Bool = {
    var col: Int;
    var ok: Bool;

    ok = true;
    col = 0;
    while (col < column && ok) {
      if (current[col] == row) {
        ok = false;
      }
      col = col + 1;
    }
    return ok;
  }

  // Whether a queen set on some row of some column would
  // be threatened along a diagonal by some queen already placed.
  def checkAgaintsDiag(current: Int[], row: Int, column: Int): Bool = {
    var col: Int;
    var ok: Bool;

    ok = true;
    col = 0;			
    while (col < column && ok) {
      if (column - col == current[col] - row) {
        ok = false;
      }
      if (column - col == row - current[col]) {
        ok = false;
      } 
      col = col + 1;
    }
    return ok;
  }

  def printBoard(board: Int[]): Bool = {
    var col: Int;
    var row: Int;
    var str: String;

    col = 0;
    row = 0;
    while (row < size) {
      col = 0;
      str = " ";
      while (col < size) {
        if (board[col] == row) {
          str = str + row + " ";
        } else {
          str = str + ". ";
        }
        col = col + 1;
      }
      row = row + 1;
      println(str);
    }
    println(" ");
    return true;
  }

  // Prints the content of a solution without formatting 
  // (Mostly useful for tests).
  def printSolution(solution: Int[]): Bool = {
    var i: Int;
    var str: String;
    
    str = "";
    i = 0;
    while (i < solution.length) {
      str = str + solution[i] + " ";
      i = i + 1;
    }
    println(str);
    return true;
  }

}

