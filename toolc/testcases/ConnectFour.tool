// ConnectFour (Puissance 4) by La Spada Luca

object ConnectFour {
    def main() : Unit = {
        println(new Game().Start(6, 7));
    }
}

// Represents the grid of the game
//
// How I count the grid's game
// 14 15 16 17 18 19 20
//  7  8  9 10 11 12 13
//  0  1  2  3  4  5  6
class Grid {

	var grid : Int[]; // 0 = clear, 1 = player1 , 2 = player 2
	
	var nbColumns : Int;
	var nbLines : Int;
	
	var win : Bool;	
	var connect : Int;
	var useless : Int;
	var freeSpace : Int;	//I use it to count the number of freespace remaning, is usefull to now
							//If the game finish in a draw (Cannot test the functionnality)
	
	var aux : Auxiliary;
	var lastPlayedDisc : Int;

	def Init(l : Int, c : Int) : Grid = {
		
		nbColumns = c;
		nbLines = l;
		
		win = false;
		connect = 4;
		
		freeSpace = nbColumns * nbLines;
		
		grid = new Int[nbColumns*nbLines];
		aux = new Auxiliary();
	
		return this;
	}

	//Return true if it can add a disc, and false otherwise
	def addDisc(idPlayer : Int, nbColumnsPlayed : Int) : Bool = {
		var foundSpace : Bool;
		var shiftLine : Int; //Which line I am testing

	    shiftLine = 0;
		foundSpace = false;
		
		if(((nbColumnsPlayed < nbColumns)) && !(nbColumnsPlayed < 0)) {
			while(shiftLine < nbColumns*nbLines && !(foundSpace)) {
				if(grid[nbColumnsPlayed + shiftLine] == 0) {
					grid[nbColumnsPlayed + shiftLine] = idPlayer;
					foundSpace = true;
					freeSpace = freeSpace - 1;
					lastPlayedDisc = nbColumnsPlayed + shiftLine;
				}
				shiftLine = shiftLine + nbColumns; //Check nextnbLines
			}
		}
		return foundSpace;
	}
	
	def displayGrid(namePlayer : String) : Int = {
		var i : Int;
		var j : Int;
		var lineString : String;
		
		i = (nbColumns-1) * nbLines -1;
		j = 0;
		lineString = "";
		
		println("-----------------------");
		println("Display Grid : ("+namePlayer+" has played)");
		
		while(!(i < 0)) {
			while(j < nbColumns) {
				lineString = lineString + "  " + grid[i+j];
				j = j + 1;
			}
			println(lineString);
			
			j = 0;
			i = i - nbColumns;
			lineString = "";
		}
		
		return 0;
	}
	
	def getFreeSpace() : Int = {
		return freeSpace;
	}
	
	def getNbColumns() : Int = {
		return nbColumns;
	}
	
	def checkWin(idPlayer : Int) : Bool =
	{	
		win = false;
		
		useless = this.checkHorizontal(idPlayer);
		useless = this.checkVertical(idPlayer);
		useless = this.checkDiagonal(idPlayer);
		useless = this.checkAntiDiagonal(idPlayer);
		
		return win;
	}

	def checkHorizontal(idPlayer : Int) : Int = {
		var count : Int;
		var i : Int;
		
		var nbLinesPlayed : Int;
		nbLinesPlayed = lastPlayedDisc / nbLines;
		
		//Write straight -1 is not accepted, but the operation 0 minus 1 is okey by the compilator
		//I count from -1 because the lastPlayedDisc is count 2 times.
		count = 0-1;		
		
		//Right from the last disc played (The first test is for the bound)
		i = 0;
		while((lastPlayedDisc+i < nbColumns*nbLines) && grid[lastPlayedDisc+i] == idPlayer && ((lastPlayedDisc + i) / nbLines) == nbLinesPlayed) {
			count = count + 1;
			i = i + 1;
		}
		
		//Left from the last disc played (The first test is for the bound)
		i = 0;
		while(!(lastPlayedDisc-i < 0) && grid[lastPlayedDisc-i] == idPlayer && ((lastPlayedDisc - i) / nbLines) == nbLinesPlayed) {
			count = count + 1;
			i = i + 1;
		}
		
		if (!(count < connect)) {
			win = true;
		}
	
		return 0;
	}
	
	def checkVertical(idPlayer : Int) : Int =	{
		var count : Int;
		var i : Int;
		
		//Write straight -1 is not accepted, but the operation 0 minus 1 is okey by the compilator
		//I count from -1 because the lastPlayedDisc is count 2 times.
		count = 0-1;
		
		//Upper from the last disc played
		i = 0;
		while((lastPlayedDisc+i < nbColumns*nbLines) && grid[lastPlayedDisc+i] == idPlayer) {
			count = count + 1;
			i = i + nbColumns;
		}
		
		//Down from the last disc played
		i = 0;
		while(!(lastPlayedDisc-i < 0) && grid[lastPlayedDisc-i] == idPlayer) {
			count = count + 1;
			i = i + nbColumns;
		}
		
		if (!(count < connect)) {
			win = true;
		}

		return 0;
	}
	
	def checkDiagonal(idPlayer : Int) : Int =	{
		var count : Int;
		var i : Int;
		var j : Int;
		
		var nbColumnsPlayed : Int;
		nbColumnsPlayed = aux.mod(lastPlayedDisc, nbColumns);
		
		//Write straight -1 is not accepted, but the operation 0 minus 1 is okey by the compilator
		//I count from -1 because the lastPlayedDisc is count 2 times.
		count = 0-1;
		
		//Upper from the last disc played
		i = 0;
		j = 0;
		while((lastPlayedDisc+i < nbColumns*nbLines) && grid[lastPlayedDisc+i] == idPlayer && aux.mod(lastPlayedDisc+i, nbColumns) == nbColumnsPlayed+j) {
			count = count + 1;
			i = i + nbColumns + 1;
			j = j + 1;
		}
		
		//Down from the last disc played
		i = 0;
		j = 0;
		while(!(lastPlayedDisc-i < 0) && grid[lastPlayedDisc-i] == idPlayer && aux.mod(lastPlayedDisc-i, nbColumns) == nbColumnsPlayed-j) {
			count = count + 1;
			i = i + nbColumns + 1;
			j = j + 1;
		}
		
		if (!(count < connect)) {
			win = true;
		}

		return 0;
	}
	
	def checkAntiDiagonal(idPlayer : Int) : Int =	{
		var count : Int;
		var i : Int;
		var j : Int;
		
		var nbColumnsPlayed : Int;
		nbColumnsPlayed = aux.mod(lastPlayedDisc, nbColumns);
		
		
		//Write straight -1 is not accepted, but the operation 0 minus 1 is okey by the compilator
		//I count from -1 because the lastPlayedDisc is count 2 times.
		count = 0-1;
		
		//Upper from the last disc played
		i = 0;
		j = 0;
		while((lastPlayedDisc+i < nbColumns*nbLines) && grid[lastPlayedDisc+i] == idPlayer  && aux.mod(lastPlayedDisc+i, nbColumns) == nbColumnsPlayed-j) {

			count = count + 1;
			i = i + nbColumns - 1;
			j = j + 1;

		}
		
		//Down from the last disc played
		i = 0;
		j = 0;
		while(!(lastPlayedDisc-i < 0) && grid[lastPlayedDisc-i] == idPlayer && aux.mod(lastPlayedDisc-i, nbColumns) == nbColumnsPlayed+j) {
			count = count + 1;
			i = i + nbColumns - 1;
			j = j + 1;
		}
		
		if (!(count < connect)) {
			win = true;
		}

		return 0;
	}
	
}

// Class representing the game
class Game {

	var grid : Grid;
	var player1 : Player;
	var player2 : Player;
	var currentPlayer : Player;
	var useless : Int;
	var prng : Auxiliary;
	
	
	def Init(nbL : Int, nbC : Int) : Int = {
		grid = new Grid().Init(nbL, nbC);
	
		prng = new Auxiliary().init();
	
		player1 = new Player().Init("Player 1", 1);
		player2 = new Player().Init("Player 2", 2);
		currentPlayer = player1;
		
		return 0;
	}

	def Start(nbLi : Int, nbCo : Int) : String = {
		var win : Bool;
		var draw : Bool;
		useless = this.Init(nbLi, nbCo);
		
		win = false;		
		draw = false;
		
		println("Welcome to the ConnectFour Game !");
		
		while(!win && !draw) {	
			if(grid.addDisc(currentPlayer.getId(), prng.getInt(0, grid.getNbColumns()))) {
				useless = grid.displayGrid(currentPlayer.getName());
				if(grid.checkWin(currentPlayer.getId())) {
					win = true;
				}
				else if (grid.getFreeSpace() == 0) {
					draw = true;
				}
				else if(currentPlayer.getId() == player1.getId()) {
					currentPlayer = player2;
				}
				else {
					currentPlayer = player1;
				}
			}
		}

		if(win) {
			println("The winner is the player : " + currentPlayer.getName());
		}
		else if(draw) {
			println("There is no winner ! It's a draw !");
		}
		
		return "Game Over";
	}
}

//Class representing a Player
//The id field represents the number that is used to fill the grid.
//It need to be a one unique number non 0
class Player {
	var name : String;
	var id : Int;
	
	def Init(nm : String, iden : Int) : Player = {
		name = nm;
		id = iden;
		
		return this;
	}
	
	def getId() : Int = {
		return id;
	}
	
	def getName() : String = {
		return name;	
	}
}

//Auxiliary class used for mod et generate an aleatory number
class Auxiliary {
  var a : Int;
  var b : Int;

  def init() : Auxiliary = {
    a = 35252; // put whatever you like in here
    b = 24321; 
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