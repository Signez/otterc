// A simulator of the card game : BlackJack.

object App {
	
	def main() : Unit = {
		println("Finished. " + new Game().init(13, 4, 8, 200, 18, 15).start());
	}
	
}


// Class representing a deck of cards.
class Deck {
	
	var NB_VALUES : Int;
	
	var content : Int[];  // Array of cards.
	var size : Int;       // Size of the deck (max. number of cards it can contain).
	var nb_in : Int;      // Number of cards currently in the deck.
	var pos : Int;        // Position of the next card to be picked.
	
	
	def init(s : Int, val : Int) : Deck = {
		NB_VALUES = val;
		
		content = new Int[s];
		size = s;
		nb_in = 0;
		pos = 0;
		
		return this;
	}
	
	// Adds a card into the deck unless it is full.
	def add(c : Int) : Int = {
		var r : Int;
		r = 1;
		
		if(nb_in < size && this.is_valid(c)) {
			content[this.mod(pos+nb_in, size)] = c;
			nb_in = nb_in + 1;
			r = 0;
		}
		
		return r;
	}
	
	// Picks a card from the deck and returns it unless it is empty.
	def pick() : Int = {
		var r : Int;
		r = 0 - 1;
		
		if(!(nb_in < 1)) {
			r = content[pos];
			pos = this.mod(pos + 1, size); 
			nb_in = nb_in - 1;
		}
		
		return r;
	}
	
	// Shuffles the deck of cards.
	def shuffle(seed : Int, nb_swaps : Int) : Int = {
		var r : Int;
		var rPrev : Int;
		var r0 : Int;
		var a : Int;
		var c : Int;
		
		var i : Int;
		var temp : Int;
		
		a = 53;
		r0 = 11;
		
		// Bad pseudo-random numbers generator (but it doesn't really matter here for this example).
		i = 0;
		r = r0;
		while(i < nb_swaps) {
			c = seed + i/53;
			
			rPrev = r;
			r = this.mod(a*rPrev + c, size);
			
			temp = content[r];
			content[r] = content[rPrev];
			content[rPrev] = temp;
			
			i = i+1;
		}
		
		return 0;
	}
	
	// Returns the current number of cards in the deck.
	def getNbCards() : Int = {
		return nb_in;
	}
	
	// Tests whether the deck of cards is empty.
	def is_empty() : Bool = {
		return (nb_in == 0);
	}
	
	// Tests whether the given value is valid for a card.
	def is_valid(c : Int) : Bool = {
		return (!(c < 2) && (c < NB_VALUES+2));
	}
	
	// Modulus.
	def mod(x : Int, m : Int) : Int = {
		var res : Int;
		res = 0;
		
		if(!(m == 0)) {
			res = (x - (x/m)*m);
		}
		
		return res;
	}
	
}

// Class representing a player : a player is simply characterized by his cards.
class Player {
	
	var NB_VALUES : Int;
	var MAX_SCORE : Int;
	var ACE_VALUE : Int;
	
	var name : String;
	var nameLength : Int;
	var sex : Int;
	
	var cards : Int[];   // Array of cards.
	var nb_in : Int;     // Number of cards the player currently owns.
	var minToStay : Int; // Minimum score to "stay".
	var score : Int;     // Total value of the player's cards.
	
	def init(n : String, l : Int, s : Int, val : Int, strategy : Int) : Player = {
		NB_VALUES = val;
		MAX_SCORE = 21;
		ACE_VALUE = 11;
		
		name = n;
		nameLength = l;
		sex = s;
		
		cards = new Int[MAX_SCORE+1];
		nb_in = 0;
		
		minToStay = strategy;
		score = 0;
		
		return this;
	}
	
	// Adds a card into the deck unless it is full.
	def getCard(c : Int, print : Bool) : Int = {
		var useless : Int;
		var r : Int;
		r = 0 - 1;
		
		if(nb_in < cards.length && this.is_valid(c)) {
			cards[nb_in] = c;
			nb_in = nb_in + 1;
			r = 0;
		}
		
		score = this.computeScore(); // The player's score gets updated.
		
		if(print) {
			println(name + " asked for a card.");
			useless = this.print(false);
			useless = this.printState();
		}
		
		return r;
	}
	
	// Computes and returns the current score of the player.
	def computeScore() : Int = {
		var currScore : Int;
		var nbAces : Int;
		var c : Int;
		var i : Int;
		
		currScore = 0;
		nbAces = 0;
		
		i = 0;
		while(i < nb_in) {
			c = cards[i];
			if(this.is_ace(c))
				nbAces = nbAces + 1;
			else
				currScore = currScore + this.getValue(c);
			i = i+1;
		}
		
		i = 0;
		while(i < nbAces) {
			if(currScore+ACE_VALUE < MAX_SCORE+1)
				currScore = currScore + ACE_VALUE;
			else
				currScore = currScore + 1;
			i = i+1;
		}
		
		return currScore;
	}
	
	// Returns the "score value" of a card.
	def getValue(c : Int) : Int = {
		var val : Int;
		val = c;
		
		if(!(c < 10)) val = 10;
		
		return val;
	}
	
	// Picks a card from the deck and returns it unless it is empty.
	def pick() : Int = {
		var r : Int;
		r = 0 - 1;
		
		if(!(nb_in < 1)) {
			nb_in = nb_in - 1;
			r = cards[nb_in];
		}
		
		return r;
	}
	
	// Returns true if the player's score is at least equal to his minimum wanted score.
	def isStaying() : Bool = {
		return !(score < minToStay);
	}
	
	def isOut() : Bool = {
		return !(score < MAX_SCORE+1);
	}
	
	// Tests whether the given value is valid for a card.
	def is_valid(c : Int) : Bool = {
		return (!(c < 2) && (c < NB_VALUES+2));
	}
	
	def is_ace(c : Int) : Bool = {
		return (c == 14);
	}
	
	def getName() : String = {
		return name;
	}
	
	def getScore() : Int = {
		return score;
	}
	
	def print(showHidden : Bool) : Int = {
		var prefix : String;
		var top : String;
		var bot : String;
		var side : String;
		var values : String;
		var c : Int;
		var i : Int;
		
		prefix = "   ";
		i = 0;
		while(i < nameLength) {
			prefix = prefix + " ";
			i = i+1;
		}
		
		top = prefix;
		bot = prefix;
		side = prefix;
		values = name + "  >";
		
		i = 0;
		while(i < nb_in) {
			c = cards[i];
			if(c == 10 || !(c < 15)) {
				top = top + "  .----.";
				side = side + "  |    |";
				bot = bot + "  `----'";
			}
			else {
				top = top + "  .---.";
				side = side + "  |   |";
				bot = bot + "  `---'";
			}
			values = values + "  | " + this.cardToString(c) + " |";
			i = i+1;
		}
		
		if(showHidden) {
			top = top + "  .---.";
			side = side + "  |   |";
			bot = bot + "  `---'";
			values = values + "  | ? |";
		}
		
		println(top);
		println(side);
		println(values);
		println(side);
		println(bot);
		println(prefix + "  Score : " + score);
		println("");
		
		return 0;
	}
	
	def printState() : Int = {
		var s : String;
		
		if(this.isOut()) {
			println(name + " went over " + MAX_SCORE + " and lost !");
			println("");
		}
		else if(nb_in == 2 && score == MAX_SCORE) {
			println(name + " got the BLACKJACK !!!");
			println("");
		}
		else if(this.isStaying()) {
			if(sex == 0)
				s = "his";
			else if(sex == 1)
				s = "her";
			else
				s = "its";
			println(name + " is now staying with " + s + " score.");
			println("");
		}
		return 0;
	}
	
	def cardToString(value : Int) : String = {
		var print : String;
		print = "";
		
		if(value == 11) { print = "J"; }
		else if(value == 12) { print = "Q"; }
		else if(value == 13) { print = "K"; }
		else if(value == 14) { print = "A"; }
		else print = ""+value;
		
		return print;
	}
	
}


// Main class of the game simulator.
class Game {
	
	var NB_VALUES : Int;
	var NB_COLORS : Int;
	var SEED : Int;
	var NB_SWAPS : Int;
	
	var BANK_STRATEGY : Int;
	var P1_STRATEGY : Int;
	var P2_STRATEGY : Int;
	
	var nb_cards : Int;
	var useless : Int;
	
	def init(v : Int, c : Int, s : Int, swaps : Int, p1Strat : Int, p2Strat : Int) : Game = {
		NB_VALUES = v;
		NB_COLORS = c;
		SEED = s;
		NB_SWAPS = swaps;
		
		BANK_STRATEGY = 17;
		P1_STRATEGY = p1Strat;
		P2_STRATEGY = p2Strat;
		
		nb_cards = NB_VALUES*NB_COLORS;
		
		return this;
	}
	
	def start() : Int = {
		var cards : Deck;
		var bank : Player;
		var p1 : Player;
		var p2 : Player;
		
		bank = new Player().init("Bank", 4, 3, NB_VALUES, BANK_STRATEGY);
		p1 = new Player().init("Bernard", 7, 0, NB_VALUES, P1_STRATEGY);
		p2 = new Player().init("Sabine", 6, 1, NB_VALUES, P2_STRATEGY);
		
		cards = this.createDeck();
		useless = cards.shuffle(SEED, NB_SWAPS);
		
		// The bank receives one card.
		println("A card is given to the Bank.");
		useless = bank.getCard(cards.pick(), false);
		useless = bank.print(true);
		
		// Two cards are given to each player.
		useless = this.getFirstCards(p1, cards);
		useless = this.getFirstCards(p2, cards);
		
		while(!p1.isStaying() || !p2.isStaying()) {
			if(!p1.isStaying()) {
				useless = p1.getCard(cards.pick(), true);
			}
			if(!p2.isStaying()) {
				useless = p2.getCard(cards.pick(), true);
			}
		}
		
		println("Bank returns its hidden card.");
		useless = bank.getCard(cards.pick(), false);
		useless = bank.print(false);
		useless = bank.printState();
		while(!bank.isStaying()) {
			useless = bank.getCard(cards.pick(), true);
		}
		
		println("");
		println(")----###########");
		println(")----# RESULTS #");
		println(")----###########");
		println("");
		
		useless = this.printWinner(p1, bank) + 2*this.printWinner(p2, bank);
		println("");
		
		return useless; // Not that useless :)
	}
	
	def getFirstCards(p : Player, cards : Deck) : Int = {
		println(p.getName() + " received two cards.");
		useless = p.getCard(cards.pick(), false);
		useless = p.getCard(cards.pick(), false);
		useless = p.print(false);
		useless = p.printState();
		return 0;
	}
	
	def createDeck() : Deck = {
		var cards : Deck;
		var i : Int;
		var j: Int;
		
		cards = new Deck().init(nb_cards, NB_VALUES);
		
		i = 0;
		while(i < NB_COLORS) {  // Could be done using a mod, but this version allows us to test nested loops.
			j = 0;
			while(j < NB_VALUES) {
				useless = cards.add(j+2);
				j = j+1;
			}
			i = i+1;
		}
		
		return cards;
	}
	
	def printWinner(p : Player, bank : Player) : Int = {
		var winner : Int;
		winner = 0;
		
		if(!p.isOut() && (bank.isOut() || !(p.getScore() < bank.getScore()+1))) {
			winner = 1;
			println(p.getName() + " beats the bank and wins !");
		}
		else
			println(p.getName() + " pathetically lost versus the bank.");
			
		return winner;
	}
	
}