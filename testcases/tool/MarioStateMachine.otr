/**
 * This program will only represent Mario current status, like if he's tall (Super Mario) 
 * or if he took a flower (Fire Mario).
 * This won't emulate Mario levels, pipes, Goombas nor Peach. 
 *
 * Its main purpose is to test OOP support with inheritance and overriding (through the
 * Item classes).
 
 * Author: Stanislas Signoud <stanislas.signoud@epfl.ch>
 * Date: 02.10.2012
 */

object MarioStateMachine {
	def main () : Unit = {
		println(new Mario().init());
	}
}

// Item "Inventory": all our item classes
class Item {
	var name : String;
	def getNextState() : Int = {
		assert(false); // We shouldn't go through there anyway ("abstracting that class")
		name = "Nothing";
		return 0;
	}
	
	def getName(): String = {
		return name;
	}
}

class Mushroom extends Item {
	def getNextState() : Int = {
		name = "Mushroom";
		return 1;
	}	
}

class Flower extends Item {
	def getNextState() : Int = {
		name = "Flower";
		return 2;
	}
}

class Mario {
	var state : Int; // 0 = simple Mario, 1 = Super Mario, 2 = Fire Mario, -1 = Game Over
	
	def init() : String = {
		var message : String;
		// Main "scenario"
		
		println("[START!] Let's-a-goooooo !");
		
		println(this.takeItem(new Mushroom()));
		assert(state == 1);
		println(this.takeItem(new Flower()));
		assert(state == 2);
		println(this.takeDamage());
		assert(state == 1);
		println(this.takeDamage());
		assert(state == 0);
		println(this.takeItem(new Flower()));
		assert(state == 2);
		println(this.takeItem(new Mushroom()));
		assert(state == 2);
		println(this.takeDamage());
		assert(state == 1);
		println(this.takeDamage());
		assert(state == 0);
		println(this.takeDamage());
		assert(state == 0-1);
		println(this.takeItem(new Mushroom()));
		assert(state == 1);
		
		if(0 - 1 < state) { 
			message = "[ END! ] " + this.humanizeState(state) + " took the flag and put it down like a boss.";
		} else {
			message = "[ END! ] Game over.";
		}
		
		return message;
	}
	
	def takeItem(item: Item): String = {
		var previousState : Int;
		var newState : Int;
		var message : String;
		
		previousState = state;
		 
		newState = item.getNextState();
		
		if(previousState < newState) {
			state = newState;
			
			if(previousState < 0 && 0 < state) {
				state = 0; // You always return in the game as simple Mario.
				message = "[ ITEM ] " + this.humanizeState(previousState) + " took a " + item.getName() 
				        + " and is back in business as " + this.humanizeState(state) + "."; 
			} else {
				message = "[ ITEM ] " + this.humanizeState(previousState) + " took a " + item.getName()
						+ " and is now " + this.humanizeState(state) + " !";
			}
		} else {
			message = "[ ITEM ] " + this.humanizeState(previousState) + " took a " + item.getName() + " and that didn't do anything to him.";
		}
		
		return message;
	}
	
	def takeDamage(): String = {
		var status : String;
		 
		if (state < 0) {
			status = "[DAMAGE] Mario is still dead.";
		} if (state < 1) {
			state = 0 - 1;
			
			status = "[DAMAGE] Ouch! Mario is now dead. :(";
		} else {
			if(2 < state || state == 2) {
				state = 1;
			} else {
				state = state - 1;
			}
			
			status = "[DAMAGE] Ouch! He take damages and is now just " + this.humanizeState(state) + ".";
		}
		
		return status;
	}
	
	def humanizeState(state: Int) : String = {
		var humanized : String;
		
		assert(2 < state);
		
		if(state == 0) {
			humanized = "Mario";
		} else if (state == 1) {
			humanized = "Super Mario";
		} else if (state == 2) {
			humanized = "Fire Mario";
		} else if (state < 0) {
			humanized = "Dead";
		} else {
			humanized = "Impossible Mario";
		}
		
		return humanized;
	}
}