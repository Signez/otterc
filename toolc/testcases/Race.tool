object Race {
	def main() : Unit = {
		println(new Starter().init());
	}
}

class Starter {
	def init():String = {
		var useless: Int;
		var r: RaceEvent;
		var j: Journalist;
		var c0: Participant; 
		var c10: Participant;
		var c11: Participant;
		var c2: Participant;
		var c3: Participant;
		
		r = new RaceEvent();
		j = new PeopleJournalist();
		c0 = new Participant();
		c10 = new Professional();
		c11 = new Participant();
		c2 = new Professional();
		c3 = new Participant();
		
		useless = r.init(2500);
		useless = c0.init("Hugo", 7, "Yahoo!", "Gosh!");
		useless = c10.init("Max", 9, "Yeah!", "@%F&!");
		useless = c11.init("Willi", 6, "Great!", "Mmh...");
		useless = c2.init("Therese", 10, "Hoho!", "Grrr...");
		useless = c3.init("Luka", 12, "Magnifico!", "AAAH!");
		useless = j.init("Marie-France");
		useless = r.init(100);
		
		useless = r.addBike(c0);
		useless = r.addTandem(c10, c11);
		useless = r.addBike(c2);
		useless = r.addBike(c3);
		useless = r.setJournalist(j);
		
		println("");
		useless = r.makeRace();
		useless = r.showResults();
		useless = r.interview();
		
		return "";
	}
}

class RaceEvent {
	var useless : Int;
	var start: Bike;
	var end: Bike;
	var distance: Int;
	var journalist: Journalist;

	def init(d: Int):Int = {
		var e: Participant;
		var s: Participant;
		e = new Participant();
		s = new Participant();
		useless = e.isEnd();
		useless = s.isStart();
		distance = d;
		start = new Bike();
		end = new Bike();
		useless = end.setRider(e);
		useless = start.setRider(s);
		useless = start.setNext(end);
		useless = end.setPrev(start);
		
		return 0;
	}
	
	def setJournalist(j : Journalist):Int = {
		journalist = j;
		return 0;
	}
	
	def addBike(p : Participant):Int = {
		var b:Bike;
		b = new Bike();
		useless = b.setRider(p);
		useless = b.setPrev(end.getPrev());
		useless = end.getPrev().setNext(b);
		useless = b.setNext(end);
		useless = end.setPrev(b);
		return 0;
	}
	
	def addTandem(p1 : Participant, p2 : Participant):Int = {
		var b:Tandem;
		b = new Tandem();
		useless = b.setRider(p1);
		useless = b.setRider(p2);
		useless = b.setPrev(end.getPrev());
		useless = end.getPrev().setNext(b);
		useless = b.setNext(end);
		useless = end.setPrev(b);
		return 0;
	}
	
	// Bubble sort on a list
	def makeRace():Int = {
		var switched : Bool;
		println("3...");
		println("2...");
		println("1...");
		println("GO!!!");
		println("");
		println("___d^o___d^o_d^o__________");
		println("");

		switched = true;
		
		while (switched) {
			switched = start.getNext().orderBySpeed();
		}

		println("All participants have arrived!");
		println("");
		
		return start.getNext().wins();
	}
	
	def showResults():Int = {
		println("============================================");
		println("                 RESULTS"                    );
		println("============================================");
		useless = start.getNext().showResults(1);
		println("============================================");
		return 0;
	}
	
	def interview():Int = {
		useless = journalist.interview(start.getNext().getRider());
		useless = journalist.interview(end.getPrev().getRider());
		return 0;
	}
}

class Bike {
	var useless : Int;
	var next : Bike;
	var prev : Bike;
	var rider : Participant;
	var ridersCount : Int;
	
	def getNames():String = {
		return rider.getName();
	}
	
	def getNext():Bike = { 
		return next;
	}
	
	def getPrev():Bike = {
		return prev;
	}
	
	def setNext(n: Bike):Int = {
		next = n;
		return 0;
	}
	
	def setPrev(p: Bike):Int = {
		prev = p;
		return 0;
	}
	
	def wins():Int = {
		return rider.wins();
	}
	
	def setRider(r: Participant):Int = {
		if (0 < ridersCount) {
			println("Bike already has a rider!");
		} else {
			rider = r;
			ridersCount = ridersCount + 1;
		}
		return 0;
	}
	
	def getRider():Participant = {
		return rider;
	}
	
	def getSpeed():Int = {
		var speed:Int;
		if (!(ridersCount == 1)) {
			println("Not enough rider for bike, not able to ride.");
			speed = 0;
		} else {
			speed = 15 + rider.getSpeed();
		}
		return speed;
	}
	
	def getTime(distance:Int):Int = {
		var t:Int;
		if (this.getSpeed() == 0) {
			t = 999;
		} else {
			t = distance / this.getSpeed();
		}
		return t;
	}
	
	def orderBySpeed() : Bool = {
		var before: Bike;
		var after: Bike;
		var switched : Bool;
		switched = false;
	
		if (!(next.getNames() == "end")) {
			if (!(this.getSpeed() < next.getSpeed())) {
				switched = next.orderBySpeed();
				
			} else {
				before = prev;
				after = next.getNext();
				prev = next;
				useless = next.setNext(this);
				useless = before.setNext(prev);
				useless = prev.setPrev(before);
				next = after;
				useless = after.setPrev(this);
				switched = this.orderBySpeed();
				switched = true;
			}
		}
		
		return switched;
	}
	
	def showResults(rank:Int) : Int = {
		println( rank + ". " + this.getNames());
		if (!(next.getNames() == "end")) {
			useless = next.showResults(rank + 1);
		}
		return 0;
	}
}

class Tandem extends Bike {
	var rider2 : Participant;
	
	def getNames():String = {
		return rider.getName() + " and " + rider2.getName();
	}
	
	def wins():Int = {
		useless = rider.wins();
		return rider2.wins();
	}
	
	def setRider(r: Participant): Int = {
		if (1 < ridersCount) {
			println("Tandem has already two riders!");
		} else {
			if (0 < ridersCount) {
				rider2 = r;
			} else {
				rider = r;
			}
			ridersCount = ridersCount + 1;
		}
		return 0;
	}
	
	def getSpeed():Int = {
		var speed:Int;
		if (!(ridersCount == 2)) {
			println("Not enough rider for bike, not able to ride.");
			speed = 0;
		} else {
			speed = 5 + rider.getSpeed() + rider2.getSpeed();
		}
		return speed;
	}
}

class Participant {
	var name : String;
	var power : Int;
	var hasWon : Bool;
	var winShout : String;
	var loseShout : String;

	def init(n:String, p:Int, ws:String, ls:String):Int = {
		if (n == "end" || n == "start") {
			n = "PetitMalin";
		}
		name = n;
		power = p;
		hasWon = false;
		winShout = ws;
		loseShout = ls;
		return 0;
	}
	
	def isEnd():Int = {
		name = "end";
		return 0;
	}
	
	def isStart():Int = {
		name = "start";
		return 0;
	}
	
	def wins():Int = {
		hasWon = true;
		return 0;
	}
	
	def getName():String = {
		return name;
	}
	
	def getSpeed():Int = {
		return power;
	}
	
	def answerRace():Int = {
		if (hasWon) {
			println(winShout + " I can't believe it!");
		} else {
			println(loseShout + " Participation is most important after all...");
		}
		return 0;
	}
	
	def answerWeekEnd():Int = {
		println("I was training the whole day long!");
		return 0;
	}
}

class Professional extends Participant {
	
	def getSpeed():Int = {
		return power + 5;
	}
	
	def answerRace():Int = {
		if (hasWon) {
			println(winShout + " I was confident and i did it!");
		} else {
			println(loseShout + " I had technical problem. Let's win next time.");
		}
		return 0;
	}
	
	def answerWeekEnd():Int = {
		println("I have nothing to say about that, please leave me alone.");
		return 0;
	}
}

class Journalist {
	var useless : Int;
	var name : String;
	
	def init(n:String):Int = {
		name = n;
		return 0;
	}
	
	def interview(p: Participant):Int = {
		useless = this.presentation(p);
		useless = this.askRace(p);
		return 0;
	}
	
	def presentation(p:Participant):Int = {
		println ("Hello " + p.getName()  + ", my name is " + name +
				 ". I have a few questions: ");
		return 0;
	}
	
	def askRace(p:Participant):Int = {
		println("How was the Race?");
		useless = p.answerRace();
		return 0;
	}
	
	def askWeekEnd(p:Participant):Int = {
		println("What did you do last week-end?");
		useless = p.answerWeekEnd();
		return 0;
	}
}

class PeopleJournalist extends Journalist {
	def interview(p: Participant):Int = {
		useless = this.presentation(p);
		useless = this.askRace(p);
		useless = this.askWeekEnd(p);
		return 0;
	}
}