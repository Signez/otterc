object TimeConverter {
	
	def main(): Unit = {
		println(new Converter().init(948923).getConverted());
	}
}

class Converter {
	
	var days: Int;
	var hours: Int;
	var minutes: Int;
	var seconds: Int;
	
	def init(s: Int): Converter = {
		seconds = s;
		return this;
	}
	
	def compute(): Bool = {
		var done: Bool;
		done = false;
		if(0 < seconds || 0 == seconds) {
			days = seconds/(3600*24);
			seconds = seconds - days*3600*24;
			hours = seconds/3600;
			seconds = seconds - hours*3600;
			minutes = seconds/60;
			seconds = seconds - minutes*60;
			done = true;
		}
		return done;
	}
	
	
	def getConverted(): String = {
		var b: Bool;
		var converted: String;
		
		b = this.compute();
		
		if(b) {
			converted = days + " days, " + hours + " hours, " + minutes + " minutes et " + seconds + " secondes";
		}
		else{
			converted = "Erreur";
		}
		return converted;
	}
}