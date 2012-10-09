object PineTree {
    def main() : Unit = {
        println(new PT().draw());
    }
}


class PT {
	var height : Int; //the number of "tier" of the pine tree
	var trunk : Int; //the height of the trunk
	var base : Int; //the length of the base
	
	def init(h : Int) : Int = {
		height = h;
		base = 2*height + 9;
		trunk = height;
		return 0;
	}
	
	def draw() : Int = {
		var h : Int;
		var s : Int;
		var n : Int;
		var space : Int;
		var ligne : String;
		var aux : Int;
		
		
		aux = this.init(5);
		//print the tree
		h=0;
		while (h<height) {
			s=0;
			while (s<h+2) {
				ligne = "";
				n=0;
				space=base-(2*s+1);
				while (n<base) {
					if (n < base - space/2 && space/2-1 < n) {
						ligne = ligne+"*";
					} else {
						ligne = ligne+" ";
					}
					n = n+1;
				}
				println(ligne);
				s = s+1;
			}
			h = h+1;
		}
		
		//print the trunk
		s=0;
		while (s<trunk) {
			n=0;
			ligne = "";
			while (n<base) {
			  if ((base-3)/2-1 < n && n < base-(base-3)/2) {
			    ligne = ligne+"|";
			  } else {
			    ligne = ligne+ " ";
			  }
			  n = n+1;
			}
			println(ligne);
			s = s+1;
		}

		//print the base
		n=0;
		ligne = "";
		while (n<base) {
		  ligne = ligne + "_";
		  n = n+1;
		}
		println(ligne);
		println("");
		return 0;
	}
}
