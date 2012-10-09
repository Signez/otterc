object Dijkstra {
    def main() : Unit = {
        println(new D().start(0));
    }
}


class D {
	var V : Int; //the vertices (actually, the number of vertices)
	var E : Int[]; //the edges (E[i*V+j] = d => there is a path from i to j of length d)
	var s : Int; //the starting vertice
	var in : Int; //used for infinite
				  //(I wrote my code without realising that the negative integers weren't allowed)
	
	def init(start : Int) : Int  = {
		V = 7;
		E = new Int[V*V];
		in = 9999; //"to infinity and beyond", or in this case just up to 9999 ...
		
		//This is a pretty simple directed graph to test the algorithm ...
		E[0] = 0; E[1] = 1; E[2] = 1; E[3] = in; E[4] = 7; E[5] =in; E[6] =in;
		E[7] =in; E[8] = 0; E[9] =in; E[10]=in; E[11]= 2; E[12]=in; E[13]=in;
		E[14]=in; E[15]= 3; E[16]= 0; E[17]=in; E[18]=in; E[19]=in; E[20]=in;
		E[21]= 3; E[22]=in; E[23]=in; E[24]= 0; E[25]= 7; E[26]= 1; E[27]=in;
		E[28]=in; E[29]=in; E[30]=in; E[31]=in; E[32]= 0; E[33]=in; E[34]= 4;
		E[35]=in; E[36]=in; E[37]=in; E[38]=in; E[39]=in; E[40]= 0; E[41]=in;
		E[42]=in; E[43]=in; E[44]=in; E[45]=in; E[46]=in; E[47]= 5; E[48]= 0;
		s = start;
		return 0;
	}
	
	//get the length of the path from i to j (infinite => no path)
	def getV(i : Int, j : Int) : Int = {
		return E[i*V+j];
	}
	
	def changeV(i : Int, j : Int, num : Int) : Int = {
		E[i*V+j] = num;
		return 0;
	}
	
	def start(start : Int) : Int = {
		var L : Int[]; //the results i.e. the distance from the starting vertice s to all the other vertices in the graph
		
		//a bunch of variables used in the algorithm...
		var i : Int;
		var T : Int[];
		var v : Int;
		var w : Int;
		var d : Int;
		var aux : Int;
		//initialisation : 
		
		//init graph
		aux = this.init(start);
		
		//init L 
		L = new Int[V];
		i=0;
		while (i < V) {
			L[i] = in;
			i=i+1;
		}
		
		//init L[s] & v
		L[s]=0;
		v = s;
		
		//init T
		T = new Int[V*V];
		i=0;
		while (i < V*V) {
			T[i] = 0;
			i = i+1;
		}
		
		
		//let's start the Dijkstra Algorithm 
		while (!(v == in) && !(L[v] == in)) {
			T[v] = 1;
			w = 0;
			while (w < V) {
				if (!(w == s) && T[w] == 0) {
					if (!(this.getV(v,w) == in)) {
						d = L[v] + this.getV(v,w);
						if (d < L[w]) {
							L[w] = d;
							//println("v=" + v + " w=" + w + " L[w]=" + L[w]);
						}
					}
				}
				w = w+1;
			}
			
			v = in;
			i=0;
			while (i<V) {
				if (T[i] == 0) {
					if (v == in || L[i] < L[v]) {
						v = i;
					}
				}
				i = i+1;
			}
		}
		
		//print results
		i=0;
		println("");
		println("===== Results ====="); 
		println("Distance from vertice " + s + " to vertice :"); 
		println("(9999 is infinity i.e. no possible path)");
		while (i<V) {
			println("   " + i + "  =>  L = " + L[i]);
			i=i+1;
		}
		println("===================");
		println("");
		return 0;
	}
}
