object IntVector {
    def main() : Unit = {
        println(new TestVector().exec());
    }
}

class TestVector {
	def exec() : String = {
    	var a : Vector;
        var b : Vector;
        var d : Vector;
        var t : Vector;
        var dim : Int;
        var c : Int;
        c = 7;
        dim = 5;
        a = new Vector();
        b = new Vector();
        d = new Vector();
        t = new Vector();
        
       	println("");
		println(" --------------- ");
		println(" Testing Vectors ");
		println(" --------------- ");
		println("");
		
		println(" Initializing vectors ");
		a = a.init(dim);
		b = b.init(dim);
		d = d.zero(dim);
        println(" A = " + a.toString());
        println(" B = " + b.toString());
        println(" D = " + d.toString());
        println("");
        
        println(" Testing substraction, addition and multiplication ");
        println(" A = A - B = " + a.substract(b).toString());
        println(" A = A + B = " + a.addition(b).toString());
        println(" c = " + c);
        println(" A = A * c = " + a.multiply(c).toString());
        println(" A = A / c = " + a.divide(c).toString());
        println("");
        
        println(" Testing scalar product ");
        println(" A.scalar(B) = " + a.scalarProduct(b));
        println(" D.scalar(B) = " + d.scalarProduct(b));
        println("");
        
        println(" Testing copy and copy methods ");
        t = t.zero(dim + 5);
        println(" T = " + t.toString());
        println(" T = T.cp(A + B) = " + t.copy(a.addition(b)).toString());
        t = d.clone();
        println(" T = D.clone() = " + t.toString());
        println("");
        
        return " Tests finished";
    }
}

class Vector {
	var dim : Int;
	var array : Int[];
	var DEFAULT_DIM : Int;
	
	def init(size : Int) : Vector = {
		var p : Int;
		var junk : Vector;
		junk = this.initDef();
		p = 0;
		dim = size;
		array = new Int[dim];
		while(p < dim){
			array[p] = p;
			p = p + 1;
		}
		return this;
	}
	
	def initDef() : Vector = {
		DEFAULT_DIM = 3;
		return this;
	}
	
	def scalarProduct(v : Vector) : Int = {
		var ret : Int;
		var p : Int;
		ret = 0;
		if(this.hasSameDim(v)) {
			p = 0;
			while(p < dim){
				ret = ret + (array[p] * v.getArray(p));
				p = p + 1;
			}
		} else {
			println("Error: vectors don't have the same size. Returning zero.");
		}
		return ret;
	}
	
	def multiply(c : Int) : Vector = {
		var p : Int;
		p = 0;
		while(p < dim){
			array[p] = c * array[p];
			p = p + 1;
		}
		return this;
	}
	
	def divide(c : Int) : Vector = {
		//return this.multiply(1/c);
		var p : Int;
		p = 0;
		while(p < dim){
			array[p] = array[p] / c;
			p = p + 1;
		}
		return this;
	}
	
	def addition(v : Vector) : Vector = {
		var p : Int;
		if(this.hasSameDim(v)) {
			p = 0;
			while(p < dim){
				array[p] = array[p] + v.getArray(p);
				p = p + 1;
			}
		} else {
			println("Error: vectors don't have the same size.");
		}
		return this;
	}
	
	def substract(v : Vector) : Vector = {
		var p : Int;
		if(this.hasSameDim(v)) {
			p = 0;
			while(p < dim){
				array[p] = array[p] - v.getArray(p);
				p = p + 1;
			}
		} else {
			println("Error: vectors don't have the same size.");
		}
		return this;
	}
	
	def hasSameDim(v : Vector) : Bool = {
		return (dim == v.getDim());
	}
	
	def isNullVector() : Bool = {
		var p : Int;
		var isNull : Bool;
		isNull = true;
		p = 0;
		while(p < dim && isNull){
			if(!(array[p] == 0)){
				if (isNull) {
					isNull = false ;
				}
			}
			p = p + 1;
		}
		return isNull;
	}
	/*
	def vectorialProduct(v : Vector) : Vector = {
		return v;
	}
	
	def deter(a1:Int, b1:Int, a2:Int, b2:Int) = {
		
	}
	*/
	def zero(size : Int) : Vector = {
		var p : Int;
		var junk : Vector;
		junk = this.initDef();
		p = 0;
		dim = size;
		array = new Int[dim];
		while(p < dim){
			array[p] = 0;
			p = p + 1;
		}
		return this;
	}
	
	def toString() : String = { // check dim < 1
		var ret : String;
		var p : Int;
		if (this.isNullVector()) {
			ret = "Vector null.";
		} else {
			ret = "Vector(" + array[0];
			p = 1;
			while(p < dim){
				ret = ret + ", " + array[p];
				p = p + 1;
			}
			ret = ret + ")";
		}
		return ret + " -- (dim = " + dim + ")";
	}
	/*
	def setArray(p : Int, e : Int) : Vector = {
		array[p] = e;
		return this;
	}
	*/
	def getArray(e : Int) : Int = {
		return array[e];
	}
	
	def getDim() : Int = {
		return dim;
	}
	
	def copy(v : Vector) : Vector = {
		var p : Int;
		dim = v.getDim();
		array = new Int[dim];
		p = 0;
		while(p < dim){
			array[p] = v.getArray(p);
			p = p + 1;
		}
		return this;
	}
	/*
	def projection(v : Vector) : Vector = {
		var vU : Vector;
		vU = v.unitary();
		return vU.multiply(this.scalarProduct(vU));
	}
	
	def unitary() : Vector = {
		return this.divide(this.norme());
	}
	
	def norme() : Int = {
		var sq : Int;
		var p : Int;
		p = 0;
		sq = 0;
		while(p < dim) {
			sq = sq + array[p] * array[p];
			p = p + 1;
		}
		return this.sqrt(sq);
	}
	
	def sqrt(s : Int) : Int = {
		return s;
	}
	*/
	def clone() : Vector = {
		var ret : Vector;
		ret = new Vector();
		ret = ret.copy(this);
		return ret;
	}
}