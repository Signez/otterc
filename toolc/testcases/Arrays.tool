object Arrays
{
	def main() : Unit = {
		if(new Vector().run())
		{
			println("success");
		}
	}
}

/*
 * Describes a vector
 */
class Vector
{
	var data: Int[];
	
	def scal(i: Int): Vector =
	{		
		return this.set(data[0]*i,data[1]*i,data[2]*i);
	}
	
	def dot(other: Vector): Vector =
	{
		return this.set(data[0]*other.get(0),
					   data[1]*other.get(1),
					   data[2]*other.get(2));
	}
	
	def sum(other: Vector): Vector =
	{
		return this.set(data[0]+other.get(0),
					   data[1]+other.get(1),
					   data[2]+other.get(2));
	}
	
	def set(i1:Int, i2:Int, i3:Int): Vector =
	{
		data = new Int[3];
		data[0]=i1;
		data[1]=i2;
		data[2]=i3;
		return this;
	}
	
	def get(i:Int): Int =
	{
		var ret: Int;
		
		if(!(i < 0) && ( i < 4))
		{
			ret = data[i];
		}
		else
		{
			println("Index out of bounds");
			ret = 0; //default error
		}
		
		return ret;
	}
	
	def run() : Bool = 
	{
		var v1: Vector;
		var v2: Vector;
		var v3: Vector;
		
		v1= new Vector();
		v2= new Vector();
		
		v1= v1.set(1,1,1);
		v2= v2.set(0-1,0-2,0-3);
		
		v1=v1.print();
		v1=v1.scal(2);
		v3=v1.sum(v2);
		v3=v3.print();
		v1=v1.dot(v1);
		v1=v1.print();
		
		return true;
	}
	
	def print(): Vector =
	{
		var i:Int;
		
		println("(");
		i=0;
		while(i<3)
		{
			println("  "+data[i]);
			i=i+1;
		}
		println(")");
		
		return this;
	}
}