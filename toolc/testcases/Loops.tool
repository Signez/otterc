object Loops
{
	def main() : Unit = {
		println(new Stairs().run(30));
	}
}

class Stairs
{
	
	def run(h:Int) : String = 
	{
		var res: String;
		var step: String;
		var i: Int;
		var j: Int;
		
		if(!(h<0))
		{
			j=1;
			step = "";
			
			while(j<h)
			{
				i=h;
				step = "";
				i=0;
				while(i<j)
				{
					step=step+"#";
					i=i+1;
				}
				//shape = shape + "\n" + step;
				j = j+1;
				println(step);
			}
			res = "Success";
		}
		else
		{
			res = "Negative Height";
		}
		
		return res;
	}
}