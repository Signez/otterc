object Inheritance
{
	def main() : Unit = 
	{
		println(new Run().run());
	}
}

class Run
{
	def run(): Bool = 
	{
	var pers: Person;
	var work: Worker;
	var temp: Temporary;
	var downcast: Person;
	
	var upcast: Temporary;
	//upcast= new Worker(); //should not work
	
	pers = new Person();
	work = new Worker();
	temp = new Temporary();
	
	downcast=new Worker();

	println(work.setSalary(10));
	println(temp.setSalary(10));
	
	println(pers.Work());
	println(work.Work());
	println(temp.Work());
	
	println(pers.setName("person"));
	println(work.setName("worker"));
	println(temp.setName("temp"));
	
	return true;
	}
}
class Person
{
	var name: String;
	
	def Work(): Int = 
	{
		return 0;
	}
	
	def Name(): String =
	{
		return name;
	}
	
	def setName(s:String): String =
	{
		name=s;
		return name;
	}
}

class Worker extends Person
{
	var salary: Int;
	
	def Work(): Int = 
	{
		return salary;
	}
	
	def setSalary(s:Int): Bool =
	{
		salary=s;
		return true;
	}
}

class Temporary extends Worker
{

	def Work(): Int = 
	{
		var res: Int;
		res = salary / 2;
		return res;
	}
}
