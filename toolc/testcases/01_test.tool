object StartIt {
    def main() : Unit = {
		if(new UnitTests().testWithSimpleCall()) {
			println("Test succeded.");
		} else {
			println("Test failed.");		
		}
    }
}

class UnitTests {
	def testWithSimpleCall() : Bool = {
		var i: Int;
		var func: Int => Bool;
		i = 1;
		func = (test : Int) => { return test == 1; };
		return func(i); // should return true
	}
	
	def testWithImmediateInlineCall() : Bool = {
		var i: Int;
		i = 2;
		return ((test : Int) => { return test == 2; })(i); // should return true
	}
	
	def testWithContext() : Bool = {
		var context: Int;
		var func: Int => Bool;
        var i: Int;
		i = 1;
		context = 2;
		func = (test : Int) => { return test + 1 == context; };
		return func(i); // should return true
	}
	
	//def testWithUnit() : Bool = {
	//	var i: Int;
	//	var j: Int;
	//	var func : Unit => Unit;
	//	i = 0;
	//	j = 0;
	//	func = () => { i = i + 1; };
	//	while(j < 3) {
	//		func();
	//		j++;
	//	}
	//	return (i == j); // should return true
	//}
}
