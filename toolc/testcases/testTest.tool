object testTest {
    def main() : Unit = {
		println(new doSomething().go());
    }
}

class doSomething {
	def go() : String = {
		println("mamma");
		return "";
	}
}