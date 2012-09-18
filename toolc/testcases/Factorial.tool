object Factorial {
    def main() : Unit = {
        println(new Fact().computeFactorial(10));        
    }
}

class Fact {
    def computeFactorial(num : Int) : Int = {
        var num_aux : Int;
        if (num < 1)
            num_aux = 1;
        else
            num_aux = num * (this.computeFactorial(num - 1));
        return num_aux;
    }
}
