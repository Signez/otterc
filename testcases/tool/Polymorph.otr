// Test overriding methods

object Polymorph {
    def main() : Unit = {
        if (new Application().run()) { /* println(new Nothing().why()); */ }
    }
}

class Nothing {
    def why() : String = {
        return "Because Unit is not a Type!";
    }
}

class Application {
    def run() : Bool = {
        var animal : Animal;
        var nothing : Nothing;
        
        animal = new Dog();
        nothing = animal.pet();
        
        animal = new Cat();
        nothing = animal.pet();
        
        return true;
    }
}

class Animal {
    def pet() : Nothing = {
        println("Who am I ? What am I ? Where am I ?");
        return new Nothing();
    }
}

class Dog extends Animal {
    def pet() : Nothing = {
        println("Whouf!");
        return new Nothing();
    }
}

class Cat extends Animal {
    def pet() : Nothing = {
        println("zzZZZZzzz");
        return new Nothing();
    }
}

