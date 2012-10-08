object Vehicules {
    def main() : Unit = {
        /*create a new car and open it*/
        println(new Car().init(4, 5).open());
    }
}

class Vehicule {
    var wheelNumber : Int;
    var doorNumber : Int;
    var doorState : Bool;

    def init(wheelNumberIn: Int, doorNumberIn: Int) : Vehicule = {
        wheelNumber = wheelNumberIn;
        doorNumber = doorNumberIn;
        doorState = true;
        return this;
    }

    def open() : Bool = {
        doorState = true;
        return doorState;
    }

    def close() : Bool = {
        doorState = false;
        return doorState;
    }
    
    def isOpen() : Bool = {
        return doorState;
    }
}

class Car extends Vehicule {

    def init(wheelNumberIn: Int, doorNumberIn: Int) : Vehicule = {
        wheelNumber = 4;
        doorNumber = doorNumberIn;
        return this;
    }
}
