object BinarySearch {
    def main(): Unit = {
        println(new BS().Start(20));
    }
}

// This class contains an array of integers and
// methods to initialize, print and search the array
// using Binary Search
class BS {
    var number : Int[];
    var size : Int;

    // Invoke methods to initialize, print and search
    // for elements on the array
    def Start(sz : Int) : Int = {
        var aux01 : Int;
        var aux02 : Int;

        aux01 = this.Init(sz);
        aux02 = this.Print();

        if (this.Search(8))
            println(1);
        else
            println(0);
        if (this.Search(19))
            println(1);
        else
            println(0);
        if (this.Search(20))
            println(1);
        else
            println(0);
        if (this.Search(21))
            println(1);
        else
            println(0);
        if (this.Search(37))
            println(1);
        else
            println(0);
        if (this.Search(38))
            println(1);
        else
            println(0);
        if (this.Search(39))
            println(1);
        else
            println(0);
        if (this.Search(50))
            println(1);
        else
            println(0);

        return 999;
    }


    // Search for a specific value (num) using
    // binary search
    def Search(num : Int) : Bool = {
        var bs01 : Bool;
        var right : Int;
        var left : Int;
        var var_cont : Bool;
        var medium : Int;
        var aux01 : Int;
        var nt : Int;

        aux01 = 0;
        bs01 = false;
        right = number.length;
        right = right - 1;
        left = 0;
        var_cont = true;
        while (var_cont) {
            medium = left + right;
            medium = medium / 2;
            aux01 = number[medium];
            if (num < aux01)
                right = medium - 1;
            else
                left = medium + 1;
            if (aux01 == num)
                var_cont = false;
            else
                var_cont = true;
            if (right < left)
                var_cont = false;
            else
                nt = 0;
        }

        if (aux01 == num)
            bs01 = true;
        else
            bs01 = false;
        return bs01;
    }

    // Print the integer array
    def Print() : Int = {
        var j : Int;

        j = 1 ;
        while (j < (size)) {
            println(number[j]);
            j = j + 1;
        }
        println(99999);
        return 0 ;
    }


    // Initialize the integer array
    def Init(sz : Int) : Int = {
        var j : Int;
        var k : Int;
        var aux01 : Int;
        var aux02 : Int;

        size = sz;
        number = new Int[sz] ;

        j = 1 ;
        k = size + 1 ;
        while (j < (size)) {
            aux01 = 2 * j ;
            aux02 = k - 3 ;
            number[j] = aux01 + aux02 ;
            j = j + 1 ;
            k = k - 1 ;
        }
        return 0 ;  
    }
}
