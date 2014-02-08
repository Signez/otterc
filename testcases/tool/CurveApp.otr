/* CurveApp is written in Toy Object-Oriented Language.
 * It's main feature is to print two kinds of curves.
 * Those are a straight line or a bezier curve.
 * 
 * Author: Michael Schneeberger
 * Date  : 02.10.2012
 */

//Main
object CurveApp {
    def main() : Unit = {
	println(new BuildCurve().go());
    }
}

//set up a curve
class BuildCurve {
    def go() : String = {
	var curve : Curve;
	var scene : Scene;
	var nothing : Int;
	
	//initializing curve
	curve = new Curve();
	nothing = curve.init(200);

	//initializing field
	scene = new Scene();
	nothing = scene.init(130, 38);

	//create curve
	nothing = curve.setStartPoint(10, 3);
	nothing = curve.setEndPoint(128, 6);
	//nothing = curve.createLine();
	nothing = curve.createBezier(100, 48, 130, 40);

	//print curve to field
	nothing = scene.insertCurve(curve);
	nothing = scene.printField();

        return "";       
    }
}

//the curve in R^2 space
class Curve {
    var len : Int;
    var posX : Int[];
    var posY : Int[];
    var startX : Int;
    var startY : Int;
    var endX : Int;
    var endY : Int;

    def init(iter : Int) : Int = {
	len = iter;
	posX = new Int[iter];
	posY = new Int[iter];

	return 0;
    }

    def len() : Int = {
	return len;
    }

    def setStartPoint(posX : Int, posY : Int) : Int = {
	startX = posX;
        startY = posY;
	return 0;
    }

    def setEndPoint(posX : Int, posY : Int) : Int = {
	endX = posX;
	endY = posY;
	return 0;
    }

    def getPosX(index : Int) : Int = {
	return posX[index];
    }

    def getPosY(index : Int) : Int = {
	return posY[index];
    }

    //creates a straight line
    def createLine() : Int = {
	var i : Int;

	i = 0;
	while (i < len) {
	    posX[i] = (startX-1)+i*(endX-startX)/(len-1);
	    posY[i] = (startY-1)+i*(endY-startY)/(len-1);
	    i = i+1;
	}
	return 0;
    }

    //creates a bezier curve
    def createBezier(p1X : Int, p1Y : Int, p2X : Int, p2Y : Int) : Int = {
	var i : Int;

	i = 0;
	while (i < len) {
	    posX[i] = ((len-i)*(len-i)*(len-i)*(startX-1)+3*(len-i)*(len-i)*i*(p1X-1)+3*(len-i)*i*i*(p2X-1)+i*i*i*(endX-1))/(len*len*len);
	    posY[i] = ((len-i)*(len-i)*(len-i)*(startY-1)+3*(len-i)*(len-i)*i*(p1Y-1)+3*(len-i)*i*i*(p2Y-1)+i*i*i*(endY-1))/(len*len*len);
	    i = i+1;
	}
	return 0;
    }
}

//scene displays curve in terminal
class Scene {
    var lenX : Int;
    var lenY : Int;
    var scene : Int[];

    def init(lengthX : Int, lengthY : Int) : Int = {
	lenX = lengthX;
	lenY = lengthY;
	scene = new Int[lenX*lenY];

	return 0;
    }

    //takes a curve and saves it in the variable "scene"
    def insertCurve(curve : Curve) : Int = {
	var i : Int;

	i = 0;
	while(i < curve.len()) {
	    scene[curve.getPosX(i) + lenX*curve.getPosY(i)] = 1;
	    i = i+1;
	}

	return 0;
    }

    def printField() : Int = {
	var str : String;
	var cntX : Int;
	var cntY : Int;

	cntY = 0;
	while (cntY < lenY) {
	    str = "";
	    cntX = 0;
	    while (cntX < lenX) {
		if (scene[cntX + lenX*cntY] == 1)
		    str = str + "*"; 		
		else
		    str = str + " ";
		cntX = cntX + 1;
	    }
	    println(str);
	    cntY = cntY + 1;
	}

	return 0;
    }
}
